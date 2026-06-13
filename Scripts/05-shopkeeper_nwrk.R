# =============================================================================
# Script: 05-shopkeeper_nwrk.R
# Project: Shopkeepers Network Analysis
# Description:
#   Builds a two-mode (bipartite) affiliation network from NER-tagged shopkeeper
#   data extracted via the histtext package. Nodes are PERSON and ORG entities;
#   edges connect persons to organisations that co-occur in the same document.
#
#   The script proceeds in five phases:
#     A. Data preparation   — filter and clean raw NER output
#     B. Deduplication      — resolve entities assigned to more than one type
#     C. Homogenisation     — standardise variant names via lookup dictionaries
#     D. Network analysis   — build igraph objects, compute centrality metrics,
#                             detect communities, analyse edge weights
#     E. Export             — write node/edge lists for Cytoscape (full + filtered,
#                             whole corpus + per-period)
#
#   Several steps require externally curated correction files that are loaded
#   from disk. The expected file names are noted at each such point.
#
# Temporal periods used throughout:
#   Period 1: up to 1884
#   Period 2: 1885–1902
#   Period 3: 1903–1916
#   Period 4: 1917–1932
#   Period 5: 1933–end
# =============================================================================


# --- LIBRARIES ----------------------------------------------------------------

library(histtext)    # corpus access and padagraph export (in_padagraph, get_padagraph_url)
library(lubridate)   # date helpers
library(ggplot2)     # plotting
library(tidygraph)   # tidy interface for graph objects
library(igraph)      # core graph construction and metrics
library(tidyverse)   # dplyr, tidyr, stringr, readr, purrr, ggplot2
library(tidytext)    # text-mining utilities

list_corpora()  # verify connection to histtext corpus server


# =============================================================================
# PHASE A: DATA PREPARATION
# =============================================================================

# --- A.1 Save / reload session -----------------------------------------------

# Use save.image() frequently to checkpoint progress; reload with load() to
# resume without re-running expensive upstream steps.
save.image("shopnwrk.RData")
load("shopnwrk.RData")   # <-- run this line to resume a saved session


# --- A.2 Remove uninformative NER types --------------------------------------

# shops_ner is the raw NER output (~1 million rows) produced by an upstream script.
# CARDINAL, DATE, QUANTITY, MONEY, ORDINAL and TIME entities are not useful for
# the person–organisation network and are removed here.
shops_nwrk <- shops_ner %>%
  filter(!str_detect(Type, "CARDINAL|DATE|QUANTITY|MONEY|ORDINAL|TIME"))


# --- A.3 Remove non-Chinese text and noise -----------------------------------

# Keep only rows whose Text field contains at least one CJK character.
# This discards romanised noise produced by the NER model.
shops_nwrk <- shops_nwrk %>%
  filter(stringr::str_detect(Text, "[\\p{Han}]"))

# Remove a small set of known false-positive strings identified during manual review.
shops_nwrk <- shops_nwrk %>%
  filter(!stringr::str_detect(Text, "·養正·且華·紫金·靜文"))

# Strip typographic noise characters (circled numbers, special symbols, punctuation,
# repeated placeholder characters) that the OCR or NER pipeline sometimes outputs.
shops_nwrk <- shops_nwrk %>%
  mutate(Text = str_remove_all(Text,
    "㈠|㈢|㈤|㈥|㈩㈠|㈣|○　|○|r|'|…　　|…　|…|·|。|㈡|㈦|
㈨|\"|	㈧　|㈧　|×××|××|×|□□|□|〇　　|〇〇〇|〇〇|〇|▲|★|００００|０|ＸＸＸＸＸ　　|ＸＸ|Ｘ|一一|
、　　|、|!|？|㈨|㈧|㈩㈨|㈩"))

# Drop one-character strings, which are too short to be meaningful names.
shops_nwrk <- shops_nwrk %>%
  filter(nchar(Text) > 1)

shops_nwrk <- shops_nwrk %>% arrange(Text)
write_csv(shops_nwrk, "shops_nwrk.csv")


# =============================================================================
# PHASE B: BUILD CLEAN PERSON / ORG TABLE (clean_sb)
# =============================================================================

# --- B.1 Keep only PERSON and ORG rows ---------------------------------------

filtered_sb <- shops_nwrk %>%
  filter(Type %in% c("PERSON", "ORG"))


# --- B.2 Initial length filter and white-space splitting ---------------------

# Recalculate string length and drop single-character strings that survived
# the earlier filter (e.g. after noise stripping).
clean_sb <- filtered_sb %>%
  mutate(Length = nchar(Text)) %>%
  filter(Length > 1)

write_csv(clean_sb, "clean_sb.csv")

# The NER tokeniser sometimes outputs multiple names concatenated with spaces
# (e.g. "張三 李四"). Split each row into one name per row.
clean_sb <- clean_sb %>%
  separate_rows(Text, sep = "\\s+") %>%
  filter(Text != "")

# Refresh the length column after splitting, and drop any new single-character entries.
clean_sb <- clean_sb %>%
  select(-Length) %>%
  mutate(Length = nchar(Text)) %>%
  filter(Length > 1)
# Expected size: ~616,887 rows


# --- B.3 Remove honorific suffixes 君 and 氏 ---------------------------------

# The NER model frequently attaches the honorific 君 or 氏 to personal names.
# A simple rule safely removes them when they appear at the very end of a string.
clean_sb <- clean_sb %>%
  mutate(Text = sub("君$", "", Text)) %>%
  mutate(Text = sub("氏$", "", Text))

# 君 sometimes appears at position 2 (e.g. "王君三"), which cannot be handled
# by a single blanket rule because 君 is occasionally a legitimate part of a name.
# Export those rows for manual inspection, then remove them from the main table
# and re-add the manually corrected version.
clean_sb_jun <- clean_sb %>%
  filter(str_sub(Text, 2, 2) == "君")

write_csv(clean_sb_jun, "clean_sb_jun.csv")
# ** MANUAL STEP: open clean_sb_jun.csv, correct entries, save as clean_sb_jun_cur.csv,
#    then load it back:
#    clean_sb_jun_cur <- read_csv("clean_sb_jun_cur.csv")

clean_sb <- clean_sb %>%
  filter(!str_sub(Text, 2, 2) == "君")   # remove the ambiguous rows from main table

# Re-integrate curated rows.
# clean_sb_jun_cur must be loaded before running this line.
clean_sb <- bind_rows(clean_sb, clean_sb_jun_cur) %>%
  filter(!is.na(Text))

write_csv(clean_sb, "clean_sb.csv")

# Refresh length column once more after merging.
clean_sb <- clean_sb %>%
  select(-Length) %>%
  mutate(Length = nchar(Text)) %>%
  filter(Length > 1)
# Expected size: ~606,816 rows

write_csv(clean_sb, "clean_sb.csv")
save.image("shopnwrk.RData")


# --- B.4 Resolve duplicate vertex names (entities with conflicting types) ----

# In a bipartite network, every node must belong to exactly one mode (PERSON or ORG).
# If the same Text string appears under both types, it would create a "duplicate
# vertex name" error in igraph. Detect and fix this before building the graph.

# Identify Text values that have been tagged as both PERSON and ORG.
clean_sb_issues <- clean_sb %>%
  group_by(Text) %>%
  summarise(Types = list(unique(Type)), .groups = "drop") %>%
  filter(map_int(Types, length) > 1)

# Retrieve the original rows for inconsistent names so they can be corrected.
clean_sb_inconsistent <- clean_sb %>%
  filter(Text %in% clean_sb_issues$Text)

# Temporarily remove all inconsistent rows from the main table.
clean_sb_temp <- anti_join(clean_sb, clean_sb_inconsistent, by = c("Text", "Type"))

# Export for manual correction.
write_csv(clean_sb_inconsistent, "clean_sb_inconsistent.csv")
# ** MANUAL STEP: decide the correct Type for each ambiguous entity.
#    Save corrections as clean_sb_inconsCorr.csv with columns: Text, Type
#    then load:  clean_sb_inconsCorr <- read_csv("clean_sb_inconsCorr.csv")

# Apply corrections: replace original Type with the manually chosen Type.
clean_sb_incon <- clean_sb_inconsistent %>%
  left_join(clean_sb_inconsCorr, by = "Text") %>%
  select(-Type.x) %>%
  rename(Type = Type.y) %>%
  relocate(Type, .before = Text) %>%
  mutate(Year = substr(DocId, 5, 8)) %>%
  filter(!Type == "Remove") %>%   # drop rows marked for removal
  distinct()

# Reconstitute the full clean table.
clean_sb5 <- bind_rows(clean_sb_temp, clean_sb_incon) %>%
  select(-matches("^DocId\\.[xy]$"), -matches("^Year\\.[xy]$"))  # drop join duplicates

# Ensure a fresh Year column derived from DocId (format: XXXX-YYYY-...).
clean_sb5 <- clean_sb5 %>%
  mutate(Year = substr(DocId, 5, 8))

write_csv(clean_sb5, "clean_sb5.csv")


# =============================================================================
# PHASE C: HOMOGENISE ENTITY NAMES
# =============================================================================

# Named entities often appear under variant spellings across documents.
# Lookup dictionaries (OrgCorr and PerCorr) map variants to standard forms.
# These dictionaries must be prepared externally and loaded before this section.
# Expected columns: Text (variant), Text_Norm (standard form).

# ** MANUAL STEP: load dictionaries before running this section:
#    OrgCorr <- read_csv("OrgCorr.csv")   # columns: Text, Text_Norm
#    PerCorr <- read_csv("PerCorr.csv")   # columns: Text, Text_Norm

# Standardise ORG names.
clean_sb5_org <- clean_sb5 %>%
  filter(Type == "ORG") %>%
  distinct() %>%
  left_join(OrgCorr, by = "Text")   # adds Text_Norm column

write_csv(clean_sb5_org, "clean_sb5_org.csv")
# Expected size: ~180,462 rows

# Standardise PERSON names.
clean_sb5_per <- clean_sb5 %>%
  filter(Type == "PERSON") %>%
  distinct() %>%
  left_join(PerCorr, by = "Text")   # adds Text_Norm column

write_csv(clean_sb5_per, "clean_sb5_per.csv")
# Expected size: ~310,268 rows

# Reconstitute the combined person–organisation table.
clean_sb5 <- bind_rows(clean_sb5_per, clean_sb5_org)
# Expected size: ~440,244 rows


# =============================================================================
# PHASE D: CLEAN PERSONS AND ORGANISATIONS INDEPENDENTLY
# =============================================================================

# --- D.1 Persons cleaning ----------------------------------------------------

# Split the combined table into separate person and org tables for targeted cleaning.
# Note: at this stage the working dataset is renamed clean_sb6 to reflect additional
# curation steps carried out in companion scripts (Shops_Names_Check, Shops_Org_Check,
# 18-Shops_Typo). Load the results of those scripts before proceeding.

# ** EXTERNAL STEP: run Shops_Names_Check script, then load:
#    person_sb_corr3 <- read_csv("person_sb_corr3.csv")

persons_sb <- clean_sb6 %>% filter(Type == "PERSON")
orgs_sb    <- clean_sb6 %>% filter(Type == "ORG")

# Remove formulaic closing phrases that the NER model incorrectly tags as names.
suffixes <- c("謹啓", "君云", "君•", "啓", "啟", "投")
pattern  <- paste0("(", paste(suffixes, collapse = "|"), ")$")

person_sb_corr$Text <- gsub(pattern, "", person_sb_corr$Text)
person_sb_corr$Text <- trimws(person_sb_corr$Text)

# Drop rows that were flagged as false positives ("Removed") during manual review.
person_sb_corr2 <- person_sb_corr %>%
  filter(!str_detect(Text, regex("Removed", ignore_case = TRUE)))

write_csv(person_sb_corr2, "person_sb_corr2.csv")

# ** EXTERNAL STEP: further manual curation produces person_sb_corr3.
#    Load it and use as the canonical persons table.
persons_sb <- person_sb_corr3


# --- D.2 Organisations cleaning ----------------------------------------------

# ** EXTERNAL STEP: run Shops_Org_Check and 18-Shops_Typo scripts, then load:
#    orgs_sb_typo3 <- read_csv("orgs_sb_typo3.csv")

orgs_sb <- orgs_sb_typo3 %>%
  select(-type_code, -type_label) %>%
  rename(Text_srce = Text,
         Text      = Institution_Norm) %>%
  filter(!str_detect(Text, "Remove")) %>%
  select(-Text_srce, -Count) %>%
  relocate(Text, .after = Type)

# Merge variant spellings of the English Mixed Court into a single standard name.
mixed_court_variants <- c(
  "上海公共租界會審公廨",
  "上海公共會審公廨",
  "上海會審公廨",
  "公共會審公廨"
)

orgs_sb <- orgs_sb %>%
  mutate(Text = case_when(
    Text %in% mixed_court_variants ~ "英界會審公廨",
    TRUE ~ Text
  ))

write_csv(persons_sb, "person_sb.csv")
write_csv(orgs_sb,    "orgs_sb.csv")


# --- D.3 Combine and build edge / node lists ---------------------------------

# Combine cleaned persons and orgs into a single entity table.
clean_sb4 <- bind_rows(persons_sb, orgs_sb) %>% distinct()
write_csv(clean_sb4, "clean_sb4.csv")

# Build the edge list by joining persons to orgs on shared DocId.
# Each (person, org) pair that co-occurs in a document becomes an edge.
edge_sb <- persons_sb %>%
  inner_join(orgs_sb, by = "DocId", relationship = "many-to-many") %>%
  select(Source = Text.x, Target = Text.y, Year = Year.x) %>%
  distinct()

# Drop any targets that were flagged as inconsistent during the type-resolution step.
edge_sb <- edge_sb %>%
  filter(!Target %in% clean_sb_inconsCorr$Text)

# Build the node list: one row per unique entity, with aggregated metadata.
node_sb <- clean_sb4 %>%
  group_by(Text, Type) %>%
  summarise(
    Years    = paste(unique(Year), collapse = ", "),  # all years the entity appears
    DocCount = n_distinct(DocId),                      # number of documents
    .groups  = "drop"
  ) %>%
  rename(id = Text)

write_csv(edge_sb,  "edge_sb.csv")
write_csv(node_sb,  "node_sb.csv")


# =============================================================================
# PHASE E: NETWORK CONSTRUCTION
# =============================================================================

# --- E.1 Frequency filter (entities appearing > 4 times) --------------------

# Very rare entities add noise and make visualisations unreadable.
# Retain only entities that appear in more than 4 documents.

persons_sb_count <- persons_sb %>% group_by(Text) %>% count()
persons_sb_nbr   <- left_join(persons_sb, persons_sb_count, by = "Text")
persons_sb_filt  <- persons_sb_nbr %>% filter(n > 4)

orgs_sb_count  <- orgs_sb %>% group_by(Text) %>% count()
orgs_sb_nbr    <- left_join(orgs_sb, orgs_sb_count, by = "Text")
orgs_sb_filt   <- orgs_sb_nbr %>% filter(n > 4)

# Combined filtered dataset (used for period networks below).
clean_sb6_filt <- bind_rows(
  persons_sb_filt %>% select(-n),
  orgs_sb_filt    %>% select(-n)
)
write_csv(clean_sb6_filt, "clean_sb6_filt.csv")

# Filtered edge and node lists for the whole corpus.
edge_sb_filt <- persons_sb_filt %>%
  inner_join(orgs_sb_filt, by = "DocId", relationship = "many-to-many") %>%
  select(Source = Text.x, Target = Text.y, Year = Year.x) %>%
  distinct()

node_sb_filt <- clean_sb6_filt %>%
  group_by(Text, Type) %>%
  summarise(
    Years    = paste(unique(Year), collapse = ", "),
    DocCount = n_distinct(DocId),
    .groups  = "drop"
  ) %>%
  rename(id = Text)

write_csv(edge_sb_filt,  "edge_sb_filt.csv")
write_csv(node_sb_filt,  "node_sb_filt.csv")
write_csv(persons_sb_filt, "persons_sb_filt.csv")
write_csv(orgs_sb_filt,    "orgs_sb_filt.csv")


# --- E.2 Whole-corpus igraph object (for metric calculation only) ------------

# WARNING: the full network is very large and igraph visualisation is not
# recommended. Use this object to compute metrics, then export to Cytoscape
# for visual exploration.
ig <- graph_from_data_frame(d = edge_sb_filt, vertices = node_sb_filt, directed = FALSE)

# Uncomment to export for Cytoscape / Gephi:
# write_graph(ig, "shopsig.graphml", format = "graphml")


# --- E.3 Helper: create a period igraph object from filtered data ------------

create_period_network <- function(data, min_year, max_year, period_name) {
  # Filter to the requested time window.
  data_period <- data %>%
    filter(Year >= min_year & Year <= max_year)

  persons_period <- data_period %>% filter(Type == "PERSON")
  orgs_period    <- data_period %>% filter(Type == "ORG")

  # Edges: person–org co-occurrences within this period.
  edges_period <- persons_period %>%
    inner_join(orgs_period, by = "DocId", relationship = "many-to-many") %>%
    select(Source = Text.x, Target = Text.y) %>%
    distinct()

  # Nodes: all entities active in this period.
  nodes_period <- data_period %>%
    select(id = Text, Type) %>%
    distinct()

  write_csv(edges_period, paste0("edges_", period_name, ".csv"))
  write_csv(nodes_period, paste0("nodes_", period_name, ".csv"))

  ig_period <- graph_from_data_frame(
    d        = edges_period,
    vertices = nodes_period,
    directed = FALSE
  )

  list(edges = edges_period, nodes = nodes_period, graph = ig_period)
}


# --- E.4 Build period networks -----------------------------------------------

period1 <- create_period_network(clean_sb6_filt,    0, 1884, "1")
period2 <- create_period_network(clean_sb6_filt, 1885, 1902, "2")
period3 <- create_period_network(clean_sb6_filt, 1903, 1916, "3")
period4 <- create_period_network(clean_sb6_filt, 1917, 1932, "4")
period5 <- create_period_network(clean_sb6_filt, 1933,  Inf, "5")

# Convenience aliases for the five period graphs.
ig1 <- period1$graph
ig2 <- period2$graph
ig3 <- period3$graph
ig4 <- period4$graph
ig5 <- period5$graph


# --- E.5 Add visual attributes to period graphs (for igraph plots) -----------

# Assign shape and colour by node type so that persons and organisations are
# visually distinct in igraph plots and exported graphml files.
add_attributes_to_graph <- function(ig, node_df) {
  V(ig)$type  <- node_df$Type[match(V(ig)$name, node_df$id)]
  V(ig)[V(ig)$type == "PERSON"]$shape <- "circle"
  V(ig)[V(ig)$type == "ORG"]$shape    <- "square"
  V(ig)[V(ig)$type == "PERSON"]$color <- "red"
  V(ig)[V(ig)$type == "ORG"]$color    <- "lightblue"
  ig
}

ig1 <- add_attributes_to_graph(ig1, node_sb)
ig2 <- add_attributes_to_graph(ig2, node_sb)
ig3 <- add_attributes_to_graph(ig3, node_sb)
ig4 <- add_attributes_to_graph(ig4, node_sb)
ig5 <- add_attributes_to_graph(ig5, node_sb)


# --- E.6 Quick igraph plots (preliminary exploration only) -------------------

plot.igraph(ig1, vertex.size = 3, vertex.label.color = "black",
            vertex.label.cex = 0.3,
            main = "Shopkeepers Affiliation Network (1872–1884)")

plot.igraph(ig2, vertex.size = 3, vertex.label.color = "black",
            vertex.label.cex = 0.3,
            main = "Shopkeepers Affiliation Network (1885–1902)")

plot.igraph(ig3, vertex.size = 3, vertex.label.color = "black",
            vertex.label.cex = 0.3,
            main = "Shopkeepers Affiliation Network (1903–1916)")

plot.igraph(ig4, vertex.size = 3, vertex.label.color = "black",
            vertex.label.cex = 0.3,
            main = "Shopkeepers Affiliation Network (1917–1932)")

plot.igraph(ig5, vertex.size = 3, vertex.label.color = "black",
            vertex.label.cex = 0.3,
            main = "Shopkeepers Affiliation Network (1933–1949)")


# --- E.7 Export to Padagraph (interactive online visualisation) --------------

# Convert each igraph to a tidygraph object and push to Padagraph.
# Each network gets a unique project name so it can be retrieved via URL.

tg1 <- as_tbl_graph(ig1) %>% activate(nodes) %>% mutate(label = name)
tg1 %>% histtext::in_padagraph("ShopsNetwork1")
tg1 %>% histtext::get_padagraph_url("ShopsNetwork1")

tg2 <- as_tbl_graph(ig2) %>% activate(nodes) %>% mutate(label = name)
tg2 %>% histtext::in_padagraph("ShopsNetwork2")
tg2 %>% histtext::get_padagraph_url("ShopsNetwork2")

tg3 <- as_tbl_graph(ig3) %>% activate(nodes) %>% mutate(label = name)
tg3 %>% histtext::in_padagraph("ShopsNetwork3")
tg3 %>% histtext::get_padagraph_url("ShopsNetwork3")

tg4 <- as_tbl_graph(ig4) %>% activate(nodes) %>% mutate(label = name)
tg4 %>% histtext::in_padagraph("ShopsNetwork4")
tg4 %>% histtext::get_padagraph_url("ShopsNetwork4")

tg5 <- as_tbl_graph(ig5) %>% activate(nodes) %>% mutate(label = name)
tg5 %>% histtext::in_padagraph("ShopsNetwork5")
tg5 %>% histtext::get_padagraph_url("ShopsNetwork5")


# =============================================================================
# PHASE F: LOCAL CENTRALITY METRICS
# =============================================================================

# For further analysis applied to a single period network, see 5-shopkeeper_nwrk1.R.

# --- F.1 Unweighted centrality metrics for each period network ---------------

# A helper that computes the five standard metrics and attaches node attributes.
compute_centralities <- function(g, suffix, node_df) {
  deg      <- degree(g)
  deg_norm <- degree(g, normalized = TRUE)
  eig      <- eigen_centrality(g)$vector
  betw     <- betweenness(g)
  clos     <- closeness(g)

  df <- cbind(deg, deg_norm, eig, betw, clos) %>%
    as.data.frame() %>%
    setNames(paste0(c("Degree", "Degree_norm", "Eig", "Betw", "Close"), suffix)) %>%
    tibble::rownames_to_column("id")

  inner_join(node_df, df, by = "id")
}

centralities1_attributes <- compute_centralities(ig1, "1", node_sb)
centralities2_attributes <- compute_centralities(ig2, "2", node_sb)
centralities3_attributes <- compute_centralities(ig3, "3", node_sb)
centralities4_attributes <- compute_centralities(ig4, "4", node_sb)
centralities5_attributes <- compute_centralities(ig5, "5", node_sb)

write_csv(centralities1_attributes, "centralities1_attributes.csv")
write_csv(centralities2_attributes, "centralities2_attributes.csv")
write_csv(centralities3_attributes, "centralities3_attributes.csv")
write_csv(centralities4_attributes, "centralities4_attributes.csv")
write_csv(centralities5_attributes, "centralities5_attributes.csv")

save.image("shopnwrk.RData")


# --- F.2 Weighted centrality metrics (whole-corpus network) ------------------

# Edge weights represent co-occurrence frequency (how many documents share
# a given person–organisation pair). Higher weight = stronger association.

# Strength: weighted degree (sum of edge weights rather than count of edges).
V(ig)$strength <- strength(ig, weights = E(ig)$weight)

# Weighted betweenness: igraph treats weights as distances, so we invert them
# so that high-weight (frequent) ties are treated as short paths.
V(ig)$betweenness_w <- betweenness(ig,
  weights    = 1 / E(ig)$weight,
  normalized = FALSE)

# Weighted closeness (less central for bipartite networks but available).
V(ig)$closeness_w <- closeness(ig, weights = 1 / E(ig)$weight)


# --- F.3 Normalised centrality -----------------------------------------------

# Degree normalisation: divide by (N − 1), the theoretical maximum.
n <- vcount(ig)
V(ig)$degree_norm <- degree(ig) / (n - 1)

# Min-max normalise strength within the full network.
V(ig)$strength_norm <- (V(ig)$strength - min(V(ig)$strength)) /
                       (max(V(ig)$strength) - min(V(ig)$strength))

# igraph's built-in normalised betweenness (divides by (n-1)(n-2)/2 for undirected).
V(ig)$betweenness_norm <- betweenness(ig,
  weights    = 1 / E(ig)$weight,
  normalized = TRUE)


# --- F.4 Within-mode normalisation (bipartite-aware) -------------------------

# In a bipartite network, normalising across both modes conflates very different
# degree distributions. Normalise separately within PERSON and ORG.
g_tidy <- as_tbl_graph(ig)

g_tidy <- g_tidy %>%
  activate(nodes) %>%
  mutate(
    degree_raw      = centrality_degree(),
    strength_raw    = centrality_degree(weights = weight),
    betweenness_raw = centrality_betweenness(weights = 1 / weight, normalized = FALSE),

    # Normalise within each mode independently.
    degree_norm_bytype = ave(degree_raw, type,
      FUN = function(x) (x - min(x)) / (max(x) - min(x))),
    betweenness_norm_bytype = ave(betweenness_raw, type,
      FUN = function(x) (x - min(x)) / (max(x) - min(x)))
  )


# --- F.5 Cross-period comparison table ---------------------------------------

# Build a single tidy data frame with normalised metrics for all five periods,
# enabling direct comparison of actors across time.
periods       <- list(ig1, ig2, ig3, ig4, ig5)
period_labels <- paste0("P", 1:5)

results <- purrr::map2_dfr(periods, period_labels, function(g, period) {
  n_nodes <- vcount(g)
  tibble(
    period          = period,
    name            = V(g)$name,
    type            = V(g)$type,
    n_nodes         = n_nodes,
    degree_raw      = degree(g),
    degree_norm     = degree(g) / (n_nodes - 1),
    strength        = strength(g, weights = E(g)$weight),
    betweenness_norm = betweenness(g,
      weights    = 1 / E(g)$weight,
      normalized = TRUE)
  )
})

# Example: top 10 organisations by normalised betweenness in each period.
results %>%
  filter(type == "ORG") %>%
  group_by(period) %>%
  slice_max(betweenness_norm, n = 10)


# =============================================================================
# PHASE G: ADVANCED STRUCTURAL ANALYSIS
# =============================================================================

# --- G.1 Dyadic analysis — "Who appears most often with whom?" ---------------

# Extract the edge list from the igraph object and join node metadata so that
# each edge row shows the names and types of both endpoints.
dyads <- as_tbl_graph(ig) %>%
  activate(edges) %>%
  as_tibble() %>%
  left_join(
    as_tibble(activate(as_tbl_graph(ig), nodes)) %>%
      mutate(id = row_number()) %>% select(id, name_from = name, type_from = type),
    by = c("from" = "id")
  ) %>%
  left_join(
    as_tibble(activate(as_tbl_graph(ig), nodes)) %>%
      mutate(id = row_number()) %>% select(id, name_to = name, type_to = type),
    by = c("to" = "id")
  ) %>%
  arrange(desc(weight))

# Top 20 person–organisation pairs by co-occurrence frequency.
top_dyads <- dyads %>%
  filter(type_from == "PERSON", type_to == "ORG") %>%
  slice_max(weight, n = 20)

# Exclusivity ratio: the share of a node's total activity concentrated in
# a single dyad. High exclusivity = the actor is strongly tied to one partner.
node_strength_vec <- strength(ig, weights = E(ig)$weight)

dyads <- dyads %>%
  mutate(
    strength_from    = node_strength_vec[from],
    strength_to      = node_strength_vec[to],
    exclusivity_from = weight / strength_from,  # person's activity share in this dyad
    exclusivity_to   = weight / strength_to     # org's activity share in this dyad
  )


# --- G.2 Community detection -------------------------------------------------

# Detect clusters of persons and organisations that frequently appear together.
# Two complementary approaches are shown.

# Approach A: Project onto organisations (which institutions share members?),
# then run Louvain community detection on the one-mode projection.
orgs_idx <- which(V(ig)$type == "ORG")   # adjust if type is stored as logical
proj     <- bipartite_projection(ig, which = "false")   # ORG projection
comm     <- cluster_louvain(proj, weights = E(proj)$weight)
V(proj)$community <- membership(comm)

# Approach B: Run label propagation directly on the bipartite graph
# (avoids projection artifacts but is less established methodologically).
comm_bip        <- cluster_label_prop(ig, weights = E(ig)$weight)
V(ig)$community <- membership(comm_bip)

# Tidygraph workflow for Louvain on the full bipartite graph.
g_tidy <- g_tidy %>%
  activate(nodes) %>%
  mutate(community = group_louvain(weights = weight))

# Summarise each community: size, type composition, and most central members.
as_tibble(activate(g_tidy, nodes)) %>%
  group_by(community, type) %>%
  summarise(
    n         = n(),
    top_nodes = paste(head(name[order(-degree_raw)], 5), collapse = ", "),
    .groups   = "drop"
  ) %>%
  arrange(community)


# --- G.3 Edge weight distribution — are tie strengths power-law distributed? -

# Plotting weight distributions on log-log axes is a standard check for
# scale-free structure in co-occurrence networks.
edge_weights <- bind_rows(
  tibble(period = "P1", weight = E(ig1)$weight),
  tibble(period = "P2", weight = E(ig2)$weight),
  tibble(period = "P3", weight = E(ig3)$weight),
  tibble(period = "P4", weight = E(ig4)$weight),
  tibble(period = "P5", weight = E(ig5)$weight)
)

ggplot(edge_weights, aes(x = weight)) +
  geom_histogram(bins = 50) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~period) +
  labs(
    title = "Edge weight distributions by period",
    x     = "Co-mention frequency (log)",
    y     = "Count (log)"
  )

# Summary statistics useful for comparing network density across periods.
edge_weights %>%
  group_by(period) %>%
  summarise(
    n_edges    = n(),
    mean_w     = mean(weight),
    median_w   = median(weight),
    max_w      = max(weight),
    pct_weak   = mean(weight == 1),   # proportion of one-off co-mentions
    pct_strong = mean(weight >= 10)   # proportion of recurrent ties
  )


# --- G.4 Relational profiles — combining centrality, exclusivity, community --

node_profiles <- as_tibble(activate(g_tidy, nodes)) %>%
  mutate(
    degree_norm      = degree_norm_bytype,
    betweenness_norm = betweenness_norm_bytype,

    # Maximum dyadic share: how much of a node's activity is in its single
    # strongest tie? High values flag structurally dependent actors.
    max_dyad_share = map_dbl(name, function(nm) {
      ego_edges <- incident(ig, nm, mode = "all")
      if (length(ego_edges) == 0) return(NA_real_)
      max(E(ig)[ego_edges]$weight) /
        strength(ig, vids = nm, weights = E(ig)$weight)
    }),

    community = community
  )

save.image("shopnwrk.RData")


# =============================================================================
# PHASE H: CYTOSCAPE FILE PREPARATION
# =============================================================================

# Cytoscape requires separate node and edge CSV files.
# Both include a Year column so time filters can be applied inside Cytoscape.
# The Year column is dropped from node files after splitting by period
# (each period file is effectively a static snapshot).

# --- H.1 Full (unfiltered) Cytoscape export ----------------------------------

shops_nodes_Cy <- bind_rows(persons_sb, orgs_sb) %>%
  mutate(Year = substr(DocId, 5, 8)) %>%
  select(Type, Text, Year) %>%
  distinct()

shops_edges_Cy <- inner_join(persons_sb, orgs_sb, by = "DocId") %>%
  select(-Year.y) %>%
  rename(Name        = Text.x,
         Institution = Text.y) %>%
  mutate(Year = substr(DocId, 5, 8)) %>%
  select(Name, Institution, Year) %>%
  distinct()

write_csv(shops_edges_Cy, "shops_edges_Cy.csv")
write_csv(shops_nodes_Cy, "shops_nodes_Cy.csv")

# Split into period node files (Year column removed; each file is a snapshot).
shops_nodes_Cy1 <- shops_nodes_Cy %>% filter(Year <  1885)              %>% select(-Year)
shops_nodes_Cy2 <- shops_nodes_Cy %>% filter(Year >= 1885, Year < 1903) %>% select(-Year)
shops_nodes_Cy3 <- shops_nodes_Cy %>% filter(Year >= 1903, Year < 1917) %>% select(-Year)
shops_nodes_Cy4 <- shops_nodes_Cy %>% filter(Year >= 1917, Year < 1933) %>% select(-Year)
shops_nodes_Cy5 <- shops_nodes_Cy %>% filter(Year >= 1933)              %>% select(-Year)

write_csv(shops_nodes_Cy1, "shops_nodes_Cy1.csv")
write_csv(shops_nodes_Cy2, "shops_nodes_Cy2.csv")
write_csv(shops_nodes_Cy3, "shops_nodes_Cy3.csv")
write_csv(shops_nodes_Cy4, "shops_nodes_Cy4.csv")
write_csv(shops_nodes_Cy5, "shops_nodes_Cy5.csv")

# Split into period edge files (Year column retained for temporal filtering).
shops_edges_Cy1 <- edge_sb %>% filter(Year <  1885)              %>% distinct()
shops_edges_Cy2 <- edge_sb %>% filter(Year >= 1885, Year < 1903) %>% distinct()
shops_edges_Cy3 <- edge_sb %>% filter(Year >= 1903, Year < 1917) %>% distinct()
shops_edges_Cy4 <- edge_sb %>% filter(Year >= 1917, Year < 1933) %>% distinct()
shops_edges_Cy5 <- edge_sb %>% filter(Year >= 1933)              %>% distinct()

write_csv(shops_edges_Cy1, "shops_edges_Cy1.csv")
write_csv(shops_edges_Cy2, "shops_edges_Cy2.csv")
write_csv(shops_edges_Cy3, "shops_edges_Cy3.csv")
write_csv(shops_edges_Cy4, "shops_edges_Cy4.csv")
write_csv(shops_edges_Cy5, "shops_edges_Cy5.csv")

save.image("shopnwrk.RData")


# --- H.2 Filtered Cytoscape export (entities appearing > 4 times) -----------

# Filtering to frequently occurring entities reduces visual clutter and focuses
# attention on the most structurally significant actors.

shops_nodes_CyFilt <- bind_rows(persons_sb_filt, orgs_sb_filt) %>%
  mutate(Year = substr(DocId, 5, 8)) %>%
  select(Type, Text, Year) %>%
  distinct()

# Note: edges use the filtered person and org tables so only well-attested
# co-occurrences appear.
shops_edges_CyFilt <- inner_join(persons_sb_filt, orgs_sb_filt, by = "DocId") %>%
  select(-Year.y) %>%
  rename(Name        = Text.x,
         Institution = Text.y) %>%
  mutate(Year = substr(DocId, 5, 8)) %>%
  select(Name, Institution, Year) %>%
  distinct()

write_csv(shops_edges_CyFilt, "shops_edges_CyFilt.csv")
write_csv(shops_nodes_CyFilt, "shops_nodes_CyFilt.csv")

# Period node files (filtered).
shops_nodes_CyFilt1 <- shops_nodes_CyFilt %>% filter(Year <  1885)              %>% select(-Year)
shops_nodes_CyFilt2 <- shops_nodes_CyFilt %>% filter(Year >= 1885, Year < 1903) %>% select(-Year)
shops_nodes_CyFilt3 <- shops_nodes_CyFilt %>% filter(Year >= 1903, Year < 1917) %>% select(-Year)
shops_nodes_CyFilt4 <- shops_nodes_CyFilt %>% filter(Year >= 1917, Year < 1933) %>% select(-Year)
shops_nodes_CyFilt5 <- shops_nodes_CyFilt %>% filter(Year >= 1933)              %>% select(-Year)

write_csv(shops_nodes_CyFilt1, "shops_nodes_CyFilt1.csv")
write_csv(shops_nodes_CyFilt2, "shops_nodes_CyFilt2.csv")
write_csv(shops_nodes_CyFilt3, "shops_nodes_CyFilt3.csv")
write_csv(shops_nodes_CyFilt4, "shops_nodes_CyFilt4.csv")
write_csv(shops_nodes_CyFilt5, "shops_nodes_CyFilt5.csv")

# Period edge files (filtered).
shops_edges_CyFilt1 <- edge_sb_filt %>% filter(Year <  1885)              %>% distinct()
shops_edges_CyFilt2 <- edge_sb_filt %>% filter(Year >= 1885, Year < 1903) %>% distinct()
shops_edges_CyFilt3 <- edge_sb_filt %>% filter(Year >= 1903, Year < 1917) %>% distinct()
shops_edges_CyFilt4 <- edge_sb_filt %>% filter(Year >= 1917, Year < 1933) %>% distinct()
shops_edges_CyFilt5 <- edge_sb_filt %>% filter(Year >= 1933)              %>% distinct()

write_csv(shops_edges_CyFilt1, "shops_edges_CyFilt1.csv")
write_csv(shops_edges_CyFilt2, "shops_edges_CyFilt2.csv")
write_csv(shops_edges_CyFilt3, "shops_edges_CyFilt3.csv")
write_csv(shops_edges_CyFilt4, "shops_edges_CyFilt4.csv")
write_csv(shops_edges_CyFilt5, "shops_edges_CyFilt5.csv")

save.image("shopnwrk.RData")

# =============================================================================
# END OF SCRIPT
# =============================================================================
