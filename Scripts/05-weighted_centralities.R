################################################################################
# WEIGHTED & NORMALIZED CENTRALITIES FOR THE SHOPKEEPER NETWORKS
# Companion to 05-shopkeeper_nwrk.R
#
# Purpose
#   Recompute the period networks with EDGE WEIGHTS (within-period recurrence of
#   each person-organization co-mention) and produce weighted + normalized
#   centrality measures, plus ranked top-N tables per period and per mode.
################################################################################

# save objects in .RData file
save.image('shpwghtnet.RData')

# Re-upload saved RData file
load(file = ("shpwghtnet.RData"))


library(data.table)
library(igraph)
library(tidyverse)

# ------------------------------------------------------------------------------
# 0. INPUT
# ------------------------------------------------------------------------------
# Start from clean_sb5 produced by 05-shopkeeper_nwrk.R (one row per
# Type / Text / DocId / Year, already de-duplicated and standardised).

if (!exists("clean_sb5")) {
  clean_sb5 <- data.table::fread("clean_sb5.csv", encoding = "UTF-8")
}
dt <- as.data.table(clean_sb5)[, .(Type, Text, DocId, Year)]
dt[, Year := as.integer(substr(Year, 1, 4))]
dt <- dt[!is.na(Year) & Type %in% c("PERSON", "ORG") & nchar(Text) > 1]
dt <- unique(dt)                       # safety: one entity per document

# Period assignment (identical boundaries to 05-...R)
dt[, Period := fcase(
  Year < 1885,                 1L,
  Year >= 1885 & Year < 1903,  2L,
  Year >= 1903 & Year < 1917,  3L,
  Year >= 1917 & Year < 1933,  4L,
  Year >= 1933,                5L
)]

# ------------------------------------------------------------------------------
# 1. TUNING KNOBS  
# ------------------------------------------------------------------------------
MAX_ENTITIES_PER_DOC <- 150   # skip articles with more than this many entities.
# These are almost always list/roster pages whose
# N^2 co-mentions are noise, not institutional
# encounters. Set to Inf to disable.
MIN_DOCS_PER_ENTITY  <- 1     # drop entities appearing in fewer than this many
# documents *within the period* before pairing.
# 1 = keep everything; 3-5 mirrors the essay's
# degree>4 filter and slashes the long tail.
TOP_N                <- 25    # rows per ranked table

# ------------------------------------------------------------------------------
# 2. WEIGHTED EDGE LIST FOR ONE PERIOD  (the memory-safe core)
# ------------------------------------------------------------------------------
build_weighted_edges <- function(d,
                                 max_entities = MAX_ENTITIES_PER_DOC,
                                 min_docs     = MIN_DOCS_PER_ENTITY) {
  d <- unique(d[, .(Type, Text, DocId)])
  
  # (a) drop globally rare entities within this period
  if (min_docs > 1) {
    freq <- d[, .(nd = uniqueN(DocId)), by = .(Type, Text)]
    keep_ent <- freq[nd >= min_docs, .(Type, Text)]
    d <- d[keep_ent, on = .(Type, Text)]
  }
  
  # (b) drop oversized documents
  if (is.finite(max_entities)) {
    docsz <- d[, .N, by = DocId]
    d <- d[DocId %in% docsz[N <= max_entities, DocId]]
  }
  
  P <- d[Type == "PERSON", .(DocId, Source = Text)]
  O <- d[Type == "ORG",    .(DocId, Target = Text)]
  if (nrow(P) == 0L || nrow(O) == 0L)
    return(data.table(Source = character(), Target = character(), weight = integer()))
  
  # cartesian join *within* document, aggregated straight to weights.
  # weight = number of distinct articles in which this person-org pair co-occurs.
  e <- merge(P, O, by = "DocId", allow.cartesian = TRUE)
  e[, .(weight = .N), by = .(Source, Target)]
}

# ------------------------------------------------------------------------------
# 3. CENTRALITIES FOR ONE WEIGHTED BIPARTITE GRAPH
# ------------------------------------------------------------------------------
# Notes on the bipartite issue:
#   * type attribute marks the two modes (FALSE = PERSON, TRUE = ORG);
#   * eigenvector / closeness / betweenness are computed on the weighted graph;
#   * igraph treats edge weights as DISTANCES for betweenness/closeness, so we
#     pass 1/weight (a stronger tie = a shorter path);
#   * normalized = TRUE gives size-independent values so periods of very
#     different size can be compared (directly addresses comment 2 & 3a);
#   * we additionally rescale strength WITHIN each mode (PERSON vs ORG), because
#     raw values are not comparable across the two node populations.
compute_centralities <- function(w, period, main_component = TRUE) {
  if (nrow(w) == 0L) return(NULL)
  
  g <- graph_from_data_frame(w, directed = FALSE)
  org_names      <- unique(w$Target)
  V(g)$type      <- V(g)$name %in% org_names      # TRUE = ORG, FALSE = PERSON
  E(g)$weight    <- w$weight
  
  if (main_component) {
    comp <- components(g)
    g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }
  
  res <- data.table(
    Period     = period,
    id         = V(g)$name,
    mode       = ifelse(V(g)$type, "ORG", "PERSON"),
    strength   = strength(g, weights = E(g)$weight),       # weighted degree
    degree     = degree(g),                                # raw degree
    degree_norm= degree(g, normalized = TRUE),             # /(n-1)
    eigen_w    = eigen_centrality(g, weights = E(g)$weight)$vector,
    betw_w     = betweenness(g, weights = 1 / E(g)$weight, normalized = TRUE),
    close_w    = closeness(g,   weights = 1 / E(g)$weight, normalized = TRUE)
  )
  
  # within-mode 0-1 rescaling of weighted degree for cross-mode comparability
  res[, strength_modenorm := strength / max(strength), by = mode]
  res[]
}

# ------------------------------------------------------------------------------
# 4. RUN ALL FIVE PERIODS
# ------------------------------------------------------------------------------
edges_by_period       <- vector("list", 5)
centralities_by_period<- vector("list", 5)

for (p in 1:5) {
  message(sprintf("Period %d ...", p))
  w <- build_weighted_edges(dt[Period == p])
  edges_by_period[[p]] <- w
  fwrite(w, sprintf("edges_weighted_p%d.csv", p))
  
  ce <- compute_centralities(w, period = p, main_component = TRUE)
  centralities_by_period[[p]] <- ce
  if (!is.null(ce)) fwrite(ce, sprintf("centralities_weighted_p%d.csv", p))
}

all_centralities <- rbindlist(centralities_by_period, use.names = TRUE)
fwrite(all_centralities, "centralities_weighted_all_periods.csv")

# ------------------------------------------------------------------------------
# 5. RANKED TOP-N TABLES  (what the reviewer asked to insert in the essay)
# ------------------------------------------------------------------------------
# Top organisations and top persons per period, by weighted degree (strength).
top_table <- function(ce, which_mode, n = TOP_N) {
  ce[mode == which_mode][order(-strength)][1:min(n, .N),
                                           .(Period, rank = seq_len(.N), id, mode,
                                             strength, strength_modenorm = round(strength_modenorm, 3),
                                             degree, degree_norm = round(degree_norm, 4),
                                             eigen_w = round(eigen_w, 3), betw_w = round(betw_w, 4))]
}

top_orgs    <- rbindlist(lapply(centralities_by_period,
                                function(x) if (!is.null(x)) top_table(x, "ORG")))
top_persons <- rbindlist(lapply(centralities_by_period,
                                function(x) if (!is.null(x)) top_table(x, "PERSON")))

fwrite(top_orgs,    "top_orgs_by_period.csv")
fwrite(top_persons, "top_persons_by_period.csv")

# OPTIONAL — strongest dyads ("who appears with whom most"): top weighted edges.
top_dyads <- rbindlist(lapply(1:5, function(p) {
  w <- edges_by_period[[p]]
  if (nrow(w) == 0L) return(NULL)
  head(w[order(-weight)], TOP_N)[, Period := p][]
}))
fwrite(top_dyads, "top_dyads_by_period.csv")


# =====================================================================
# VISUALIZATIONS
# =====================================================================
# top_orgs_by_period — visual overview of organizational centrality
# Three figures:
#   1. Bump chart  — ranking flow of top-12 orgs across the 5 periods
#   2. Heatmap     — strength (within-period normalized) by org x period
#   3. Small multiples — top-12 ranked bars per period
#
# Metric used throughout: strength_modenorm (within-period normalized
# strength, 0-1). Raw `strength` is NOT comparable across periods because
# it scales with corpus size (period 1 max = 310; period 4 max = 14,899).
# =====================================================================

# ---- packages -------------------------------------------------------
library(tidyverse)
library(showtext)   # renders Chinese (CJK) glyphs in plots
library(tidytext)

# Chinese font: use a CJK font installed on your system.
# macOS: "PingFang SC" or "STSong"; Windows: "Microsoft YaHei";
# Linux: install Noto Sans CJK then use "Noto Sans CJK SC".
font_add(family = "cjk", regular = {
  cand <- c("/System/Library/Fonts/PingFang.ttc",                       # macOS
            "C:/Windows/Fonts/msyh.ttc",                                # Windows
            "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc")   # Linux
  hit <- cand[file.exists(cand)]
  if (length(hit) == 0) stop("No CJK font found - edit the path in font_add().")
  hit[1]
})
showtext_auto()
showtext_opts(dpi = 300)   # match the 300-dpi export so CJK labels size correctly

# ---- load & clean ---------------------------------------------------
dftop <- read_csv("top_orgs_by_period.csv", show_col_types = FALSE) %>%
  # merge the variant-character duplicate (period 3); canonical form = 懐
  mutate(id = recode(id, "懷安施醫局" = "懐安施醫局")) %>%
  group_by(Period, id) %>%
  slice_max(strength, n = 1, with_ties = FALSE) %>%   # keep strongest if merged
  ungroup() %>%
  # recompute rank within period after the merge
  group_by(Period) %>%
  mutate(rank = rank(-strength, ties.method = "first")) %>%
  ungroup()

periods <- sort(unique(dftop$Period))

# Core organizations: present in >= MIN_PERIODS periods. With this dataset,
# >=3 yields 9 orgs (a clean, publication-ready set). Lower to 2 for more lines.
MIN_PERIODS <- 3
core <- dftop %>% count(id) %>% filter(n >= MIN_PERIODS) %>% pull(id)


# =====================================================================
# FIGURE 1 — BUMP CHART  focused on the core orgs
# =====================================================================
df_bump <- dftop %>% mutate(is_core = id %in% core)

label_pts <- df_bump %>%                       # label core orgs at entry & exit
  filter(is_core) %>%
  group_by(id) %>%
  filter(Period == min(Period) | Period == max(Period)) %>%
  mutate(side = ifelse(Period == min(Period), "first", "last")) %>%
  ungroup()

p_bump <- ggplot() +
  # faint grey context: every other top-25 entrant
  geom_line(data = filter(df_bump, !is_core),
            aes(Period, rank, group = id), color = "grey85", linewidth = .35) +
  geom_point(data = filter(df_bump, !is_core),
             aes(Period, rank), color = "grey85", size = 1) +
  # core orgs in color
  geom_line(data = filter(df_bump, is_core),
            aes(Period, rank, group = id, color = id), linewidth = 1.1) +
  geom_point(data = filter(df_bump, is_core),
             aes(Period, rank, color = id), size = 2.8) +
  geom_text(data = filter(label_pts, side == "first"),
            aes(Period, rank, label = id, color = id),
            family = "cjk", fontface = "bold", hjust = 1, nudge_x = -0.08,
            size = 5, show.legend = FALSE) +
  geom_text(data = filter(label_pts, side == "last"),
            aes(Period, rank, label = id, color = id),
            family = "cjk", fontface = "bold", hjust = 0, nudge_x = 0.08,
            size = 5, show.legend = FALSE) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25)) +
  scale_x_continuous(breaks = periods, expand = expansion(mult = .18)) +
  scale_color_brewer(palette = "Set1") +
  guides(color = "none") +
  labs(title = sprintf("Ranking flow of organizations present in ≥%d periods", MIN_PERIODS),
       subtitle = "Grey = other top-25 entrants, for context",
       x = "Period", y = "Rank (by strength)") +
  theme_minimal(base_family = "cjk") +
  theme(panel.grid.minor = element_blank())

ggsave("R_fig1_bump.png", p_bump, width = 13, height = 8, dpi = 300, bg = "white")

# =====================================================================
# FIGURE 2 — HEATMAP
# =====================================================================
n_periods <- dftop %>% count(id, name = "n_per")
peak      <- dftop %>% group_by(id) %>% summarise(peak = max(strength_modenorm))

ord <- n_periods %>% left_join(peak, by = "id") %>%
  arrange(desc(n_per), desc(peak)) %>%
  slice_head(n = 40) %>% pull(id)             # cap rows for readability

df_hm <- dftop %>%
  filter(id %in% ord) %>%
  mutate(id = factor(id, levels = rev(ord)),  # strongest/most-persistent on top
         Period = factor(Period))

p_hm <- ggplot(df_hm, aes(Period, id, fill = strength_modenorm)) +
  geom_tile(color = "white", linewidth = .3) +
  scale_fill_viridis_c(limits = c(0, 1), name = "strength\n(modenorm)") +
  labs(title = "Strength heatmap (within-period normalized)",
       subtitle = "Blank = not in that period's top-25; rows by # periods present, then peak",
       x = "Period", y = NULL) +
  theme_minimal(base_family = "cjk") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 8))

ggsave("R_fig2_heatmap.png", p_hm, width = 8, height = 11, dpi = 300, bg = "white")

# =====================================================================
# FIGURE 3 — SMALL MULTIPLES (top-12 ranked bars per period)
# =====================================================================
df_sm <- dftop %>% filter(rank <= 12) %>%
  mutate(id = reorder_within(id, strength_modenorm, Period))

p_sm <- ggplot(df_sm, aes(strength_modenorm, id, fill = strength_modenorm)) +
  geom_col() +
  scale_y_reordered() +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 1), guide = "none") +
  facet_wrap(~ Period, scales = "free_y", nrow = 1,
             labeller = labeller(Period = function(x) paste("Period", x))) +
  labs(title = "Top-12 organizations per period by strength (within-period normalized)",
       x = "strength_modenorm", y = NULL) +
  theme_minimal(base_family = "cjk") +
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank())

ggsave("R_fig3_small_multiples.png", p_sm, width = 20, height = 7, dpi = 300, bg = "white")