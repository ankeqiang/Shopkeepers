# ============================================================================
# NAMED ENTITY RECOGNITION (NER) ON SHOPKEEPER CORPUS
# ============================================================================
# This script extracts named entities from the shopkeeper corpus using the 
# HistText NER model (trftc_nopunct:zh:ner). The corpus is divided into two 
# subcorpora based on article length to address segmentation quality issues:
#   - shops_ftext400: Articles with ≤400 characters (better segmentation)
#   - shopkeeper_complete_ftext401: Articles with >400 characters
#
# The larger subcorpus (ftext401) is further split into 6 parts to:
#   1. Manage memory usage during processing
#   2. Reduce risk of processing failures
#   3. Handle the high volume of named entities in longer texts
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------
# Load required packages

library(histtext)      # For NER extraction on historical Chinese texts
library(lubridate)     # For date manipulation
library(ggplot2)       # For visualization
library(tidygraph)     # For network analysis
library(igraph)        # For graph structures
library(tidyverse)     # For data manipulation
library(tidytext)      # For text mining

# Save workspace (optional - for preserving work in progress)
save.image('shopner2.RData')

# Load previously saved workspace (optional - uncomment if resuming work)
# load(file = "shopner2.RData")

# ----------------------------------------------------------------------------
# 2. NAMED ENTITY EXTRACTION - TWO APPROACHES
# ----------------------------------------------------------------------------

## APPROACH A: DIRECT EXTRACTION (NOT RECOMMENDED FOR LARGE FILES)
## ---------------------------------------------------------------
## This approach extracts named entities in a single operation for each subcorpus.
## WARNING: For ftext401 (longer articles), this is NOT RECOMMENDED because:
##   - Large text size increases processing time significantly
##   - High number of named entities in longer texts may cause memory issues
##   - Risk of process failure increases with file size
##   - No recovery point if extraction fails partway through

# Extract NEs from shorter articles (≤400 characters) - SAFE to run directly
shops_400_ner <- histtext::ner_on_df(
  shops_ftext400,                          # Input dataframe with texts
  "Text",                                  # Column containing text to analyze
  id_column = "DocId",                     # Column with document identifiers
  model = "trftc_nopunct:zh:ner"          # Chinese NER model (no punctuation)
)

# Extract NEs from longer articles (>400 characters) - NOT RECOMMENDED
# Uncomment only if you have sufficient memory and accept the risk
# shops_401_ner <- histtext::ner_on_df(
#   shopkeeper_complete_ftext401, 
#   "Text", 
#   id_column = "DocId", 
#   model = "trftc_nopunct:zh:ner"
# )

# Save direct extraction results
write_csv(shops_400_ner, "shops_400_ner.csv")
# write_csv(shops_401_ner, "shops_401_ner.csv")  # Uncomment if using direct approach


## APPROACH B: SPLIT EXTRACTION IN SIX BATCHES (RECOMMENDED FOR FTEXT401)
## -----------------------------------------------------------------------
## This approach divides the larger subcorpus into 6 approximately equal parts
## and processes each part separately, then merges the results.
## ADVANTAGES:
##   - Manages memory usage more effectively
##   - Allows recovery from failures (reprocess only failed batches)
##   - Provides progress tracking (1/6, 2/6, etc.)
##   - More stable for large-scale processing

# Step 1: Calculate split points for 6 equal parts
total_rows <- nrow(shopkeeper_complete_ftext401)
rows_per_part <- ceiling(total_rows / 6)

# Display split information
cat("Total rows:", total_rows, "\n")
cat("Rows per part:", rows_per_part, "\n\n")

# Step 2: Create six subsets of the data
shops_401_ner_part1 <- shopkeeper_complete_ftext401 %>% 
  slice(1:rows_per_part)

shops_401_ner_part2 <- shopkeeper_complete_ftext401 %>% 
  slice((rows_per_part + 1):(2 * rows_per_part))

shops_401_ner_part3 <- shopkeeper_complete_ftext401 %>% 
  slice((2 * rows_per_part + 1):(3 * rows_per_part))

shops_401_ner_part4 <- shopkeeper_complete_ftext401 %>% 
  slice((3 * rows_per_part + 1):(4 * rows_per_part))

shops_401_ner_part5 <- shopkeeper_complete_ftext401 %>% 
  slice((4 * rows_per_part + 1):(5 * rows_per_part))

shops_401_ner_part6 <- shopkeeper_complete_ftext401 %>% 
  slice((5 * rows_per_part + 1):total_rows)


# Step 3: Extract named entities from each part separately
# Note: Run these one at a time, monitoring progress and memory usage

cat("Processing Part 1/6...\n")
shops_401_ner1 <- histtext::ner_on_df(
  shops_401_ner_part1, 
  "Text", 
  id_column = "DocId", 
  model = "trftc_nopunct:zh:ner"
)

cat("Processing Part 2/6...\n")
shops_401_ner2 <- histtext::ner_on_df(
  shops_401_ner_part2, 
  "Text", 
  id_column = "DocId", 
  model = "trftc_nopunct:zh:ner"
)

cat("Processing Part 3/6...\n")
shops_401_ner3 <- histtext::ner_on_df(
  shops_401_ner_part3, 
  "Text", 
  id_column = "DocId", 
  model = "trftc_nopunct:zh:ner"
)

cat("Processing Part 4/6...\n")
shops_401_ner4 <- histtext::ner_on_df(
  shops_401_ner_part4, 
  "Text", 
  id_column = "DocId", 
  model = "trftc_nopunct:zh:ner"
)

cat("Processing Part 5/6...\n")
shops_401_ner5 <- histtext::ner_on_df(
  shops_401_ner_part5, 
  "Text", 
  id_column = "DocId", 
  model = "trftc_nopunct:zh:ner"
)

cat("Processing Part 6/6...\n")
shops_401_ner6 <- histtext::ner_on_df(
  shops_401_ner_part6, 
  "Text", 
  id_column = "DocId", 
  model = "trftc_nopunct:zh:ner"
)

# Step 4: Merge all six extracted named entity datasets into one complete file
cat("Merging all parts...\n")
shops_401_ner <- bind_rows(
  shops_401_ner1, 
  shops_401_ner2, 
  shops_401_ner3, 
  shops_401_ner4, 
  shops_401_ner5, 
  shops_401_ner6
)

# Save the complete merged result
write_csv(shops_401_ner, "shops_401_ner_complete.csv")

cat("Extraction complete! Total entities extracted:", nrow(shops_401_ner), "\n")

# ----------------------------------------------------------------------------
# 3. PROCESS NAMED ENTITIES FROM SHORTER ARTICLES (≤400 CHARACTERS)
# ----------------------------------------------------------------------------

cat("\nProcessing NER results from shorter articles...\n")

# Step 3.1: Remove unnecessary position columns
# The Start and End columns indicate character positions but aren't needed for analysis
shops_400_ner <- shops_400_ner %>% 
  select(-Start, -End)

# Step 3.2: Filter out numerical and temporal entities
# Remove entity types that represent numbers, dates, money, etc.
# These are often not substantively meaningful for historical analysis
# Entity types removed:
#   - CARDINAL: cardinal numbers (一, 二, 三, etc.)
#   - DATE: dates (昨日, 今年, etc.)
#   - QUANTITY: quantities with units
#   - MONEY: monetary values
#   - ORDINAL: ordinal numbers (第一, 第二, etc.)
#   - TIME: time expressions
shops_400_ner2 <- shops_400_ner %>%
  filter(!str_detect(Type, "CARDINAL|DATE|QUANTITY|MONEY|ORDINAL|TIME"))

# Step 3.3: Remove noisy outputs and artifacts
# Filter 1: Keep only entities containing Chinese characters
shops_400_ner2 <- shops_400_ner2 %>% 
  filter(stringr::str_detect(Text, "[\\p{Han}]"))

# Filter 2: Remove specific noise patterns identified during exploratory analysis
shops_400_ner2 <- shops_400_ner2 %>% 
  filter(!stringr::str_detect(Text, "·養正·且華·紫金·靜文"))

# Filter 3: Remove OCR artifacts and formatting marks
shops_400_ner2 <- shops_400_ner2 %>% 
  mutate(Text = str_remove_all(Text, "㈠|㈢|㈤|㈥|㈩㈠|㈣|○　|○|r|'"))

# Step 3.4: Remove single-character entities
# Single characters are often noise or ambiguous
shops_400_ner2 <- shops_400_ner2 %>%
  filter(nchar(Text) > 1)

# Save cleaned results
write_csv(shops_400_ner2, "shops_400_ner2.csv")

# Step 3.5: Create separate files for each entity type
# This facilitates type-specific analysis and reduces file size for focused work

# PERSON entities (人名) - individuals mentioned in articles
shops_400_PER <- shops_400_ner %>% 
  filter(Type == "PERSON") %>%
  filter(nchar(Text) > 1)

# ORGANIZATION entities (机构名) - companies, institutions, organizations
shops_400_ORG <- shops_400_ner %>% 
  filter(Type == "ORG") %>%
  filter(nchar(Text) > 1)

# LOCATION entities (地点名) - geographical locations, buildings
shops_400_LOC <- shops_400_ner %>% 
  filter(Type == "LOC") %>%
  filter(nchar(Text) > 1)

# GPE entities (地缘政治实体) - countries, cities, states
shops_400_GPE <- shops_400_ner %>% 
  filter(Type == "GPE") %>%
  filter(nchar(Text) > 1)

# NORP entities (民族/宗教/政治团体) - nationalities, religious/political groups
shops_400_NORP <- shops_400_ner %>% 
  filter(Type == "NORP") %>%
  filter(nchar(Text) > 1)

# FACILITY entities (设施名) - buildings, airports, highways, bridges
shops_400_FAC <- shops_400_ner %>% 
  filter(Type == "FAC") %>%
  filter(nchar(Text) > 1)

# EVENT entities (事件名) - named hurricanes, battles, wars, sports events
shops_400_EVE <- shops_400_ner %>% 
  filter(Type == "EVENT") %>%
  filter(nchar(Text) > 1)

# Save entity-type-specific files
write_csv(shops_400_PER, "shops_400_PER.csv")
write_csv(shops_400_ORG, "shops_400_ORG.csv")
write_csv(shops_400_LOC, "shops_400_LOC.csv")
write_csv(shops_400_GPE, "shops_400_GPE.csv")
write_csv(shops_400_NORP, "shops_400_NORP.csv")
write_csv(shops_400_FAC, "shops_400_FAC.csv")
write_csv(shops_400_EVE, "shops_400_EVE.csv")

cat("Shorter articles processing complete!\n")

# ----------------------------------------------------------------------------
# 4. PROCESS NAMED ENTITIES FROM LONGER ARTICLES (>400 CHARACTERS)
# ----------------------------------------------------------------------------

cat("\nProcessing NER results from longer articles...\n")

# Apply the same cleaning steps as for shorter articles (Section 3)

# Step 4.1: Remove unnecessary position columns
shops_401_ner <- shops_401_ner %>% 
  select(-Start, -End)

# Step 4.2: Filter out numerical and temporal entities
shops_401_ner2 <- shops_401_ner %>%
  filter(!str_detect(Type, "CARDINAL|DATE|QUANTITY|MONEY|ORDINAL|TIME"))

# Step 4.3: Remove noisy outputs and artifacts
shops_401_ner2 <- shops_401_ner2 %>% 
  filter(stringr::str_detect(Text, "[\\p{Han}]"))

shops_401_ner2 <- shops_401_ner2 %>% 
  filter(!stringr::str_detect(Text, "·養正·且華·紫金·靜文"))

shops_401_ner2 <- shops_401_ner2 %>% 
  mutate(Text = str_remove_all(Text, "㈠|㈢|㈤|㈥|㈩㈠|㈣|○　|○|r|'"))

# Step 4.4: Remove single-character entities
shops_401_ner2 <- shops_401_ner2 %>%
  filter(nchar(Text) > 1)

# Save cleaned results
write_csv(shops_401_ner2, "shops_401_ner2.csv")

# Step 4.5: Create separate files for each entity type
shops_401_PER <- shops_401_ner2 %>% 
  filter(Type == "PERSON") %>%
  filter(nchar(Text) > 1)

shops_401_ORG <- shops_401_ner2 %>% 
  filter(Type == "ORG") %>%
  filter(nchar(Text) > 1)

shops_401_LOC <- shops_401_ner2 %>% 
  filter(Type == "LOC") %>%
  filter(nchar(Text) > 1)

shops_401_GPE <- shops_401_ner2 %>% 
  filter(Type == "GPE") %>%
  filter(nchar(Text) > 1)

shops_401_NORP <- shops_401_ner2 %>% 
  filter(Type == "NORP") %>%
  filter(nchar(Text) > 1)

shops_401_FAC <- shops_401_ner2 %>% 
  filter(Type == "FAC") %>%
  filter(nchar(Text) > 1)

shops_401_EVE <- shops_401_ner2 %>% 
  filter(Type == "EVENT") %>%
  filter(nchar(Text) > 1)

# Save entity-type-specific files
write_csv(shops_401_PER, "shops_401_PER.csv")
write_csv(shops_401_ORG, "shops_401_ORG.csv")
write_csv(shops_401_LOC, "shops_401_LOC.csv")
write_csv(shops_401_GPE, "shops_401_GPE.csv")
write_csv(shops_401_NORP, "shops_401_NORP.csv")
write_csv(shops_401_FAC, "shops_401_FAC.csv")
write_csv(shops_401_EVE, "shops_401_EVE.csv")

cat("Longer articles processing complete!\n")

# ----------------------------------------------------------------------------
# 5. FINAL COMPILATION - MERGE BOTH SUBCORPORA
# ----------------------------------------------------------------------------

cat("\nMerging results from both subcorpora...\n")

# Combine named entities from shorter and longer articles by type
# This creates complete datasets spanning the entire corpus

# Compile the two main NER files
shops_ner <- bind_rows(shops_400_ner2, shops_401_ner2)

shopsnerPER <- bind_rows(shops_400_PER, shops_401_PER)
shopsnerORG <- bind_rows(shops_400_ORG, shops_401_ORG)
shopsnerLOC <- bind_rows(shops_400_LOC, shops_401_LOC)
shopsnerGPE <- bind_rows(shops_400_GPE, shops_401_GPE)
shopsnerNORP <- bind_rows(shops_400_NORP, shops_401_NORP)
shopsnerFAC <- bind_rows(shops_400_FAC, shops_401_FAC)
shopsnerEVE <- bind_rows(shops_400_EVE, shops_401_EVE)

# Save final compiled datasets
write_csv(shops_ner, "shops_ner.csv")
write_csv(shopsnerPER, "shopsnerPER.csv")
write_csv(shopsnerORG, "shopsnerORG.csv")
write_csv(shopsnerLOC, "shopsnerLOC.csv")
write_csv(shopsnerGPE, "shopsnerGPE.csv")
write_csv(shopsnerNORP, "shopsnerNORP.csv")
write_csv(shopsnerFAC, "shopsnerFAC.csv")
write_csv(shopsnerEVE, "shopsnerEVE.csv")

# Display summary statistics
cat("\n=== EXTRACTION SUMMARY ===\n")
cat("PERSON entities:", nrow(shopsnerPER), "\n")
cat("ORGANIZATION entities:", nrow(shopsnerORG), "\n")
cat("LOCATION entities:", nrow(shopsnerLOC), "\n")
cat("GPE entities:", nrow(shopsnerGPE), "\n")
cat("NORP entities:", nrow(shopsnerNORP), "\n")
cat("FACILITY entities:", nrow(shopsnerFAC), "\n")
cat("EVENT entities:", nrow(shopsnerEVE), "\n")
cat("Total entities extracted:", 
    nrow(shopsnerPER) + nrow(shopsnerORG) + nrow(shopsnerLOC) + 
      nrow(shopsnerGPE) + nrow(shopsnerNORP) + nrow(shopsnerFAC) + 
      nrow(shopsnerEVE), "\n")

# Save final workspace
save.image('shopner.RData')

cat("\n=== NER EXTRACTION COMPLETE ===\n")

# ============================================================================
# END OF SCRIPT
# ============================================================================
#
# NOTES FOR USERS:
# ---------------
# 1. The split extraction approach (Approach B) is strongly recommended for
#    large corpora to avoid memory issues and processing failures.
#
# 2. Entity types extracted:
#    - PERSON: Personal names
#    - ORG: Organizations, companies, institutions
#    - LOC: Locations (non-GPE)
#    - GPE: Geopolitical entities (countries, cities, states)
#    - NORP: Nationalities, religious or political groups
#    - FAC: Facilities (buildings, structures)
#    - EVENT: Named events (wars, conferences, etc.)
#
# 3. Filtered entity types (not retained in final outputs):
#    - CARDINAL, DATE, QUANTITY, MONEY, ORDINAL, TIME
#    These are removed because they typically don't contribute to 
#    historical or social network analysis of actors and institutions.
#
# 4. The cleaning process removes:
#    - Single-character entities (often ambiguous)
#    - Entities without Chinese characters
#    - OCR artifacts and formatting marks
#    - Specific noise patterns identified during analysis
#
# 5. For adapting this script to other corpora:
#    - Adjust the length threshold (400 characters) based on your data
#    - Modify noise patterns to match your specific corpus
#    - Consider different split numbers based on corpus size
#    - Add additional entity types if needed for your research
# ============================================================================