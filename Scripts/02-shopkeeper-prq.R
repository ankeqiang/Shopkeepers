# ============================================================================
# SHOPKEEPERS IN THE ENGLISH-LANGUAGE CHINESE PRESS: CORPUS CONSTRUCTION
# ============================================================================
# This script builds and analyzes a corpus of articles about shopkeepers
# from English-language newspapers published in China (1850-1950).
# The corpus uses the ProQuest "Chinese Historical Newspapers" collection.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------
# Load required packages for text analysis, visualization, and data manipulation

library(histtext)      # For accessing historical newspaper corpora
library(lubridate)     # For working with dates
library(ggplot2)       # For creating visualizations
library(tidygraph)     # For network analysis
library(igraph)        # For graph structures
library(tidyr)         # For data tidying
library(tidyverse)     # For general data manipulation
library(tidytext)      # For text mining and tokenization

# List all available corpora in the histtext package
histtext::list_corpora()

# ----------------------------------------------------------------------------
# 2. DEFINING THE SEARCH TERMS
# ----------------------------------------------------------------------------
# Unlike Chinese, English has fewer synonym variations for shopkeeper terms
# We focus on four main search patterns:
# - "shopkeeper" (single word, most common)
# - "shopowner" (alternative single word)
# - "shop owner" (two-word phrase)
# - "grocer" (specific type of shopkeeper)

# ----------------------------------------------------------------------------
# 3. SEARCHING THE PROQUEST CORPUS
# ----------------------------------------------------------------------------
# Search for all shopkeeper-related terms in the ProQuest collection

# Initial search combining three single-word terms
shopPRQ <- search_documents("shopkeeper|shopowner|grocer", "proquest")

# Separate search for the two-word phrase (requires exact phrase matching)
shopPRQa <- search_documents('"shop owner"', "proquest")

# Combine both searches and remove duplicates
shopPRQ <- bind_rows(shopPRQ, shopPRQa)
shopPRQ <- unique(shopPRQ)
# Initial compilation: 5,145 articles

# Extract year from date field for temporal analysis
shopPRQ <- shopPRQ %>% 
  mutate(Year = substr(Date, 1, 4))

# Optional: Search the South China Morning Post recent archive
shopSCMP <- search_documents("shopkeeper|shopowner|grocer", "scmp-recent")

# ----------------------------------------------------------------------------
# 4. INDIVIDUAL TERM SEARCHES
# ----------------------------------------------------------------------------
# Search for each term separately to understand their relative frequency

shops1prq <- search_documents("shopkeeper", "proquest")
shops2prq <- search_documents("shopowner", "proquest")
shops3prq <- search_documents("grocer", "proquest")
shops4prq <- search_documents('"shop owner"', "proquest")

# Additional specific shopkeeper types (for potential future analysis)
shops5prq <- search_documents("tailor", "proquest")
shops6prq <- search_documents("butcher", "proquest")

# ----------------------------------------------------------------------------
# 5. TEMPORAL ANALYSIS: OVERALL TRENDS
# ----------------------------------------------------------------------------
# Count articles by year to identify historical patterns

shopsPRQ_Year <- shopPRQ %>% 
  group_by(Year) %>% 
  count()

# Save yearly counts
write_csv(shopsPRQ_Year, "shopsPRQ_Year.csv")

# Visualize the temporal distribution
shopsPRQ_Year %>% 
  mutate(Year = as.numeric(Year)) %>%
  ggplot(aes(Year, n)) + 
  geom_line(color = "orange") + 
  labs(
    title = "Shopkeepers in the ProQuest Collection",
    subtitle = "Number of articles mentioning shopkeepers",
    x = "Year",
    y = "Number of articles"
  )

# Compute statistics across the entire ProQuest corpus
stats_date(shopPRQ, "proquest", to_plot = TRUE, over_all = TRUE, ly = TRUE)

# ----------------------------------------------------------------------------
# 6. DISTRIBUTION BY SOURCE PUBLICATION
# ----------------------------------------------------------------------------
# Analyze which newspapers published the most shopkeeper-related content
# This reveals geographic and editorial patterns

search_documents('"shop owner"|"shopowner"|"shopkeeper"|"grocer"', "proquest") %>%
  mutate(Year = stringr::str_sub(Date, 0, 4)) %>% 
  group_by(Year) %>% 
  count(Source) %>%
  ggplot(aes(x = Year, y = n, fill = Source)) + 
  geom_col(alpha = 0.8) + 
  scale_x_discrete(breaks = c(1850, 1875, 1900, 1925, 1950)) + 
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 8)
  ) + 
  labs(
    title = "Shopkeepers in the English-language press (1850-1950)", 
    subtitle = "Distribution of mentions between newspapers", 
    caption = "Based on data from ProQuest 'Chinese Historical Newspapers'",
    y = "Number of articles"
  )

# ----------------------------------------------------------------------------
# 7. COMPARING INDIVIDUAL TERMS OVER TIME
# ----------------------------------------------------------------------------
# Analyze how different shopkeeper terms were used across the decades

# Step 7.1: Convert dates to years for all term-specific searches
shops1prq <- shops1prq %>% 
  mutate(Date = lubridate::as_date(Date, "%y%m%d"), Year = year(Date))
shops2prq <- shops2prq %>% 
  mutate(Date = lubridate::as_date(Date, "%y%m%d"), Year = year(Date))
shops3prq <- shops3prq %>% 
  mutate(Date = lubridate::as_date(Date, "%y%m%d"), Year = year(Date))
shops4prq <- shops4prq %>% 
  mutate(Date = lubridate::as_date(Date, "%y%m%d"), Year = year(Date))

# Step 7.2: Count occurrences per year for each term
shops1prq_aggregated <- shops1prq %>% group_by(Year) %>% summarise(Count = n())
shops2prq_aggregated <- shops2prq %>% group_by(Year) %>% summarise(Count = n())
shops3prq_aggregated <- shops3prq %>% group_by(Year) %>% summarise(Count = n())
shops4prq_aggregated <- shops4prq %>% group_by(Year) %>% summarise(Count = n())

# Step 7.3: Label each dataset with its term
shops1prq_aggregated$Term <- "shopkeeper"
shops2prq_aggregated$Term <- "shopowner"
shops3prq_aggregated$Term <- "grocer"
shops4prq_aggregated$Term <- "shop owner"

# Step 7.4: Combine all term data
combined_data <- bind_rows(
  shops1prq_aggregated, 
  shops2prq_aggregated, 
  shops3prq_aggregated, 
  shops4prq_aggregated
)

# Step 7.5: Visualize term usage patterns
ggplot(combined_data, aes(x = Year, y = Count, color = Term)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Shopkeeper Term Usage Over Time",
    x = "Year",
    y = "Document Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----------------------------------------------------------------------------
# 8. RETRIEVING FULL ARTICLE TEXTS
# ----------------------------------------------------------------------------
# Get complete text of all articles for in-depth analysis

shopsPRQ_ftext <- histtext::get_documents(shopPRQ, "proquest")

# Calculate article length in characters
shopsPRQ_ftext <- shopsPRQ_ftext %>% 
  mutate(Length = nchar(Text))

# ----------------------------------------------------------------------------
# 9. SEPARATING ADVERTISEMENTS FROM ARTICLES
# ----------------------------------------------------------------------------
# Advertisements have different discourse patterns than news articles
# Separate them for focused analysis

# Extract advertisements (identified by "Display" in title)
shopsPRQads <- shopsPRQ_ftext %>% 
  filter(str_detect(Title, "Display"))

# Create dataset without advertisements (news articles only)
shopsPRQtext <- shopsPRQ_ftext %>% 
  filter(!str_detect(Title, "Display"))

# ----------------------------------------------------------------------------
# 10. FOCUSING ON SHANGHAI NEWSPAPERS
# ----------------------------------------------------------------------------
# Shanghai was the commercial center and publishing hub of treaty-port China
# Focus on major Shanghai English-language newspapers for geographic coherence

shopsPRQsubtext <- shopsPRQtext %>% 
  filter(str_detect(
    Source, 
    "The North China Herald|The China Press|The China Weekly Review|The Shanghai Times|The Shanghai Gazette"
  ))

# Add year field for temporal analysis
shopsPRQsubtext <- shopsPRQsubtext %>% 
  mutate(Year = stringr::str_sub(Date, 0, 4))

# Count articles by year for Shanghai press
shopsPRQsubtext_Year <- shopsPRQsubtext %>% 
  group_by(Year) %>% 
  count()

# Visualize temporal distribution for Shanghai newspapers
shopsPRQsubtext_Year %>% 
  mutate(Year = as.numeric(Year)) %>%
  ggplot(aes(Year, n)) + 
  geom_line(color = "orange") + 
  labs(
    title = "Shopkeepers in the Shanghai Press",
    subtitle = "Number of articles mentioning shopkeepers",
    x = "Year",
    y = "Number of articles"
  )

# ----------------------------------------------------------------------------
# 11. SPLITTING CORPUS BY ARTICLE LENGTH
# ----------------------------------------------------------------------------
# Different article lengths suit different analytical methods
# Threshold: 7,000 characters (roughly 1,000-1,500 words)

# Shorter articles: suitable for batch text mining
shopsPRQ_shrtext <- shopsPRQsubtext %>% 
  filter(Length < 7001)
# Result: 1,453 shorter articles

# Longer articles: require concordance analysis for efficiency
shopsPRQ_lngtext <- shopsPRQsubtext %>% 
  filter(Length > 7000)
# Result: 685 longer articles

# ----------------------------------------------------------------------------
# 12. CONCORDANCE EXTRACTION
# ----------------------------------------------------------------------------
# Extract terms with surrounding context for discourse analysis
# Context size: 220 characters (approximately 30-40 words on each side)

shopsConc1 <- search_concordance("shopkeeper", corpus = "proquest", context_size = 220)
shopsConc2 <- search_concordance('"shopowner"', corpus = "proquest", context_size = 220)
shopsConc3 <- search_concordance('"grocer"', corpus = "proquest", context_size = 220)
shopsConc4 <- search_concordance('"shop owner"', corpus = "proquest", context_size = 220)

# Combine all concordances and remove duplicates
shopsPRQConc <- bind_rows(shopsConc1, shopsConc2, shopsConc3, shopsConc4)
shopsPRQConc <- unique(shopsPRQConc)

# ----------------------------------------------------------------------------
# 13. PROCESSING CONCORDANCE DATA
# ----------------------------------------------------------------------------
# Merge the three concordance columns into readable continuous text

shopsPRQConcWrk <- shopsPRQConc %>% 
  mutate(TextConc = paste(Before, Matched, After, sep = " "))

# Reorganize columns for better readability
shopsPRQConcWrk <- shopsPRQConcWrk %>% 
  relocate(TextConc, .after = Title) %>%
  relocate(Source, .after = After)
# Result: 2,739 concordance extracts

# ----------------------------------------------------------------------------
# 14. JOINING CONCORDANCES WITH FULL TEXTS
# ----------------------------------------------------------------------------
# Link concordance extracts to full articles (for longer texts only)
# This helps locate passages within lengthy documents

shopsPRQConcFull <- left_join(shopsPRQ_lngtext, shopsPRQConcWrk, by = "DocId")

# Remove duplicate document IDs (keep first occurrence)
shopsPRQConcFull <- shopsPRQConcFull %>% 
  distinct(DocId, .keep_all = TRUE)

# ----------------------------------------------------------------------------
# 15. TEXT TOKENIZATION FOR COMPUTATIONAL ANALYSIS
# ----------------------------------------------------------------------------
# Break shorter articles into individual words for frequency analysis
# Remove common English stop words and apply additional filters

# Load standard English stop words
data("stop_words")

shopPRQ_token <- shopsPRQ_shrtext %>% 
  # Tokenize: split text into individual words
  unnest_tokens(output = word, input = Text) %>% 
  # Remove common words (the, a, is, etc.)
  anti_join(stop_words) %>% 
  # Remove numbers
  filter(!str_detect(word, '[0-9]{1,}')) %>% 
  # Remove very short words (likely fragments or abbreviations)
  filter(nchar(word) > 3) %>% 
  # Remove specific common fragments that aren't meaningful
  filter(!word %in% c("tion", "tions", "chin", "ment"))

# ----------------------------------------------------------------------------
# 16. SAVING ALL OUTPUTS
# ----------------------------------------------------------------------------
# Export all processed datasets for future analysis and preservation

write_csv(shopPRQ, "shopPRQ.csv")                          # All articles
write_csv(shopsPRQ_ftext, "shopsPRQ_ftext.csv")            # All full texts
write_csv(shopsPRQsubtext, "shopsPRQ_subtext.csv")         # Shanghai subset
write_csv(shopsPRQ_shrtext, "shopsPRQ_shrtext.csv")        # Short articles
write_csv(shopsPRQ_lngtext, "shopsPRQ_lngtext.csv")        # Long articles
write_csv(shopsPRQConcWrk, "shopsPRQConcWrk.csv")          # Concordances
write_csv(shopsPRQConcFull, "shopsPRQConcFull.csv")        # Concordances + full texts
write_csv(shopPRQ_token, "shopPRQ_token.csv")              # Tokenized words
write_csv(combined_data, "combined_data.csv")              # Term comparison data

# ============================================================================
# END OF SCRIPT
# ============================================================================