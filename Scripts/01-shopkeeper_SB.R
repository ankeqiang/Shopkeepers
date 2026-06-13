# ============================================================================
# SHOPKEEPERS IN THE SHENBAO: CORPUS CONSTRUCTION AND ANALYSIS
# ============================================================================
# This script builds and analyzes a corpus of articles about shopkeepers
# from the Shenbao (申报), a major Chinese newspaper (1872-1949).
# The corpus uses 16 Chinese terms related to shopkeepers, identified through
# word embeddings and expert consultation.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------
# Load required packages for text analysis, visualization, and data manipulation

library(histtext)      # For accessing historical Chinese text corpora
library(lubridate)     # For working with dates
library(ggplot2)       # For creating visualizations
library(tidygraph)     # For network analysis
library(igraph)        # For graph structures
library(tidyr)         # For data tidying
library(tidyverse)     # For general data manipulation
library(tidytext)      # For text mining
library(RColorBrewer)  # For color palettes in visualizations

# ----------------------------------------------------------------------------
# 2. DEFINING THE SEARCH TERMS
# ----------------------------------------------------------------------------
# The 16 terms for "shopkeeper" were identified using:
# - HistText Word Embeddings function (13 terms)
# - Expert consultation with Chinese historian (3 terms: 东家, 东主, 店家)
# - ChatGPT suggestions (3 terms: 掌柜, 商户, 业主)
#
# Terms and their meanings:
# 坊主 - workshop owner
# 店主 - shop owner (most common)
# 栈主 - warehouse owner
# 庄主 - estate/manor owner
# 号主 - firm owner
# 行主 - guild/trade association head
# 铺主 - store owner
# 馆主 - establishment owner
# 店主妇 - female shop owner
# 店东 - shop proprietor
# 东家 - employer/proprietor
# 东主 - master/proprietor
# 店家 - shop family/proprietor
# 掌柜 - shopkeeper/manager
# 商户 - merchant household
# 业主 - business owner

# ----------------------------------------------------------------------------
# 3. SEARCHING THE CORPUS
# ----------------------------------------------------------------------------
# Search for all 16 terms together in the Shenbao corpus

shopsSB <- search_documents(
  '"坊主|店主|栈主|庄主|号主|行主|铺主|馆主|店主妇|店东|东家|东主|店家|掌柜|商户|业主"',
  "shunpao-revised"
)

# Search for each term individually to understand their distribution
shops2 <- search_documents('"坊主"', "shunpao-revised")
shops3 <- search_documents('"店主"', "shunpao-revised")
shops4 <- search_documents('"栈主"', "shunpao-revised")
shops6 <- search_documents('"庄主"', "shunpao-revised")
shops7 <- search_documents('"号主"', "shunpao-revised")
shops8 <- search_documents('"行主"', "shunpao-revised")
shops9 <- search_documents('"铺主"', "shunpao-revised")
shops10 <- search_documents('"馆主"', "shunpao-revised")
shops11 <- search_documents('"店主妇"', "shunpao-revised")
shops12 <- search_documents('"店东"', "shunpao-revised")
shops13 <- search_documents('"东家"', "shunpao-revised")
shops14 <- search_documents('"东主"', "shunpao-revised")
shops15 <- search_documents('"店家"', "shunpao-revised")
shops16 <- search_documents('"掌柜"', "shunpao-revised")
shops17 <- search_documents('"商户"', "shunpao-revised")
shops18 <- search_documents('"业主"', "shunpao-revised")

# Combine all individual searches into one dataset
shopsAll <- bind_rows(
  shops2, shops3, shops4, shops6, shops7, shops8, shops9, shops10,
  shops11, shops12, shops13, shops14, shops15, shops16, shops17, shops18
)

# Remove duplicate articles (some articles contain multiple terms)
shopsAll <- unique(shopsAll)
# Initial compilation: 69,851 articles

# ----------------------------------------------------------------------------
# 4. TEMPORAL ANALYSIS: ARTICLES BY YEAR
# ----------------------------------------------------------------------------
# Count how many articles mention shopkeepers each year

shopsAll_Year <- shopsAll %>% 
  group_by(Year) %>% 
  count()

# Save the yearly counts
write_csv(shopsAll_Year, "shopsAll_Year.csv")

# Create a line graph showing the trend over time
shopsAll_Year %>% 
  ggplot(aes(Year, n)) + 
  geom_line(color = "orange") + 
  theme_light() +
  labs(
    title = "Shopkeepers in the Shenbao",
    subtitle = "Number of articles mentioning shopkeepers",
    x = "Year",
    y = "Number of articles"
  )

# Compute statistics across the entire Shenbao corpus
stats_date(shopsAll, "shunpao", to_plot = TRUE, over_all = TRUE, ly = TRUE)

# ----------------------------------------------------------------------------
# 5. INDIVIDUAL TERM ANALYSIS: TEMPORAL DISTRIBUTION
# ----------------------------------------------------------------------------
# Example: Analyze the most common term (店主) over time

histtext::count_documents("店主", "shunpao-revised") %>% 
  mutate(Date = lubridate::as_date(Date, "%y%m%d")) %>% 
  mutate(Year = year(Date)) %>%  
  group_by(Year) %>% 
  summarise(N = sum(N)) %>% 
  ggplot(aes(Year, N)) + 
  geom_col() + 
  labs(
    title = "店主 in the Shenbao",
    subtitle = "Number of articles mentioning 店主",
    x = "Year",
    y = "Number of articles"
  )

# ----------------------------------------------------------------------------
# 6. COMPARING ALL TERMS OVER TIME
# ----------------------------------------------------------------------------
# Step 6.1: Convert dates to years for all individual search results

shops2 <- shops2 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops3 <- shops3 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops4 <- shops4 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops6 <- shops6 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops7 <- shops7 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops8 <- shops8 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops9 <- shops9 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops10 <- shops10 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops11 <- shops11 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops12 <- shops12 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops13 <- shops13 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops14 <- shops14 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops15 <- shops15 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops16 <- shops16 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops17 <- shops17 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))
shops18 <- shops18 %>% mutate(Date = as_date(Date, "%y%m%d"), Year = year(Date))

# Step 6.2: Count occurrences per year for each term

shops2_aggregated <- shops2 %>% group_by(Year) %>% summarise(Count = n())
shops3_aggregated <- shops3 %>% group_by(Year) %>% summarise(Count = n())
shops4_aggregated <- shops4 %>% group_by(Year) %>% summarise(Count = n())
shops6_aggregated <- shops6 %>% group_by(Year) %>% summarise(Count = n())
shops7_aggregated <- shops7 %>% group_by(Year) %>% summarise(Count = n())
shops8_aggregated <- shops8 %>% group_by(Year) %>% summarise(Count = n())
shops9_aggregated <- shops9 %>% group_by(Year) %>% summarise(Count = n())
shops10_aggregated <- shops10 %>% group_by(Year) %>% summarise(Count = n())
shops11_aggregated <- shops11 %>% group_by(Year) %>% summarise(Count = n())
shops12_aggregated <- shops12 %>% group_by(Year) %>% summarise(Count = n())
shops13_aggregated <- shops13 %>% group_by(Year) %>% summarise(Count = n())
shops14_aggregated <- shops14 %>% group_by(Year) %>% summarise(Count = n())
shops15_aggregated <- shops15 %>% group_by(Year) %>% summarise(Count = n())
shops16_aggregated <- shops16 %>% group_by(Year) %>% summarise(Count = n())
shops17_aggregated <- shops17 %>% group_by(Year) %>% summarise(Count = n())
shops18_aggregated <- shops18 %>% group_by(Year) %>% summarise(Count = n())

# Step 6.3: Label each dataset with its corresponding term

shops2_aggregated$Term <- "坊主"
shops3_aggregated$Term <- "店主"
shops4_aggregated$Term <- "栈主"
shops6_aggregated$Term <- "庄主"
shops7_aggregated$Term <- "号主"
shops8_aggregated$Term <- "行主"
shops9_aggregated$Term <- "铺主"
shops10_aggregated$Term <- "馆主"
shops11_aggregated$Term <- "店主妇"
shops12_aggregated$Term <- "店东"
shops13_aggregated$Term <- "东家"
shops14_aggregated$Term <- "东主"
shops15_aggregated$Term <- "店家"
shops16_aggregated$Term <- "掌柜"
shops17_aggregated$Term <- "商户"
shops18_aggregated$Term <- "业主"

# Step 6.4: Combine all aggregated data into one dataset

combined_data <- bind_rows(
  shops2_aggregated, shops3_aggregated, shops4_aggregated, shops6_aggregated,
  shops7_aggregated, shops8_aggregated, shops9_aggregated, shops10_aggregated,
  shops11_aggregated, shops12_aggregated, shops13_aggregated, shops14_aggregated,
  shops15_aggregated, shops16_aggregated, shops17_aggregated, shops18_aggregated
)

# Step 6.5: Create visualization with custom color palette

color_palette <- c(
  "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", 
  "#a65628", "#f781bf", "#999999", "#66c2a5", "#fc8d62", "#8da0cb",
  "#e78ac3", "#a6d854", "#ffd92f"
)

ggplot(combined_data, aes(x = Year, y = Count, color = Term)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  labs(
    title = "Term Usage Over Time",
    x = "Year",
    y = "Document Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----------------------------------------------------------------------------
# 7. CONCORDANCE EXTRACTION: TERMS IN CONTEXT
# ----------------------------------------------------------------------------
# Extract each term with 120 characters of surrounding context
# This allows us to see how shopkeepers are discussed in the articles

shopsConc2 <- search_concordance('"坊主"', corpus = "shunpao-revised", context_size = 120)
shopsConc3 <- search_concordance('"店主"', corpus = "shunpao-revised", context_size = 120)
shopsConc4 <- search_concordance('"栈主"', corpus = "shunpao-revised", context_size = 120)
shopsConc5 <- search_concordance('"舖主"', corpus = "shunpao-revised", context_size = 120)
shopsConc6 <- search_concordance('"庄主"', corpus = "shunpao-revised", context_size = 120)
shopsConc7 <- search_concordance('"号主"', corpus = "shunpao-revised", context_size = 120)
shopsConc8 <- search_concordance('"行主"', corpus = "shunpao-revised", context_size = 120)
shopsConc9 <- search_concordance('"铺主"', corpus = "shunpao-revised", context_size = 120)
shopsConc10 <- search_concordance('"馆主"', corpus = "shunpao-revised", context_size = 120)
shopsConc11 <- search_concordance('"店主妇"', corpus = "shunpao-revised", context_size = 120)
shopsConc12 <- search_concordance('"店东"', corpus = "shunpao-revised", context_size = 120)
shopsConc13 <- search_concordance('"东家"', corpus = "shunpao-revised", context_size = 120)
shopsConc14 <- search_concordance('"东主"', corpus = "shunpao-revised", context_size = 120)
shopsConc15 <- search_concordance('"店家"', corpus = "shunpao-revised", context_size = 120)
shopsConc16 <- search_concordance('"掌柜"', corpus = "shunpao-revised", context_size = 120)
shopsConc17 <- search_concordance('"商户"', corpus = "shunpao-revised", context_size = 120)
shopsConc18 <- search_concordance('"业主"', corpus = "shunpao-revised", context_size = 120)

# Combine all concordance results
shopsConc <- bind_rows(
  shopsConc2, shopsConc3, shopsConc4, shopsConc5, shopsConc6, shopsConc7,
  shopsConc8, shopsConc9, shopsConc10, shopsConc11, shopsConc12, shopsConc13,
  shopsConc14, shopsConc15, shopsConc16, shopsConc17, shopsConc18
)

# Remove duplicates
shopsConc <- unique(shopsConc)

# ----------------------------------------------------------------------------
# 8. CLEANING FALSE POSITIVES
# ----------------------------------------------------------------------------
# Remove instances where the matched characters are part of different words
# or followed by punctuation that indicates they're not the intended term

shopsConc <- shopsConc %>%
  filter(!str_detect(Matched, "行。|行·　|行）|行」|店、|作、|業。|行、|東、|作，主|作。|作·|作（|作“|作㈢|作「|作〕|作，|作：|作）|作　，"))

# Merge the three concordance columns (Before, Matched, After) into one text
shopsConcWrk <- shopsConc %>% 
  mutate(TextConc = paste(Before, Matched, After, sep = ""))

# Remove the separate concordance columns (no longer needed)
shopsConc <- shopsConcWrk %>% 
  select(-Before, -Matched, -After, -Source, -Title, -Date)

# Calculate the length of each concordance text
shopsConc <- shopsConc %>% 
  mutate(Size = nchar(TextConc))

# Final count: 107,287 occurrences (more than the number of articles,
# meaning some articles contain multiple mentions)

# ----------------------------------------------------------------------------
# 9. RETRIEVING FULL ARTICLE TEXTS
# ----------------------------------------------------------------------------
# Get the complete text of all articles mentioning shopkeepers

shops_ftext <- histtext::get_documents(shopsAll, "shunpao-revised")

# Add article length in characters
shops_ftext <- shops_ftext %>% 
  mutate(Length = nchar(Text))

# Remove unnecessary Source column
shops_ftext <- shops_ftext %>% 
  select(-Source)

# Clean the dataset by removing problematic entries
shops_ftext <- shops_ftext %>% 
  filter(!is.na(Text)) %>%           # Remove rows with missing text
  filter(Length > 7) %>%              # Remove very short entries
  filter(Length < 23336)              # Remove extremely long outliers

# After filtering: 69,591 articles remain

# ----------------------------------------------------------------------------
# 10. JOINING CONCORDANCES WITH FULL TEXTS
# ----------------------------------------------------------------------------
# Create a combined dataset that links concordance extracts to full articles
# This helps locate the extracted text within longer documents

shops_concfull <- inner_join(shops_ftext, shopsConc, by = "DocId")

# ----------------------------------------------------------------------------
# 11. CREATING SUBCORPORA BY LENGTH
# ----------------------------------------------------------------------------
# Divide the corpus into shorter and longer articles for different analyses

# Subcorpus 1: Longer articles (>400 characters)
shops_ftext401 <- shops_ftext %>% 
  filter(Length > 400)

# Extract first 20 rows as a sample
shops_ftext20r <- shops_ftext401 %>% 
  slice(1:20)

# Subcorpus 2: Shorter articles (≤400 characters)
# These are mostly unique, standalone articles ideal for workflow development
shops_ftext400 <- shops_ftext %>% 
  filter(Length < 401)

# ----------------------------------------------------------------------------
# 12. TEXT TOKENIZATION
# ----------------------------------------------------------------------------
# Break down the shorter articles into individual words for further analysis
# Uses a specialized Chinese word segmentation model trained on Shenbao texts

shops_400ft_tok <- cws_on_df(
  shops_ftext400, 
  text_column = "Text",
  id_column = "DocId",
  model = "trftc_shunpao_23:zh:cws",  # Shenbao-specific segmentation model
  detailed_output = FALSE,
  token_separator = " ",
  verbose = TRUE
)

# ----------------------------------------------------------------------------
# 13. SAVING ALL OUTPUTS
# ----------------------------------------------------------------------------
# Export all processed datasets for future analysis

write_csv(shopsAll, "shopsAll.csv")                    # All articles
write_csv(shopsConc, "shopsConc.csv")                  # All concordances
write_csv(shops_concfull, "shops_concfull.csv")        # Concordances + full texts
write_csv(shops_ftext, "shops_ftext.csv")              # All full texts
write_csv(shops_ftext400, "shops_ftext400.csv")        # Short articles
write_csv(shops_ftext401, "shops_ftext401.csv")        # Long articles
write_csv(combined_data, "combined_data.csv")          # Term comparison data
write_csv(shops_400ft_tok, "shops_400ft_tok.csv")      # Tokenized short articles


# ============================================================================
# END OF SCRIPT
# ============================================================================