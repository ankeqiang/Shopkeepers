# ============================================================================
# SOCIOLOGICAL DATA ANALYSIS: SHOPKEEPERS IN THE SHENBAO
# ============================================================================
# This script performs basic analysis and processing of sociological data
# extracted from Shenbao newspaper articles about shopkeepers using Large
# Language Model (LLM) processing via the Claude API.
#
# The sociological data includes:
#   - Personal names (Name)
#   - Geographic origins (Origin)
#   - Shop names and businesses (Shop, Shop1-5)
#   - Shop types/categories (Shop_Type)
#   - Locations (Location)
#   - Institutions involved (Institution, Institution1-5)
#   - Other actors mentioned (Other_Actors)
#
# WORKFLOW:
#   1. Prepare and subset the data
#   2. Generate frequency counts for each variable
#   3. Handle multi-valued fields (shops, institutions, types)
#   4. Clean and standardize extracted text
#   5. Split concatenated values into separate columns
#   6. Export processed data for subsequent analysis
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------

# Load required packages
library(histtext)      # For accessing historical text corpora
library(lubridate)     # For date manipulation
library(ggplot2)       # For visualization
library(tidyr)         # For data reshaping and tidying
library(tidyverse)     # For general data manipulation
library(tidytext)      # For text mining

# Save workspace (optional - for preserving work in progress)
save.image('shopsoc.RData')

# Load previously saved workspace (optional - uncomment if resuming work)
# load(file = "shopsoc.RData")

# ----------------------------------------------------------------------------
# 2. DATA PREPARATION
# ----------------------------------------------------------------------------
# Purpose: Select relevant columns from the raw sociological data

# Select first 8 columns from the extracted sociological data
# These columns contain the core sociological variables:
# DocId, Name, Origin, Shop, Shop_Type, Location, Institution, Other_Actors
shops_socdt <- shops_socdata[, 1:8]

cat("Data preparation complete. Working with", ncol(shops_socdt), "columns\n")

# ----------------------------------------------------------------------------
# 3. FREQUENCY ANALYSIS: GENERATE COUNTS FOR EACH VARIABLE
# ----------------------------------------------------------------------------
# Purpose: Create frequency tables for all sociological variables to understand
# the distribution of names, origins, shop types, locations, institutions, etc.

cat("\nGenerating frequency counts for all variables...\n")

# 3.1 Personal Names
# Count frequency of each unique name mentioned in the articles
name <- shops_socdt %>% 
  group_by(Name) %>% 
  count() %>%
  arrange(desc(n))  # Sort by frequency, most common first

cat("Unique names identified:", nrow(name), "\n")

# 3.2 Geographic Origins
# Count frequency of each origin/native place mentioned
origin <- shops_socdt %>% 
  group_by(Origin) %>% 
  count() %>%
  arrange(desc(n))

cat("Unique origins identified:", nrow(origin), "\n")

# 3.3 Shop Names (Multiple Columns)
# Some articles mention multiple shops, stored in Shop, Shop1-5 columns
# Generate frequency counts for each column

# Main shop column (concatenated values)
shop <- shops_socdt %>% 
  group_by(Shop) %>% 
  count() %>%
  arrange(desc(n))

# Individual shop columns (after splitting)
shop1 <- shops_socdt %>% 
  group_by(Shop1) %>% 
  count() %>%
  arrange(desc(n))

shop2 <- shops_socdt %>% 
  group_by(Shop2) %>% 
  count() %>%
  arrange(desc(n))

shop3 <- shops_socdt %>% 
  group_by(Shop3) %>% 
  count() %>%
  arrange(desc(n))

shop4 <- shops_socdt %>% 
  group_by(Shop4) %>% 
  count() %>%
  arrange(desc(n))

shop5 <- shops_socdt %>% 
  group_by(Shop5) %>% 
  count() %>%
  arrange(desc(n))

cat("Shop frequency tables generated\n")

# 3.4 Calculate Number of Shops per Article
# Purpose: Understand how many shops are mentioned in each article
# Method: Count commas in the Shop field (commas separate multiple shops)

# Count commas and add 1 (if there are N commas, there are N+1 shops)
shops_socdt$Shop_Nbr <- str_count(shops_socdt$Shop, ",") + 1

# Correction: If Shop is NA (no shop mentioned), set Shop_Nbr to NA instead of 1
shops_socdt$Shop_Nbr[is.na(shops_socdt$Shop)] <- NA

# Identify articles with many shops (>5 shops mentioned)
# These may represent complex cases or market reports
shop_multi <- shops_socdt %>% 
  filter(Shop_Nbr > 5)

cat("Articles with >5 shops:", nrow(shop_multi), "\n")

# 3.5 Locations
# Count frequency of each location mentioned
location <- shops_socdt %>% 
  group_by(Location) %>% 
  count() %>%
  arrange(desc(n))

cat("Unique locations identified:", nrow(location), "\n")

# 3.6 Institutions (Multiple Columns)
# Articles often mention multiple institutions (courts, police, guilds, etc.)

# Main institution column (concatenated values)
institutions <- shops_socdt %>% 
  group_by(Institution) %>% 
  count() %>%
  arrange(desc(n))

# Individual institution columns (after splitting)
institutions1 <- shops_socdt %>% 
  group_by(Institution1) %>% 
  count() %>%
  arrange(desc(n))

institutions2 <- shops_socdt %>% 
  group_by(Institution2) %>% 
  count() %>%
  arrange(desc(n))

institutions3 <- shops_socdt %>% 
  group_by(Institution3) %>% 
  count() %>%
  arrange(desc(n))

institutions4 <- shops_socdt %>% 
  group_by(Institution4) %>% 
  count() %>%
  arrange(desc(n))

institutions5 <- shops_socdt %>% 
  group_by(Institution5) %>% 
  count() %>%
  arrange(desc(n))

cat("Institution frequency tables generated\n")

# 3.7 Shop Types
# Categories of businesses (tea shop, rice shop, pawn shop, etc.)
shoptype <- shops_socdt %>% 
  group_by(Shop_Type) %>% 
  count() %>%
  arrange(desc(n))

cat("Unique shop types identified:", nrow(shoptype), "\n")

# 3.8 Calculate Number of Shop Types per Article
# Some articles mention multiple types of businesses

# Count commas and add 1
shops_socdt$Type_Nbr <- str_count(shops_socdt$Shop_Type, ",") + 1

# Correction: If Shop_Type is NA, set Type_Nbr to NA instead of 1
shops_socdt$Type_Nbr[is.na(shops_socdt$Shop_Type)] <- NA

# Identify articles with multiple shop types
Type_Multi <- shops_socdt %>% 
  filter(Type_Nbr > 1)

cat("Articles with multiple shop types:", nrow(Type_Multi), "\n")

# 3.9 Other Actors
# Count frequency of other actors mentioned (employees, customers, officials, etc.)
otheractors <- shops_socdt %>% 
  group_by(Other_Actors) %>% 
  count() %>%
  arrange(desc(n))

cat("Unique other actors identified:", nrow(otheractors), "\n")

# 3.10 Documents
# Count how many entries per document (some documents may have multiple extractions)
documents <- shops_socdt %>% 
  group_by(DocId) %>% 
  count() %>%
  arrange(desc(n))

cat("Total documents represented:", nrow(documents), "\n")

# ----------------------------------------------------------------------------
# 4. EXPORT FREQUENCY TABLES
# ----------------------------------------------------------------------------
# Purpose: Save all frequency counts as CSV files for further analysis

cat("\nExporting frequency tables...\n")

# Main processed dataset
write.csv(shops_socdt, "shops_socdt.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Individual variable frequency tables
write.csv(name, "name.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(origin, "origin.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop, "shop.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop1, "shop1.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop2, "shop2.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop3, "shop3.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop4, "shop4.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop5, "shop5.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shop_multi, "shop_multi.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shoptype, "shoptype.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(Type_Multi, "Type_Multi.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(location, "location.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(institutions, "institutions.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(institutions1, "institutions1.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(institutions2, "institutions2.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(institutions3, "institutions3.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(institutions4, "institutions4.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(institutions5, "institutions5.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(otheractors, "otheractors.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(documents, "documents.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("Frequency tables exported successfully\n")

# ----------------------------------------------------------------------------
# 5. DATA CLEANING: EXTRACT ENGLISH SHOP TYPE LABELS
# ----------------------------------------------------------------------------
# Purpose: The LLM sometimes includes English translations in parentheses
# Example: "茶館 (tea house)" - we want to extract "tea house"

cat("\nExtracting English shop type labels...\n")

# First, identify which rows have parentheses
has_parentheses <- grepl("\\(", shops_socdt$Shop_Type)
cat("Rows with English translations:", sum(has_parentheses, na.rm = TRUE), "\n")

# Extract text inside parentheses using regex
# Pattern: .*\\(([^)]+)\\).* means "capture anything inside parentheses"
shops_socdt$Shop_Type_eng <- sub(".*\\(([^)]+)\\).*", "\\1", shops_socdt$Shop_Type)

# Set to NA where there are no parentheses (no English translation available)
shops_socdt$Shop_Type_eng[!has_parentheses] <- NA

cat("English labels extracted\n")

# ----------------------------------------------------------------------------
# 6. DATA CLEANING: CLEAN OTHER_ACTORS COLUMN
# ----------------------------------------------------------------------------
# Purpose: Remove artifacts from LLM extraction, including English annotations
# and extraneous characters

cat("\nCleaning Other_Actors column...\n")

# Step 1: Remove text inside parentheses (including the parentheses themselves)
# This removes English translations like "(employee)" from "夥計 (employee)"
shops_socdt$Other_Actors <- gsub("\\s*\\([^)]*\\)", "", shops_socdt$Other_Actors)

# Step 2: Remove any remaining Latin characters (a-z, A-Z)
# This catches any stray English words that weren't in parentheses
shops_socdt$Other_Actors <- gsub("[a-zA-Z]+", "", shops_socdt$Other_Actors)

# Step 3: Clean up whitespace
# Multiple spaces become single space
shops_socdt$Other_Actors <- gsub("\\s+", " ", shops_socdt$Other_Actors)

# Remove leading and trailing whitespace
shops_socdt$Other_Actors <- trimws(shops_socdt$Other_Actors)

cat("Other_Actors column cleaned\n")

# ----------------------------------------------------------------------------
# 7. DATA SPLITTING: SEPARATE MULTI-VALUED FIELDS
# ----------------------------------------------------------------------------
# Purpose: Some fields contain multiple values separated by commas or semicolons
# Split these into separate columns for easier analysis

# 7.1 Split Institution Field
# ----------------------------------------------------------------------------
# Articles often mention multiple institutions (e.g., "公堂, 縣署, 巡捕房")
# Split these into Institution1, Institution2, Institution3, Institution4, Institution5

cat("\nSplitting Institution field into multiple columns...\n")

shops_socdt <- shops_socdt %>%
  separate(Institution, 
           into = c("Institution1", "Institution2", "Institution3", 
                    "Institution4", "Institution5"),
           sep = "[,;、]",      # Separators: comma, semicolon, Chinese pause mark
           fill = "right",      # If fewer than 5 values, fill right columns with NA
           remove = FALSE,      # Keep original Institution column
           extra = "merge")     # If more than 5 values, merge extras into Institution5

# Clean up whitespace in new institution columns
shops_socdt <- shops_socdt %>%
  mutate(across(starts_with("Institution"), trimws))

cat("Institution field split successfully\n")

# 7.2 Split Shop Field
# ----------------------------------------------------------------------------
# Articles may mention multiple shop names
# Split these into Shop1, Shop2, Shop3, Shop4, Shop5

cat("Splitting Shop field into multiple columns...\n")

shops_socdt <- shops_socdt %>%
  separate(Shop, 
           into = c("Shop1", "Shop2", "Shop3", "Shop4", "Shop5"),
           sep = "[,;、]",      # Separators: comma, semicolon, Chinese pause mark
           fill = "right",      # If fewer than 5 values, fill right columns with NA
           remove = FALSE,      # Keep original Shop column
           extra = "merge")     # If more than 5 values, merge extras into Shop5

# Clean up whitespace in new shop columns
shops_socdt <- shops_socdt %>%
  mutate(across(starts_with("Shop"), trimws))

cat("Shop field split successfully\n")

# 7.3 Split Shop_Type Field (Extended Split for Multi-Type Articles)
# ----------------------------------------------------------------------------
# Some articles mention many different shop types
# Split these into up to 22 columns (st1, st2, ... st22)
# This is for articles with multiple shop types

cat("Splitting Shop_Type field for multi-type articles...\n")

Type_Multi <- Type_Multi %>%
  separate(Shop_Type, 
           into = paste0("st", 1:22),  # Create columns st1 through st22
           sep = "[,;、]",              # Separators: comma, semicolon, Chinese pause mark
           fill = "right",              # Fill remaining columns with NA
           remove = FALSE,              # Keep original Shop_Type column
           extra = "drop")              # If more than 22, drop extras

# Clean up whitespace in new shop type columns
Type_Multi <- Type_Multi %>%
  mutate(across(starts_with("st"), trimws))

cat("Shop_Type field split successfully\n")

# 7.4 Create Long Format for Shop Types
# ----------------------------------------------------------------------------
# Purpose: Convert from wide format (st1, st2, ..., st22) to long format
# This makes it easier to analyze and visualize shop type distributions

cat("Converting Shop_Type to long format...\n")

# Select only DocId and st columns
Type_Multi_Col <- Type_Multi %>% 
  select(DocId, st1, st2, st3, st4, st5, st6, st7, st8, st9, st10, 
         st11, st12, st13, st14, st15, st16, st17, st18, st19, st20, st21, st22)

# Pivot from wide to long format
# Each shop type gets its own row
Type_Multi_Long <- Type_Multi_Col %>%
  pivot_longer(
    cols = starts_with("st"),           # Pivot all st columns
    names_to = "shop_type_number",      # Column name becomes "shop_type_number"
    values_to = "shop_type",            # Values become "shop_type"
    values_drop_na = TRUE               # Remove rows where shop_type is NA
  )

cat("Long format created with", nrow(Type_Multi_Long), "shop type entries\n")

# Export processed multi-type data
write.csv(Type_Multi_Col, "Type_Multi_Col.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(Type_Multi_Long, "Type_Multi_Long.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ----------------------------------------------------------------------------
# 8. FINAL DATA EXPORT
# ----------------------------------------------------------------------------
# Purpose: Save the fully processed dataset

cat("\nExporting final processed dataset...\n")

# Save the complete processed dataset with all new columns
write.csv(shops_socdt, "shops_socdata_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("Final dataset exported as shops_socdata_clean.csv\n")

# Save workspace
save.image('shopsoc.RData')

# ----------------------------------------------------------------------------
# 9. SUMMARY STATISTICS
# ----------------------------------------------------------------------------
# Purpose: Display overview of the processed data

cat("\n=== PROCESSING SUMMARY ===\n")
cat("Total articles analyzed:", nrow(shops_socdt), "\n")
cat("Unique names:", sum(!is.na(name$Name)), "\n")
cat("Unique origins:", sum(!is.na(origin$Origin)), "\n")
cat("Unique locations:", sum(!is.na(location$Location)), "\n")
cat("Unique shop types:", sum(!is.na(shoptype$Shop_Type)), "\n")
cat("Articles with multiple shops:", nrow(shop_multi), "\n")
cat("Articles with multiple shop types:", nrow(Type_Multi), "\n")
cat("Total shop type entries (long format):", nrow(Type_Multi_Long), "\n")
cat("\n=== PROCESSING COMPLETE ===\n")

# ============================================================================
# END OF MAIN SCRIPT
# ============================================================================

# ============================================================================
# OPTIONAL: VERIFICATION AND QUALITY CONTROL PROCEDURES
# ============================================================================
# The following section contains code for verifying LLM extraction completeness
# and cleaning text artifacts. These steps are optional and specific to the
# original research workflow. Users adapting this script may skip this section.
#
# NOTE: This section requires additional files (ShopSoc_ID, shops_ftext_all)
# that may not be available in all workflows. Comment out if not needed.
# ============================================================================

# VERIFICATION SECTION (OPTIONAL)
# ----------------------------------------------------------------------------
# Purpose: Verify which articles were processed by the LLM
# Identify any articles that were missed and need reprocessing
#
# UNCOMMENT THE FOLLOWING CODE BLOCK IF YOU NEED TO VERIFY PROCESSING:
#
# cat("\n=== VERIFICATION: Checking LLM Processing Completeness ===\n")
#
# # Join sociological data with source document IDs
# ShopVerif <- left_join(ShopSoc_ID, documents, by = "DocId")
#
# # Find documents that weren't processed (NA status)
# ShopVerif_left <- ShopVerif %>% 
#   filter(is.na(Status))
#
# cat("Documents not yet processed:", nrow(ShopVerif_left), "\n")
#
# # Get full text for unprocessed documents
# ShopVerif_Text <- left_join(ShopVerif_left, shops_ftext_all, by = "DocId")
# ShopVerif_Text <- unique(ShopVerif_Text)
# ShopVerif_Text <- ShopVerif_Text %>% 
#   select(-Status)
#
# # Export for reprocessing
# write.csv(ShopVerif_Text, "ShopVerif_Text.csv", 
#           row.names = FALSE, fileEncoding = "UTF-8")
#
# cat("Unprocessed documents exported to ShopVerif_Text.csv\n")

# TEXT CLEANING FUNCTION (OPTIONAL)
# ----------------------------------------------------------------------------
# Purpose: Aggressively clean text by removing non-Chinese characters from
# the beginning of each text field. This addresses OCR artifacts that
# sometimes appear at the start of digitized texts.
#
# UNCOMMENT THE FOLLOWING CODE BLOCK IF YOU NEED TO CLEAN TEXT:
#
# cat("\n=== TEXT CLEANING: Removing Leading Non-Chinese Characters ===\n")
#
# # Define aggressive cleaning function
# aggressive_clean_text <- function(text) {
#   # Remove everything that is not a Chinese character from the beginning
#   # Unicode range \u4e00-\u9fa5 covers most common Chinese characters
#   text <- gsub("^[^\u4e00-\u9fa5]+", "", text)
#   return(text)
# }
#
# # Apply to verification texts
# ShopVerif_Text$Text <- sapply(ShopVerif_Text$Text, aggressive_clean_text)
#
# # Apply to full corpus
# shops_ftext_all$Text <- sapply(shops_ftext_all$Text, aggressive_clean_text)
#
# # Save cleaned corpus
# write.csv(shops_ftext_all, "shops_ftext_all_clean.csv", 
#           row.names = FALSE, fileEncoding = "UTF-8")
#
# cat("Text cleaning complete. Each text now starts with first Chinese character.\n")

# ============================================================================
# NOTES FOR USERS
# ============================================================================
#
# 1. DATA STRUCTURE:
#    This script expects sociological data with the following columns:
#    - DocId: Document identifier
#    - Name: Personal names mentioned
#    - Origin: Geographic origins/native places
#    - Shop: Shop names (may contain multiple values separated by commas)
#    - Shop_Type: Business types/categories
#    - Location: Locations mentioned
#    - Institution: Institutions involved (courts, police, guilds, etc.)
#    - Other_Actors: Other people mentioned (employees, customers, etc.)
#
# 2. MULTI-VALUED FIELDS:
#    The script handles fields that contain multiple values by:
#    a. Counting the number of values (using comma counting)
#    b. Splitting into separate columns (Shop1-5, Institution1-5, st1-22)
#    c. Creating long-format versions for easier analysis
#
# 3. DATA CLEANING:
#    The script removes:
#    - English translations in parentheses (optional: can extract them)
#    - Stray Latin characters
#    - Extra whitespace
#    - Leading non-Chinese characters (optional verification section)
#
# 4. OUTPUT FILES:
#    The script generates numerous CSV files:
#    - Frequency tables for each variable (name.csv, origin.csv, etc.)
#    - Processed main dataset (shops_socdata_clean.csv)
#    - Multi-value subsets (shop_multi.csv, Type_Multi.csv)
#    - Long-format data (Type_Multi_Long.csv)
#
# 5. ADAPTING TO OTHER CORPORA:
#    To adapt this script for other LLM-extracted data:
#    - Adjust column names to match your data structure
#    - Modify separators if your data uses different delimiters
#    - Change the number of split columns based on your data's complexity
#    - Update the Unicode range in text cleaning if working with other languages
#
# 6. PERFORMANCE CONSIDERATIONS:
#    - For large datasets (>100,000 rows), consider processing in batches
#    - The separate() function can be slow on very wide splits
#    - Consider using data.table for faster processing of large files
#
# 7. NEXT STEPS:
#    After running this script, you can:
#    - Perform network analysis using shop and actor relationships
#    - Analyze temporal trends by joining with date information
#    - Create visualizations of shop type distributions
#    - Conduct geographic analysis using location data
#    - Build biographies using name and origin information
#
# ============================================================================