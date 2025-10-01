# This is a study that targets "shopkeepers" in the Chinese press, starting with the Shenbao. 
# The constitution of the corpus is based on an initial query with the 店主 term from which the scope of the query was extended with word embeddings


# Upload of required packages 

library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyr)
library(tidyverse)
library(tidytext)

# list all available corpora 
histtext::list_corpora()

# save objects in .RData file
save.image('shopkeep.RData')


# Re-upload saved RData file
load(file = "shopkeep.RData")


  # The list of terms was established with the HistText Word Embeddings function, except for three terms from a Chinese historian and three from ChatGPT 5.

# Search the 16 terms in the corpus, using the function search_documents() : 
shopsSB <- search_documents('"坊主|店主|棧主|莊主|號主|行主|鋪主|館主|店主婦|店東|東家|東主|店家|掌櫃|商戶|業主"', "shunpao-revised")


# Search by individual entries to get a view of results for each
shops2 <- search_documents('"坊主"', "shunpao-revised")
shops3 <- search_documents('"店主"', "shunpao-revised")
shops4 <- search_documents('"棧主"', "shunpao-revised")
shops6 <- search_documents('"莊主"', "shunpao-revised")
shops7 <- search_documents('"號主"', "shunpao-revised")
shops8 <- search_documents('"行主"', "shunpao-revised")
shops9 <- search_documents('"鋪主"', "shunpao-revised")
shops10 <- search_documents('"館主"', "shunpao-revised")
shops11 <- search_documents('"店主婦"', "shunpao-revised")
shops12 <- search_documents('"店東"', "shunpao-revised")
shops13 <- search_documents('"東家"', "shunpao-revised") # From Jiang Jie
shops14 <- search_documents('"東主"', "shunpao-revised") # From Jiang Jie
shops15 <- search_documents('"店家"', "shunpao-revised") # From Jiang Jie
shops16 <- search_documents('"掌櫃"', "shunpao-revised") # From GPT
shops17 <- search_documents('"商戶"', "shunpao-revised") # From JGPT
shops18 <- search_documents('"業主"', "shunpao-revised") # From GPT


# Create unique file for all searches
shopsAll <- bind_rows(shops2, shops3, shops4, shops6, shops7, shops8, shops9, shops10, shops11, shops12, shops13, shops14, shops15, shops16, shops17, shops18)
shopsAll <- unique(shopsAll)
# The initial compilation contained 69,851 articles


shopsAll_Year <- shopsAll %>% group_by(Year) %>% count()
write_csv(shopsAll_Year, "shopsAll_Year.csv")

shopsAll_Year %>% ggplot(aes(Year,n)) + geom_line(color = "orange") + 
  theme_light() +
  labs(title = "Shopkeepers in the Shenbao",
       subtitle = "Number of articles mentioning shopkeepers",
       x = "Year",
       y = "Number of articles")


# Compute results over whole SB corpus
stats_date(shopsAll, "shunpao", to_plot = TRUE, over_all = TRUE, ly = TRUE)



# Plot data by term (Example 1: 店主): count the number of articles on a given term per year and plot the results as a histogram
histtext::count_documents("店主", "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "店主 in the Shenbao",
       subtitle = "Number of articles mentioning 店主",
       x = "Year",
       y = "Number of articles")

# To display the distribution over time of all the terms in a single graph:
# First, aggregate the data for each data frame to count the occurrences of each term per year. 
# Then, combine these counts into a single data frame and plot them.

shops2 <- shops2 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops3 <- shops3 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops4 <- shops4 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops6 <- shops6 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops7 <- shops7 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops8 <- shops8 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops9 <- shops9 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops10 <- shops10 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops11 <- shops11 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops12 <- shops12 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops13 <- shops13 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops14 <- shops14 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops15 <- shops15 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops16 <- shops16 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops17 <- shops17 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 
shops18 <- shops18 %>% mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) 


# Step 1: Aggregate Data by Year for Each Data Frame
shops2_aggregated <- shops2 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops3_aggregated <- shops3 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops4_aggregated <- shops4 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops6_aggregated <- shops6 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops7_aggregated <- shops7 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops8_aggregated <- shops8 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops9_aggregated <- shops9 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops10_aggregated <- shops10 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops11_aggregated <- shops11 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops12_aggregated <- shops12 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops13_aggregated <- shops13 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops14_aggregated <- shops14 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops15_aggregated <- shops15 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops16_aggregated <- shops16 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops17_aggregated <- shops17 %>%
  group_by(Year) %>%
  summarise(Count = n())
shops18_aggregated <- shops18 %>%
  group_by(Year) %>%
  summarise(Count = n())

# Add a new column to each aggregated data frame indicating the term or data frame it represents
shops2_aggregated$Term <- "坊主"
shops3_aggregated$Term <- "店主"
shops4_aggregated$Term <- "棧主"
shops6_aggregated$Term <- "莊主"
shops7_aggregated$Term <- "號主"
shops8_aggregated$Term <- "行主"
shops9_aggregated$Term <- "鋪主"
shops10_aggregated$Term <- "館主"
shops11_aggregated$Term <- "店主婦"
shops12_aggregated$Term <- "店東"
shops13_aggregated$Term <- "東家"
shops14_aggregated$Term <- "東主"
shops15_aggregated$Term <- "店家"
shops16_aggregated$Term <- "掌櫃"
shops17_aggregated$Term <- "商户"
shops18_aggregated$Term <- "業主"


# Step 2: Combine Aggregated Data Frames into one.
combined_data <- bind_rows(shops2_aggregated, shops3_aggregated, shops4_aggregated, shops6_aggregated, shops7_aggregated, shops8_aggregated, shops9_aggregated, shops10_aggregated, shops11_aggregated, shops12_aggregated, shops13_aggregated, shops14_aggregated, shops15_aggregated, shops16_aggregated, shops17_aggregated, shops18_aggregated) 

# Step 3: Plot the combined data.

ggplot(combined_data, aes(x = Year, y = Count, color = Term)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Term Usage Over Time", x = "Year", y = "Document Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# This script aggregates the counts by year for each term, combines these counts, and plots them. 
# The Term column is used to differentiate the lines in the plot.
# Adjust the column names and the term identification as necessary based on your data frame structure and content.

library(RColorBrewer)
# Get colors from a RColorBrewer palette
color_palette <- brewer.pal(n = 15, name = "Set3")

# If you need more than what a single palette offers, you can combine palettes
# or manually specify additional colors
color_palette <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", 
                   "#a65628", "#f781bf", "#999999", "#66c2a5", "#fc8d62", "#8da0cb",
                   "#e78ac3", "#a6d854", "#ffd92f")



# Apply the Color Palette to Your Plot
# Use the scale_color_manual function in ggplot to apply your selected or custom color palette.
ggplot(combined_data, aes(x = Year, y = Count, color = Term)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Term Usage Over Time", x = "Year", y = "Document Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Extract all the occurrences of the different terms in context using the search_concordances() function
shopsConc2 <-search_concordance('"坊主"', corpus = "shunpao-revised", context_size = 120)
shopsConc3 <-search_concordance('"店主"', corpus = "shunpao-revised", context_size = 120)
shopsConc4 <-search_concordance('"棧主"', corpus = "shunpao-revised", context_size = 120)
shopsConc5 <-search_concordance('"舖主"', corpus = "shunpao-revised", context_size = 120)
shopsConc6 <-search_concordance('"莊主"', corpus = "shunpao-revised", context_size = 120)
shopsConc7 <-search_concordance('"號主"', corpus = "shunpao-revised", context_size = 120)
shopsConc8 <-search_concordance('"行主"', corpus = "shunpao-revised", context_size = 120)
shopsConc9 <-search_concordance('"鋪主"', corpus = "shunpao-revised", context_size = 120)
shopsConc10 <-search_concordance('"館主"', corpus = "shunpao-revised", context_size = 120)
shopsConc11 <-search_concordance('"店主婦"', corpus = "shunpao-revised", context_size = 120)
shopsConc12 <-search_concordance('"店東"', corpus = "shunpao-revised", context_size = 120)
shopsConc13 <-search_concordance('"東家"', corpus = "shunpao-revised", context_size = 120) 
shopsConc14 <-search_concordance('"東主"', corpus = "shunpao-revised", context_size = 120) 
shopsConc15 <-search_concordance('"店家"', corpus = "shunpao-revised", context_size = 120)
shopsConc16 <-search_concordance('"掌櫃"', corpus = "shunpao-revised", context_size = 120)
shopsConc17 <-search_concordance('"商户"', corpus = "shunpao-revised", context_size = 120)
shopsConc18 <-search_concordance('"業主"', corpus = "shunpao-revised", context_size = 120)



# Bind all concordance files by binding rows
shopsConc <- bind_rows(shopsConc2, shopsConc3, shopsConc4, shopsConc5, shopsConc6, shopsConc7, shopsConc8, shopsConc9, shopsConc10, shopsConc11, shopsConc12, shopsConc13, shopsConc14, shopsConc15, shopsConc16, shopsConc17, shopsConc18)
shopsConc <- unique(shopsConc)

# Remove false positives in Matched
shopsConc <- shopsConc %>%
  filter(!str_detect(Matched, "行。|行·　|行）|行」|店、|作、|業。|行、|東、|作，主|作。|作·|作（|作“|作㈢|作「|作〕|作，|作：|作）|作　，"))
# Merge and create text column
shopsConcWrk <- shopsConc %>% 
  mutate(TextConc = paste(Before, Matched, After, sep = ""))
# Remove concordance columns
shopsConc <- shopsConcWrk %>% select(-Before, -Matched, - After, -Source, -Title, -Date)
# Add article length
shopsConc <- shopsConc %>% mutate(Size = (nchar(TextConc)))

# Save results file
write_csv(shopsConc, "shopsConc.csv")
# The results include 107,287 occurrences, which means that there were more than one mention of the searched terms in the documents


# Retrieve the full text for each term using the function get_documents()
shops_ftext <- histtext::get_documents(shopsAll, "shunpao-revised")
# Add article length
shops_ftext <- shops_ftext %>% mutate(Length = (nchar(Text)))
# Remove Source column
shops_ftext <- shops_ftext %>% select(-Source)
# Remove empty text rows
shops_ftext <- shops_ftext %>% filter(!is.na(Text))
# Remove short text rows
shops_ftext <- shops_ftext %>% filter(Length > 7)
# Remove extra long text rows
shops_ftext <- shops_ftext %>% filter(Length < 23336)
# After filtering, the number of rows in shops_ftext is 69591

# Join of the concordance file and the full text file
# This data frame brings together the full text and the extracted concordance text
# Useful to easily identify the extracted text in the long documents
shops_concfull <- inner_join(shops_ftext, shopsConc, by = "DocId")

# Select sub-corpus of articles with more than 400 characters
shops_ftext401 <- shops_ftext %>% filter(Length > 400)
# Select first 20 rows
shops_ftext20r <- shops_ftext401 %>% slice(1:20)

# Select sub-corpus of articles with less than 400 characters
shops_ftext400 <- shops_ftext %>% filter(Length < 401)
# The shops_ftext400 subcorpus can be used to develop the research workflow
# The shops_ftext400 subcorpus contains articles that are by and large unique articles

# Tokenize the documents
shops_400ft_tok <- cws_on_df(shops_ftext400, 
                             text_column = "Text",
                             id_column = "DocId",
                             model = "trftc_shunpao_23:zh:cws",
                             detailed_output = FALSE,
                             token_separator = " ",
                             verbose = TRUE
)

write_csv(shops_400ft_tok, "shops_400ft_tok.csv")


# Save results files
write_csv(shopsAll, "shopsAll.csv")
write_csv(shopsConc, "shopsConc.csv")
write_csv(shops_concfull, "shops_concfull.csv")
write_csv(shops_ftext, "shops_ftext.csv")
write_csv(shops_ftext400, "shops_ftext400.csv")
write_csv(shops_ftext401, "shops_ftext401.csv")
write_csv(combined_data, "combined_data.csv")

# save objects in .RData file
save.image('shopkeep.RData')
