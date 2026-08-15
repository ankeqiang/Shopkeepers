# ============================================================================
# SHOPKEEPERS IN THE SHENBAO: TOPIC MODELING ANALYSIS
# ============================================================================
# This script performs Structural Topic Modeling (STM) on a corpus of articles
# about shopkeepers from the Shenbao newspaper (1872-1949). 
#
# Topic modeling is an unsupervised machine learning technique that discovers
# abstract "topics" that occur in a collection of documents. Each topic is
# represented by a set of words that frequently co-occur together.
#
# The script uses the Structural Topic Model (STM) which allows:
#   - Incorporation of document metadata (here: Year)
#   - Modeling how topic prevalence changes over time
#   - Better interpretability than standard LDA models
#
# Main workflow:
#   1. Data preparation and cleaning
#   2. Stopword removal (general and domain-specific)
#   3. Text preprocessing for STM
#   4. Model selection (comparing 5-10 topics)
#   5. Model estimation and interpretation
#   6. Temporal analysis of topic prevalence
#   7. Interactive visualization
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------
# Load required packages for topic modeling and visualization

library(histtext)      # For historical Chinese text processing
library(tidyverse)     # For data manipulation and visualization
library(stm)           # Structural Topic Model package
library(stminsights)   # Interactive visualization for STM results

# Save workspace for recovery and continuation
# This creates a snapshot of all objects currently in memory
save.image('shoptm.RData')

# To restore this workspace later, use:
load(file = "shoptm.RData")

# ============================================================================
# PART 1: DATA PREPARATION AND CLEANING
# ============================================================================

# ----------------------------------------------------------------------------
# 2. INITIAL DATA PREPARATION
# ----------------------------------------------------------------------------
# Start with the tokenized and cleaned shopkeeper corpus from previous script
# This assumes you have shop_bind_tok2 from the corpus construction script

shoptok <- shop_bind_tok2

# deduplicate based on docId
shoptok <- shop_bind_tok2 %>%
  mutate(base_id = str_remove(DocId, "_\\d+$")) %>%
  distinct(base_id, .keep_all = TRUE) %>%
  select(-base_id)

# Basic text cleaning to remove leading noise and normalize spacing
shoptok <- shoptok %>%
  mutate(
    Text = str_replace_all(Text, "^[[:punct:][:space:]]+", ""),  # Remove leading punctuation/spaces
    Text = str_squish(Text)                                       # Normalize internal spacing
  )

# ----------------------------------------------------------------------------
# 3. REMOVING PROBLEMATIC DOCUMENTS
# ----------------------------------------------------------------------------
# Some documents may contain errors, be too short, or have other quality issues
# These specific DocIds were identified during preliminary analysis

docs_to_remove <- c(
  "SPSP193104241001",      # Problematic document 1
  "SPSP194706280401",      # Problematic document 2
  "SPSP194505030201",      # Problematic document 3
  "SPSP193907061701_02"    # Problematic document 4
)

# Filter out the problematic documents
shoptok <- shoptok %>%
  filter(!DocId %in% docs_to_remove)

# ----------------------------------------------------------------------------
# 4. EXTRACTING TEMPORAL INFORMATION
# ----------------------------------------------------------------------------
# Extract year from DocId for temporal analysis
# DocId format: "SPSP19310424..." where positions 5-8 contain the year

shoptok <- shoptok %>%
  mutate(Year = as.numeric(substr(DocId, 5, 8)))


# Save progress
save.image('shoptm.RData')

# ----------------------------------------------------------------------------
# 5. ADDITIONAL TEXT CLEANING
# ----------------------------------------------------------------------------
# Remove texts without Chinese characters (likely OCR errors)
shoptok_filt <- shoptok %>% 
  filter(stringr::str_detect(Text, "[\\p{Han}]"))

# Extract year information for later use
shoptok_year <- shoptok %>% 
  select(DocId, Year)

# Remove any remaining excess whitespace
shoptok_filt$Text <- str_squish(shoptok_filt$Text)

# Export cleaned data
write_csv(shoptok, "shoptok.csv")
write_csv(shoptok_filt, "shoptok_filt.csv")

# ----------------------------------------------------------------------------
# 6. PREPARING FINAL DATASET WITH METADATA
# ----------------------------------------------------------------------------
# Join tokenized text with metadata (DocId and Year)
shop_doc <- shoptok_filt %>% 
  select(DocId, Year)

# Inner join to ensure all documents have complete metadata
shoptok2 <- inner_join(shop_doc, shoptok_filt)

# Remove any duplicates that may have been introduced
shoptok2 <- unique(shoptok2)

# ----------------------------------------------------------------------------
# 7. DOMAIN-SPECIFIC TEXT CLEANING
# ----------------------------------------------------------------------------
# Remove common terms that don't contribute to topic differentiation

shoptok3 <- shoptok2 %>%
  mutate(Text = str_remove_all(Text, "先生")) %>%      # Remove "Mr./sir"
  mutate(Text = str_remove_all(Text, "昨日")) %>%      # Remove "yesterday"
  mutate(Text = str_remove_all(Text, "無名氏")) %>%    # Remove "anonymous"
  # Remove Chinese numerals that appear as standalone terms
  mutate(Text = str_remove_all(Text, "[零一二三四五六七八九十百千]+")) %>%
  # Normalize institutional terms to their base forms
  mutate(Text = str_replace_all(Text, "本公司", "公司")) %>%   # "our company" → "company"
  mutate(Text = str_replace_all(Text, "本銀行", "銀行"))       # "our bank" → "bank"

# ============================================================================
# PART 2: STOPWORD REMOVAL
# ============================================================================

# ----------------------------------------------------------------------------
# 8. COMPREHENSIVE CHINESE STOPWORD LIST
# ----------------------------------------------------------------------------
# Stopwords are high-frequency words that carry little semantic meaning
# Removing them improves topic model quality and interpretability
# This list includes:
#   - Classical Chinese function words (之, 與, 為, etc.)
#   - Modern Chinese particles and conjunctions
#   - Discourse markers and interjections
#   - Place names that appear frequently in newspapers (滬甯, 京奉, etc.)

chinese_stopwords <- c(
  # Classical Chinese function words and particles
  "之", "與", "為", "也", "有", "在", "以", "於", "即", "係", "爲",
  "經方", "謹上", "滬甯", "京奉", "匯豐", "匯理", "上月", "一日",
  "往來", "云云", "而已", "如是", "欽此", "十", "三", "二", "一",
  "四", "五", "六", "七", "向", "八", "九", "前報", "昨報", "昨日",
  "前日", "於是", "所謂", "以來", "如此", "又此", "等", "卽", "亦",
  "又", "各", "至", "其", "來", "前", "該", "因", "謂", "無", "及",
  "由", "稱", "飭", "已", "據", "仍", "皆", "再", "去", "將", "到",
  "旣", "欲", "曾", "矣", "均", "日", "可", "爾", "本", "或", "從",
  "兩", "内", "并", "某", "未", "如", "此", "巳", "聞", "經",
  "爲此", "行", "用", "以致", "令", "惟", "下", "所有", "呈", "而",
  "是", "不", "小", "人", "的", "被", "我", "著", "請", "者", "已",
  "後", "氏", "上", "尙", "奏", "稟", "一併", "一百", "欵", "雖",
  "昨", "出", "自", "共", "以上", "前來", "啓", "則", "開", "明",
  "詎", "之下", "何處",
  
  # Extended stopwords (Modern and Traditional Chinese)
  "按", "按照", "俺", "俺們", "阿", "別", "別人", "別處", "別是",
  "別的", "別管", "別說", "不僅", "不但", "不光", "不單", "不只",
  "不外乎", "不如", "不妨", "不盡", "不盡然", "不得", "不怕", "不惟",
  "不成", "不拘", "不料", "不是", "不比", "不然", "不特", "不獨",
  "不管", "不至於", "不若", "不論", "不過", "不問", "比方", "比如",
  "比及", "比", "本身", "本著", "本地", "本人", "巴巴", "巴", "並",
  "並且", "非", "彼", "彼時", "彼此", "便於", "把", "邊", "鄙人",
  "罷了", "被", "般的", "此間", "此次", "此時", "此外", "此處",
  "此地", "才", "才能", "朝", "朝著", "從", "從此", "從而", "除非",
  "除此之外", "除開", "除外", "除了", "除", "誠然", "誠如", "出來",
  "出於", "曾", "趁著", "趁", "處在", "乘", "衝", "等等", "等到",
  "第", "當著", "當然", "當地", "當", "多", "多麼", "多少", "對",
  "對於", "對待", "對方", "對比", "得", "得了", "打", "打從", "的確",
  "的話", "但", "但凡", "但是", "大家", "大", "地", "待", "都",
  "叮咚", "而言", "而是", "而外", "而後", "而況", "而且", "爾爾",
  "爾後", "爾", "二來", "非獨", "非特", "非徒", "非但", "否則",
  "反過來說", "反過來", "反而", "反之", "分別", "凡是", "凡", "個",
  "個別", "固然", "故", "故此", "故而", "果然", "果真", "各個",
  "各位", "各種", "各自", "關於", "具體地說", "歸齊", "歸", "根據",
  "管", "趕", "跟", "過", "該", "給", "光是", "或者", "或曰", "或是",
  "或則", "何", "何以", "何況", "何處", "何時", "還要", "還有",
  "還是", "還", "後者", "很", "換言之", "換句話說", "好", "後", "和",
  "即令", "即使", "即便", "即如", "即或", "即若", "繼而", "繼後",
  "繼之", "既然", "既是", "既往", "既", "盡管如此", "盡管", "盡",
  "就要", "就算", "就是說", "就是了", "就是", "就", "據", "據此",
  "接著", "經", "經過", "結果", "及", "及其", "及至", "加以", "加之",
  "例如", "介於", "幾時", "幾", "截至", "極了", "簡言之", "竟而",
  "緊接著", "距", "較之", "較", "進而", "鑒於", "基於", "具體說來",
  "兼之", "借", "儻然", "今", "叫", "將", "可以", "可是", "可見",
  "開始", "開外", "況且", "靠", "看", "來說", "來自", "來著", "來",
  "兩者", "臨", "類如", "論", "賴以", "連", "連同", "離", "莫若",
  "莫如", "莫不然", "假使", "假如", "假若", "某", "某個", "某些",
  "某某", "漫說", "沒奈何", "每當", "每", "慢說", "冒", "哪個",
  "哪些", "哪兒", "哪天", "哪年", "哪怕", "哪樣", "哪邊", "哪裡",
  "那裡", "那邊", "那般", "那樣", "那時", "那兒", "那會兒", "那些",
  "那麼樣", "那麼些", "那麼", "那個", "那", "乃", "乃至", "乃至於",
  "寧肯", "寧願", "寧可", "寧", "能", "能否", "你", "你們", "您",
  "拿", "難道說", "內", "哪", "憑藉", "憑", "旁人", "譬如", "譬喻",
  "且", "且不說", "且說", "其", "其一", "其中", "其二", "其他",
  "其餘", "其它", "其次", "前後", "前此", "前者", "起見", "起",
  "全部", "全體", "恰恰相反", "豈但", "卻", "若非", "若果", "若是",
  "若夫", "若", "另", "另一方面", "另外", "另悉", "如若", "如此",
  "如果", "如同", "如其", "如何", "如下", "如上所述", "如上", "如",
  "然則", "然後", "然而", "任", "任何", "任憑", "仍", "仍舊", "人家",
  "人們", "讓", "甚至於", "甚至", "甚而", "甚或", "甚麼", "甚且",
  "什麼", "什麼樣", "上下", "雖說", "雖然", "雖則", "雖", "孰知",
  "孰料", "始而", "所", "所以", "所在", "所幸", "所有", "是以",
  "是的", "設使", "設或", "設若", "誰", "誰人", "誰料", "誰知",
  "隨著", "隨時", "隨後", "隨", "順著", "順", "受到", "使得", "使",
  "似的", "尚且", "庶幾", "庶乎", "時候", "省得", "說來", "首先",
  "倘", "倘使", "倘或", "倘然", "倘若", "同", "同時", "他", "他人",
  "他們", "們", "她們", "她", "它們", "它", "替代", "替", "通過",
  "騰", "這裡", "這邊", "這般", "這次", "這樣", "這時", "這就是說",
  "這兒", "這會兒", "這些", "這麼點兒", "這麼樣", "這麼些", "這麼",
  "這個", "這一來", "這", "正是", "正巧", "正如", "正值", "萬一",
  "為", "為了", "為什麼", "為何", "為止", "為此", "為著", "無論",
  "無寧", "無", "我們", "往", "望", "惟其", "唯有", "向著", "向使",
  "先不先", "相對而言", "許多", "像", "些", "一何", "一切", "一則",
  "一方面", "一旦", "一來", "一樣", "一般", "一轉眼", "由此可見",
  "由此", "由是", "由於", "用來", "因而", "因著", "因此", "因了",
  "因為", "因", "要是", "要麼", "要不然", "要不是", "要不", "要",
  "與", "與其", "與其說", "與否", "與此同時", "以為", "以便", "以免",
  "以及", "以故", "以期", "以至", "以至於", "以致", "己", "已矣",
  "有些", "有關", "有及", "有時", "有的", "沿", "沿著", "於", "於是",
  "於是乎", "雲爾", "依照", "依據", "依", "餘外", "也罷", "也好",
  "又及", "抑或", "猶自", "猶且", "用", "越是", "只當", "只怕",
  "只是", "只有", "只消", "只要", "只限", "再其次", "再則", "再有",
  "再者", "再者說", "再說", "自身", "自打", "自己", "自家", "自後",
  "自各兒", "自從", "自個兒", "怎樣", "怎奈", "怎麼樣", "怎麼辦",
  "怎麼", "怎", "至若", "至今", "至於", "縱然", "縱使", "縱令", "縱",
  "之一", "之所以", "之類", "著呢", "眨眼", "總而言之", "總的說來",
  "總的來說", "總的來看", "總之", "在於", "在下", "諸", "諸位",
  "諸如", "咱們", "咱", "作為", "只", "最", "照著", "照", "直到",
  "綜上所述", "賊死", "逐步", "遵照", "遵循", "針對", "致", "者",
  "則甚", "則", 
  
  # Interjections and exclamations
  "咳", "哇", "哈", "哈哈", "哉", "哎", "哎呀", "哎喲",
  "嘩", "喲", "哦", "哩", "矣哉", "矣乎", "焉", "毋寧", "歟", "嘿嘿",
  "嘿", "嘻", "嘛", "噓", "嘎登", "嘎", "噯", "嗯", "嗬", "嗡嗡",
  "嗡", "嘍", "喔唷", "喏", "喂", "啷當", "啪噠", "啦", "啥", "啐",
  "啊", "唉", "哼唷", "哼", "咧", "咦", "咚", "咋", "呼哧", "呸",
  "呵呵", "呵", "呢", "嗚呼", "嗚", "唄", "嘔", "呃", "呀", "吱",
  "吧噠", "吧", "嗎", "嚇", "兮", "兒", "了", "乎"
)

# Create a regex pattern from the stopwords for efficient removal
stop_pattern <- paste(chinese_stopwords, collapse = "|")

# Apply stopword removal
shoptok3 <- shoptok3 %>% 
  mutate(Text = str_remove_all(Text, stop_pattern))

# ----------------------------------------------------------------------------
# 9. REMOVING SEARCH TERMS
# ----------------------------------------------------------------------------
# Remove the original shopkeeper terms used to construct the corpus
# These terms appear in ALL documents and don't help differentiate topics
# Also remove additional common location/quantity terms

shoptok3 <- shoptok3 %>% 
  mutate(Text = str_remove_all(
    Text,
    "坊主|店主|棧主|莊主|號主|行主|鋪主|館主|店主婦|店東|東家|東主|店家|掌櫃|商戶|業主|○昨|○初|附近|左近|千百|〇"
  ))

# Save the cleaned dataset for topic modeling
shopTM <- shoptok3
write_csv(shopTM, "shopTM.csv")

# ----------------------------------------------------------------------------
# 10. CREATING PERIOD DIVISIONS (OPTIONAL)
# ----------------------------------------------------------------------------
# Alternative periodization based on different historical criteria
# This shows a different approach to dividing the corpus temporally

# Extract year for filtering
shoptok3$year <- as.numeric(str_sub(shoptok3$DocId, 5, 8))

# Alternative period divisions:
shops_tokp1 <- shopTM %>% filter(Year < 1885)                # Early years
shops_tokp2 <- shopTM %>% filter(Year > 1884 & Year < 1903) # Late Qing
shops_tokp3 <- shopTM %>% filter(Year > 1902 & Year < 1917) # Early Republic
shops_tokp4 <- shopTM %>% filter(Year > 1916 & Year < 1933) # Republican/Warlord
shops_tokp5 <- shopTM %>% filter(Year > 1933)                # War period

# Save progress
save.image('shoptm.RData')

# ============================================================================
# PART 3: TOPIC MODEL PREPARATION AND ESTIMATION
# ============================================================================

# ----------------------------------------------------------------------------
# 11. PREPARING METADATA
# ----------------------------------------------------------------------------
# Extract metadata that will be used in the topic model
# Here we use Year as a covariate to model temporal changes in topics

meta <- shopTM %>% 
  transmute(DocId, Year)

# ----------------------------------------------------------------------------
# 12. TEXT PREPROCESSING FOR STM
# ----------------------------------------------------------------------------
# The textProcessor function prepares texts for topic modeling by:
#   - Tokenizing (already done, so we work with pre-tokenized text)
#   - Converting to lowercase (not applicable for Chinese)
#   - Removing very short words (< 2 characters)
#   - Creating document-term matrix
#
# Important parameters:
#   - stem = FALSE: Don't apply stemming (not meaningful for Chinese)
#   - wordLengths = c(2, Inf): Keep words with 2+ characters
#   - customstopwords: Additional stopwords to remove

corpus <- stm::textProcessor(
  shopTM$Text,
  metadata = meta,
  stem = FALSE,                    # No stemming for Chinese
  wordLengths = c(2, Inf),         # Keep words ≥2 characters
  verbose = FALSE,
  customstopwords = stop_all$word  # Additional custom stopwords
)

# Extract vocabulary for inspection
words <- as_tibble(corpus$vocab)

# ----------------------------------------------------------------------------
# 13. EXPLORING WORD FREQUENCY THRESHOLDS
# ----------------------------------------------------------------------------
# Visualize how many terms would be removed at different frequency thresholds
# This helps decide on an appropriate lower threshold
# Terms appearing in very few documents may be noise or OCR errors

stm::plotRemoved(corpus$documents, lower.thresh = c(0, 10, by=5))

# ----------------------------------------------------------------------------
# 14. PREPARING DOCUMENTS FOR MODELING
# ----------------------------------------------------------------------------
# The prepDocuments function:
#   - Removes very rare terms (appearing in < lower.thresh documents)
#   - Removes documents with no remaining words
#   - Creates the final document-term matrix

out <- stm::prepDocuments(
  corpus$documents,
  corpus$vocab,
  corpus$meta,
  lower.thresh = 2    # Remove words appearing in fewer than 2 documents
)

# Results from prepDocuments:
# - Removed 659,386 of 794,733 terms (748,656 of 4,455,663 tokens)
# - Removed 87 documents with no words after filtering
# - Final corpus: 82,896 documents, 135,347 terms, 3,707,007 tokens

# Inspect what was removed
wordsremoved <- as_tibble(out$words.removed)
docremoved <- as_tibble(out$docs.removed)

# ============================================================================
# PART 4: MODEL SELECTION AND DIAGNOSTICS
# ============================================================================

# ----------------------------------------------------------------------------
# 15. FIXING DATA ALIGNMENT ISSUES
# ----------------------------------------------------------------------------
# Sometimes there's a mismatch between documents and metadata
# This section diagnoses and fixes any alignment problems

# Check dimensions
cat("Documents:", length(out$documents), "\n")
cat("Meta rows:", nrow(out$meta), "\n")
cat("Vocab length:", length(out$vocab), "\n")

# Fix alignment if needed - keep only matching indices
valid_indices <- seq_len(min(length(out$documents), nrow(out$meta)))
out$documents <- out$documents[valid_indices]
out$meta <- out$meta[valid_indices, ]

# Verify the fix
cat("After fix - Documents:", length(out$documents), "\n")
cat("After fix - Meta rows:", nrow(out$meta), "\n")

# Check for NA values in Year column
cat("NAs in Year column:", sum(is.na(out$meta$Year)), "\n")

# Examine Year structure
str(out$meta$Year)
table(out$meta$Year, useNA="always")

# Remove documents with NA years if present
if(sum(is.na(out$meta$Year)) > 0) {
  valid_rows <- !is.na(out$meta$Year)
  out$documents <- out$documents[valid_rows]
  out$meta <- out$meta[valid_rows, ]
  
  cat("After removing NAs:\n")
  cat("Documents:", length(out$documents), "\n")
  cat("Meta rows:", nrow(out$meta), "\n")
  cat("NAs in Year:", sum(is.na(out$meta$Year)), "\n")
}

# ----------------------------------------------------------------------------
# 16. MODEL SELECTION: COMPARING DIFFERENT NUMBERS OF TOPICS
# ----------------------------------------------------------------------------
# The searchK function fits models with different numbers of topics (K)
# and compares them using multiple diagnostic measures:
#
# - Held-Out Likelihood: Higher is better (model fits held-out data well)
# - Residuals: Lower is better (model fits observed data well)
# - Semantic Coherence: Higher is better (words in topics co-occur more)
# - Lower Bound: Higher is better (overall model fit)
#
# We test models from 5 to 10 topics to find the optimal number

set.seed(1111)  # Set seed for reproducibility
K <- seq(5, 10, by=1)  # Test 5, 6, 7, 8, 9, 10 topics

kresult <- searchK(
  out$documents,
  out$vocab,
  K,
  data = out$meta,
  prevalence = ~ Year,  # Model how topic prevalence changes with Year
  verbose = FALSE
)

# Plot diagnostic results for all models
plot(kresult)

# Save progress
save.image('shoptm.RData')


# ============================================================================
# PART 5: ESTIMATING TOPIC MODELS
# ============================================================================

# ----------------------------------------------------------------------------
# 18. ESTIMATING MODELS WITH DIFFERENT TOPIC COUNTS
# ----------------------------------------------------------------------------
# Estimate four models with different numbers of topics
# The prevalence formula (~ Year) models how topic proportions change over time

# 5-topic model
mod.5 <- stm::stm(
  out$documents,
  out$vocab,
  K = 5,
  data = out$meta,
  prevalence = ~ Year,  # Topic proportions vary by year
  verbose = FALSE
)

# 6-topic model (recommended by diagnostics)
mod.6 <- stm::stm(
  out$documents,
  out$vocab,
  K = 6,
  data = out$meta,
  prevalence = ~ Year,
  verbose = FALSE
)

# 7-topic model
mod.7 <- stm::stm(
  out$documents,
  out$vocab,
  K = 7,
  data = out$meta,
  prevalence = ~ Year,
  verbose = FALSE
)

# 10-topic model
mod.10 <- stm::stm(
  out$documents,
  out$vocab,
  K = 10,
  data = out$meta,
  prevalence = ~ Year,
  verbose = FALSE
)

# ----------------------------------------------------------------------------
# 19. ESTIMATING TEMPORAL EFFECTS
# ----------------------------------------------------------------------------
# The estimateEffect function quantifies how topic prevalence changes with Year
# This allows us to test whether topics significantly increase or decrease over time

Year5 <- stm::estimateEffect(1:5 ~ Year, mod.5, meta = out$meta)
Year6 <- stm::estimateEffect(1:6 ~ Year, mod.6, meta = out$meta)
Year7 <- stm::estimateEffect(1:7 ~ Year, mod.7, meta = out$meta)
Year10 <- stm::estimateEffect(1:10 ~ Year, mod.10, meta = out$meta)

# Save all models
save.image('shoptm.RData')

# ============================================================================
# PART 6: MODEL EXPLORATION AND INTERPRETATION
# ============================================================================

# ----------------------------------------------------------------------------
# 20. EXTRACTING TOPIC PROPORTIONS
# ----------------------------------------------------------------------------
# Extract the proportion of each topic in each document
# This creates a document-topic matrix showing topic distributions

topicprop5 <- make.dt(mod.5, meta)
topicprop6 <- make.dt(mod.6, meta)
topicprop7 <- make.dt(mod.7, meta)
topicprop10 <- make.dt(mod.10, meta)

# ----------------------------------------------------------------------------
# 21. VISUALIZING TOPIC PROPORTIONS IN CORPUS
# ----------------------------------------------------------------------------
# Histogram showing the distribution of topic proportions across all documents
# This reveals which topics are more prevalent overall

plot.STM(mod.5, "hist")   # 5-topic model
plot.STM(mod.6, "hist")   # 6-topic model
plot.STM(mod.7, "hist")   # 7-topic model
plot.STM(mod.10, "hist")  # 10-topic model

# ----------------------------------------------------------------------------
# 22. TOPIC SUMMARIES
# ----------------------------------------------------------------------------
# Display the most important topics and their top words
# n parameter controls how many top words to show per topic

plot.STM(mod.5, "summary", n=5)   # Top 5 words for each topic
plot.STM(mod.6, "summary", n=6)
plot.STM(mod.7, "summary", n=7)
plot.STM(mod.10, "summary", n=10)

# ----------------------------------------------------------------------------
# 23. DETAILED TOPIC LABELING
# ----------------------------------------------------------------------------
# The labelTopics function shows multiple word lists for each topic:
#   - Highest Prob: Words with highest probability in this topic
#   - FREX: Words that are both frequent and exclusive to this topic
#   - Lift: Words that appear much more in this topic than in others
#   - Score: Weighted combination of probability and exclusivity
#
# FREX and Score are often most useful for interpretation

labelTopics(mod.5, n=10)   # Show top 10 words per topic
labelTopics(mod.6, n=10)
labelTopics(mod.7, n=10)
labelTopics(mod.10, n=10)

# ----------------------------------------------------------------------------
# 24. WORD CLOUDS FOR VISUAL INTERPRETATION
# ----------------------------------------------------------------------------
# Word clouds provide visual representation of topic word distributions
# Size indicates word importance in the topic

# 5-topic model word clouds
cloud(mod.5, topic = 1, scale = c(4, 0.4))
cloud(mod.5, topic = 2, scale = c(4, 0.4))
cloud(mod.5, topic = 3, scale = c(4, 0.4))
cloud(mod.5, topic = 4, scale = c(4, 0.4))
cloud(mod.5, topic = 5, scale = c(4, 0.4))

# 7-topic model word clouds
cloud(mod.7, topic = 1, scale = c(4, 0.4))
cloud(mod.7, topic = 2, scale = c(4, 0.4))
cloud(mod.7, topic = 3, scale = c(4, 0.4))
cloud(mod.7, topic = 4, scale = c(4, 0.4))
cloud(mod.7, topic = 5, scale = c(4, 0.4))
cloud(mod.7, topic = 6, scale = c(5, 0.6))

# 10-topic model word clouds
cloud(mod.10, topic = 1, scale = c(4, 0.4))
cloud(mod.10, topic = 2, scale = c(4, 0.4))
cloud(mod.10, topic = 3, scale = c(4, 0.4))
cloud(mod.10, topic = 4, scale = c(4, 0.4))
cloud(mod.10, topic = 5, scale = c(4, 0.4))
cloud(mod.10, topic = 6, scale = c(4, 0.4))

# ----------------------------------------------------------------------------
# 25. FINDING REPRESENTATIVE DOCUMENTS
# ----------------------------------------------------------------------------
# The findThoughts function identifies documents that are most representative
# of each topic (highest proportion of that topic)
# This helps interpret what each topic is actually about

# 5-topic model examples
T5_thoughts1 <- findThoughts(mod.5, texts=shopTM$Text, topics=1, n=5)$docs[[1]]
T5_thoughts2 <- findThoughts(mod.5, texts=shopTM$Text, topics=2, n=5)$docs[[1]]
T5_thoughts3 <- findThoughts(mod.5, texts=shopTM$Text, topics=3, n=5)$docs[[1]]

# 10-topic model examples
T10_thoughts1 <- findThoughts(mod.10, texts=shopTM$Text, topics=1, n=5)$docs[[1]]
T10_thoughts2 <- findThoughts(mod.10, texts=shopTM$Text, topics=2, n=5)$docs[[1]]
T10_thoughts3 <- findThoughts(mod.10, texts=shopTM$Text, topics=3, n=5)$docs[[1]]
T10_thoughts4 <- findThoughts(mod.10, texts=shopTM$Text, topics=4, n=5)$docs[[1]]

# Plot representative quotes for selected topics
par(mfrow=c(1,3), mar=c(1,1,2,2))
plotQuote(T5_thoughts1, width=50, maxwidth=400, text.cex=0.8, main="Topic 1")
plotQuote(T5_thoughts2, width=50, maxwidth=400, text.cex=0.8, main="Topic 2")
plotQuote(T5_thoughts3, width=50, maxwidth=400, text.cex=0.8, main="Topic 3")
par(mfrow=c(1, 1))  # Reset to single plot

# ============================================================================
# PART 7: TEMPORAL ANALYSIS OF TOPICS
# ============================================================================

# ----------------------------------------------------------------------------
# 26. PREPARING DATA FOR TEMPORAL VISUALIZATION
# ----------------------------------------------------------------------------
# Extract just the topic proportion columns (removing DocId)

topic5prop <- topicprop5 %>% select(c(2:6))      # Topics 1-5
topic6prop <- topicprop6 %>% select(c(2:7))      # Topics 1-6
topic7prop <- topicprop7 %>% select(c(2:8))      # Topics 1-7
topic10prop <- topicprop10 %>% select(c(2:11))   # Topics 1-10

# ----------------------------------------------------------------------------
# 27. COMPUTING YEARLY TOPIC PROPORTIONS
# ----------------------------------------------------------------------------
# Calculate the average proportion of each topic per year
# This shows how topics wax and wane over the newspaper's history

topic_proportion_per_year5 <- aggregate(
  topic5prop,
  by = list(Year = meta$Year),
  mean
)

topic_proportion_per_year6 <- aggregate(
  topic6prop,
  by = list(Year = meta$Year),
  mean
)

topic_proportion_per_year7 <- aggregate(
  topic7prop,
  by = list(Year = meta$Year),
  mean
)

topic_proportion_per_year10 <- aggregate(
  topic10prop,
  by = list(Year = meta$Year),
  mean
)

# Export temporal data for external analysis
write_csv(topic_proportion_per_year5, "topic_proportion_per_year5.csv")
write_csv(topic_proportion_per_year6, "topic_proportion_per_year6.csv")
write_csv(topic_proportion_per_year7, "topic_proportion_per_year7.csv")
write_csv(topic_proportion_per_year10, "topic_proportion_per_year10.csv")

# Export document-level topic proportions
write_csv(topic5prop, "topic5prop.csv")
write_csv(topic6prop, "topic6prop.csv")
write_csv(topic7prop, "topic7prop.csv")
write_csv(topic10prop, "topic10prop.csv")

# ----------------------------------------------------------------------------
# 28. RESHAPING DATA FOR VISUALIZATION
# ----------------------------------------------------------------------------
# Convert from wide format (one column per topic) to long format
# (one row per year-topic combination) for ggplot2

library(reshape)

vizDataFrame5y <- melt(topic_proportion_per_year5, id.vars = "Year")
vizDataFrame6y <- melt(topic_proportion_per_year6, id.vars = "Year")
vizDataFrame7y <- melt(topic_proportion_per_year7, id.vars = "Year")
vizDataFrame10y <- melt(topic_proportion_per_year10, id.vars = "Year")

# ----------------------------------------------------------------------------
# 29. VISUALIZING TEMPORAL TRENDS: STACKED BAR CHARTS
# ----------------------------------------------------------------------------
# Stacked bar charts show the changing composition of topics over time
# Each year's bar shows the relative proportion of all topics

library(pals)  # For color palettes

# 5-topic model temporal visualization
ggplot(vizDataFrame5y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") +
  ylab("proportion") + 
  scale_fill_manual(
    values = paste0(alphabet(20), "FF"),  # Color palette with full opacity
    name = "Topic"
  ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Shopkeepers in the Shenbao (1872-1949)", 
    subtitle = "Topic proportion over time (5-topic model)"
  )

# 6-topic model temporal visualization
ggplot(vizDataFrame6y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") +
  ylab("proportion") + 
  scale_fill_manual(
    values = paste0(alphabet(20), "FF"),
    name = "Topic"
  ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Shopkeepers in the Shenbao (1872-1949)", 
    subtitle = "Topic proportion over time (6-topic model)"
  )

# 7-topic model temporal visualization
ggplot(vizDataFrame7y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") +
  ylab("proportion") + 
  scale_fill_manual(
    values = paste0(alphabet(20), "FF"),
    name = "Topic"
  ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Shopkeepers in the Shenbao (1872-1949)", 
    subtitle = "Topic proportion over time (7-topic model)"
  )

# 10-topic model temporal visualization
ggplot(vizDataFrame10y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") +
  ylab("proportion") + 
  scale_fill_manual(
    values = paste0(alphabet(20), "FF"),
    name = "Topic"
  ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Shopkeepers in the Shenbao (1872-1949)", 
    subtitle = "Topic proportion over time (10-topic model)"
  )

# Save final workspace
save.image('shoptm.RData')

# ============================================================================
# PART 8: INTERACTIVE VISUALIZATION
# ============================================================================

# ----------------------------------------------------------------------------
# 30. LDAVIS INTERACTIVE VISUALIZATION
# ----------------------------------------------------------------------------
# LDAvis provides an interactive web-based visualization of topic models
# Features include:
#   - Topic circles sized by prevalence
#   - Distance between topics (similarity)
#   - Top words for each topic
#   - Word saliency and relevance metrics
#
# This opens in a web browser and allows interactive exploration

set.seed(1111)  # For reproducible layouts

stm::toLDAvis(mod.5, doc=out$documents)   # 5-topic model
stm::toLDAvis(mod.6, doc=out$documents)   # 6-topic model
stm::toLDAvis(mod.7, doc=out$documents)   # 7-topic model
stm::toLDAvis(mod.10, doc=out$documents)  # 10-topic model

# ----------------------------------------------------------------------------
# 31. STMINSIGHTS DASHBOARD
# ----------------------------------------------------------------------------
# STMinsights provides a comprehensive Shiny dashboard for exploring STM results
# Features include:
#   - Topic prevalence and covariate effects
#   - Word-topic relationships
#   - Document-topic distributions
#   - Temporal trends
#   - Topic correlations
#
# This launches an interactive web application

library(stminsights)
run_stminsights()

# ============================================================================
# END OF SCRIPT
# ============================================================================
#
# WORKFLOW SUMMARY:
# 1. Load and clean tokenized shopkeeper corpus
# 2. Remove stopwords (general Chinese + domain-specific)
# 3. Prepare texts and metadata for STM
# 4. Test models with 5-10 topics using diagnostic metrics
# 5. Estimate models with different K values
# 6. Explore topics through words, documents, and visualizations
# 7. Analyze temporal trends in topic prevalence
# 8. Create interactive visualizations for detailed exploration
#
# NEXT STEPS:
# - Based on interpretability, select the optimal model 
# - Assign meaningful labels to topics based on top words and documents
# - Conduct deeper analysis of temporal trends for specific topics
# - Potentially estimate models with content covariates or topic correlations
# - Use topics as features for further analysis (classification, clustering, etc.)
#
# KEY FILES PRODUCED:
# - shopTM.csv: Cleaned text ready for modeling
# - topic_proportion_per_year*.csv: Temporal topic trends
# - topic*prop.csv: Document-level topic distributions
# - shoptm.RData: Complete workspace with all models
# ============================================================================