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
#   7. Publication-quality figures for the primary (8-topic) model
#   8. Interactive visualization
#
# The eight-topic model (mod.8) is the one used throughout the accompanying
# blog post: it has the best semantic coherence among the fitted models and
# is the model behind every figure and close-reading claim in that post. The
# other K values (6, 7, 10) are kept as documented alternatives.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------
# Load required packages for topic modeling and visualization

library(histtext)      # For historical Chinese text processing
library(tidyverse)     # For data manipulation and visualization (includes
                        # ggplot2, dplyr, tidyr, stringr, readr)
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

# ----------------------------------------------------------------------------
# 10. FINAL PUNCTUATION AND SYMBOL SWEEP
# ----------------------------------------------------------------------------
# Strips stray punctuation and symbol characters left in the tokenized text,
# both half-width ASCII forms (e.g. "(", ")") and full-width/CJK forms (e.g.
# "（", "）", "〔", "〕"). Left unremoved, these were surfacing as top FREX
# words like "()" and "〔" in the fitted topic models -- they carry no
# semantic content and only dilute the vocabulary.
#
# \\p{P} and \\p{S} are Unicode property classes (punctuation, symbol), so a
# single pass catches both half- and full-width forms, unlike ASCII-only
# punctuation stripping. This runs once here, before shopTM is assigned, and
# the result is cached in shopTM.csv -- there is no need to redo any of the
# steps above to pick up this cleaning.

shoptok3 <- shoptok3 %>%
  mutate(Text = str_remove_all(Text, "[\\p{P}\\p{S}]")) %>%
  mutate(Text = str_squish(Text))

# Save the cleaned dataset for topic modeling
shopTM <- shoptok3
write_csv(shopTM, "shopTM.csv")

# ----------------------------------------------------------------------------
# 11. CREATING PERIOD DIVISIONS (OPTIONAL)
# ----------------------------------------------------------------------------
# Alternative periodization based on different historical criteria
# This shows a different approach to dividing the corpus temporally
# (not used elsewhere in this script -- kept as a starting point for anyone
# who wants to fit period-specific models instead of a single Year covariate)

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
# 12. PREPARING METADATA
# ----------------------------------------------------------------------------
# Extract metadata that will be used in the topic model
# Here we use Year as a covariate to model temporal changes in topics

meta <- shopTM %>%
  transmute(DocId, Year)

# ----------------------------------------------------------------------------
# 13. TEXT PREPROCESSING FOR STM
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
#   - ucp = TRUE: makes textProcessor's own punctuation/number stripping
#     Unicode-aware, as a second line of defense behind the explicit
#     regex sweep in step 10 above
#   - customstopwords: Additional stopwords to remove (the `stop_all` data
#     frame is assumed loaded from the corpus-construction script, same as
#     shop_bind_tok2; it needs a `word` column)
#
# The two stopifnot() guards below fail fast, with a clear message, if a
# prerequisite is missing -- cheaper than discovering it mid-run after
# several expensive steps have already re-executed.

stopifnot(!any(duplicated(shopTM$DocId)))
stopifnot(exists("stop_all"))

corpus <- stm::textProcessor(
  shopTM$Text,
  metadata = meta,
  stem = FALSE,                    # No stemming for Chinese
  wordLengths = c(2, Inf),         # Keep words ≥2 characters
  verbose = FALSE,
  ucp = TRUE,                      # Unicode-aware punctuation/number stripping
  customstopwords = stop_all$word  # Additional custom stopwords
)

# Extract vocabulary for inspection
words <- as_tibble(corpus$vocab)

# ----------------------------------------------------------------------------
# 14. EXPLORING WORD FREQUENCY THRESHOLDS
# ----------------------------------------------------------------------------
# Visualize how many terms would be removed at different frequency thresholds
# This helps decide on an appropriate lower threshold
# Terms appearing in very few documents may be noise or OCR errors

stm::plotRemoved(corpus$documents, lower.thresh = c(0, 10, by=5))

# ----------------------------------------------------------------------------
# 15. PREPARING DOCUMENTS FOR MODELING
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

# Results from prepDocuments (verified run, with the punctuation/symbol
# sweep and ucp = TRUE from steps 10 and 13 above both in place):
#   Removing 695,563 of 839,579 terms (787,854 of 4,622,944 tokens) due to
#     frequency
#   Removing 41 documents with no words
#   Final corpus: 59,106 documents, 144,016 terms, 3,835,090 tokens
# These are the counts the rest of this script -- and the accompanying blog
# post -- are built on. If you change any preprocessing step above, re-run
# this call and expect these numbers to shift.

# Inspect what was removed
wordsremoved <- as_tibble(out$words.removed)
docremoved <- as_tibble(out$docs.removed)

# ============================================================================
# PART 4: MODEL SELECTION AND DIAGNOSTICS
# ============================================================================

# ----------------------------------------------------------------------------
# 16. VERIFYING DOCUMENT/METADATA ALIGNMENT
# ----------------------------------------------------------------------------
# textProcessor() and prepDocuments() match documents to metadata by
# POSITION, not by name: `corpus$documents`/`out$documents` and
# `corpus$meta`/`out$meta` come out of these calls already aligned
# one-to-one, and under normal operation never drift apart on their own.
# (A tempting-looking fix is to re-align by matching names(out$documents)
# against out$meta$DocId -- that does NOT work, because textProcessor()'s
# internal use of tm::VectorSource() discards any names on the input text
# vector, so names(out$documents) are just sequential labels ("1","2","3",
# ...), never DocIds. Matching those against real DocId strings returns
# zero overlap, which silently reduces both objects to zero rows -- passing
# an alignment check trivially, since an empty set matches an empty set,
# while actually discarding the entire corpus.)
#
# What prepDocuments() has no way to know about is our own substantive
# filtering criterion: documents whose Year is NA. Because out$documents and
# out$meta are still guaranteed aligned by position at this point, it is
# safe to compute that drop list from out$meta alone and apply the identical
# index vector to both objects together.

cat("Documents:", length(out$documents), "\n")
cat("Meta rows:", nrow(out$meta), "\n")
cat("Vocab length:", length(out$vocab), "\n")

# Sanity check: prepDocuments()/textProcessor() should already guarantee
# this -- fail loudly if not
stopifnot(length(out$documents) == nrow(out$meta))

cat("NAs in Year:", sum(is.na(out$meta$Year)), "\n")

bad_idx <- which(is.na(out$meta$Year))
if (length(bad_idx) > 0) {
  out$documents <- out$documents[-bad_idx]
  out$meta      <- out$meta[-bad_idx, , drop = FALSE]
}

cat("After fix - Documents:", length(out$documents), "\n")
cat("After fix - Meta rows:", nrow(out$meta), "\n")
cat("After fix - NAs in Year:", sum(is.na(out$meta$Year)), "\n")

# Hard assertions: documents and meta are aligned one-to-one, and the
# corpus isn't empty
stopifnot(length(out$documents) == nrow(out$meta))
stopifnot(length(out$documents) > 0)

# ----------------------------------------------------------------------------
# 17. MODEL SELECTION: COMPARING DIFFERENT NUMBERS OF TOPICS
# ----------------------------------------------------------------------------
# The searchK function fits models with different numbers of topics (K)
# and compares them using multiple diagnostic measures:
#
# - Held-Out Likelihood: Higher is better (model fits held-out data well)
# - Residuals: Lower is better (model fits observed data well)
# - Semantic Coherence: Higher is better (words in topics co-occur more)
# - Lower Bound: Higher is better (overall model fit)
#
# Semantic coherence is the one diagnostic of the four that is NOT
# monotonic in K, which is why it carries the most weight below: the other
# three will favor the largest K available almost by construction. Run
# once, over the full intended K range, on the now-verified-aligned data
# from step 16 -- splitting this into multiple partial runs risks silently
# comparing models fit on inconsistent data.

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

# Interpretation (see the accompanying blog post's Methodology section for
# the full discussion): held-out likelihood, residuals, and the lower bound
# all move in the same, largely uninformative direction as K grows -- a
# model with more topics will almost always fit a little better on these
# three measures alone. Semantic coherence is not monotonic: in this
# corpus it peaks at K=5 and K=8, dips at K=6-7, and drops sharply at K=9
# before a partial recovery at K=10 that still falls short of the K=8
# peak. Combined with exclusivity (how distinctly a topic's words belong
# to it, checked separately once the K=8 model below is fit), K=8 comes
# out ahead of every other candidate on both axes at once, which is why it
# is the model used throughout the rest of this script and the blog post.

# ============================================================================
# PART 5: ESTIMATING TOPIC MODELS
# ============================================================================

# ----------------------------------------------------------------------------
# 18. ESTIMATING MODELS WITH DIFFERENT TOPIC COUNTS
# ----------------------------------------------------------------------------
# Estimate four models with different numbers of topics
# The prevalence formula (~ Year) models how topic proportions change over time

# 6-topic model (most parsimonious candidate)
mod.6 <- stm::stm(
  out$documents,
  out$vocab,
  K = 6,
  data = out$meta,
  prevalence = ~ Year,
  verbose = FALSE
)

# 7-topic model (intermediate candidate)
mod.7 <- stm::stm(
  out$documents,
  out$vocab,
  K = 7,
  data = out$meta,
  prevalence = ~ Year,
  verbose = FALSE
)

# 8-topic model (primary model used throughout the analysis: best semantic
# coherence among the fitted models, plus the best coherence/exclusivity
# trade-off overall)
mod.8 <- stm::stm(
  out$documents,
  out$vocab,
  K = 8,
  data = out$meta,
  prevalence = ~ Year,  # Topic proportions vary by year
  verbose = FALSE
)

# 10-topic model (higher-granularity alternative; contains a redundant
# near-duplicate topic pair -- see the blog post's Methodology section)
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

Year6 <- stm::estimateEffect(1:6 ~ Year, mod.6, meta = out$meta)
Year7 <- stm::estimateEffect(1:7 ~ Year, mod.7, meta = out$meta)
Year8 <- stm::estimateEffect(1:8 ~ Year, mod.8, meta = out$meta)
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

topicprop6 <- make.dt(mod.6, meta)
topicprop7 <- make.dt(mod.7, meta)
topicprop8 <- make.dt(mod.8, meta)
topicprop10 <- make.dt(mod.10, meta)

# ----------------------------------------------------------------------------
# 21. VISUALIZING TOPIC PROPORTIONS IN CORPUS
# ----------------------------------------------------------------------------
# Histogram showing the distribution of topic proportions across all documents
# This reveals which topics are more prevalent overall

plot.STM(mod.6, "hist")   # 6-topic model
plot.STM(mod.7, "hist")   # 7-topic model
plot.STM(mod.8, "hist")   # 8-topic model (primary)
plot.STM(mod.10, "hist")  # 10-topic model

# ----------------------------------------------------------------------------
# 22. TOPIC SUMMARIES
# ----------------------------------------------------------------------------
# Display the most important topics and their top words
# n parameter controls how many top words to show per topic

plot.STM(mod.6, "summary", n=6)
plot.STM(mod.7, "summary", n=7)
plot.STM(mod.8, "summary", n=8)   # Top 8 words for each topic
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

labelTopics(mod.6, n=10)
labelTopics(mod.7, n=10)
labelTopics(mod.8, n=10)   # Show top 10 words per topic (primary model)
labelTopics(mod.10, n=10)

# ----------------------------------------------------------------------------
# 24. WORD CLOUDS FOR VISUAL INTERPRETATION
# ----------------------------------------------------------------------------
# Word clouds provide visual representation of topic word distributions
# Size indicates word importance in the topic

# 8-topic model word clouds (primary model)
cloud(mod.8, topic = 1, scale = c(4, 0.4))
cloud(mod.8, topic = 2, scale = c(4, 0.4))
cloud(mod.8, topic = 3, scale = c(4, 0.4))
cloud(mod.8, topic = 4, scale = c(4, 0.4))
cloud(mod.8, topic = 5, scale = c(4, 0.4))
cloud(mod.8, topic = 6, scale = c(5, 0.6))

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

# 8-topic model examples (primary model)
T8_thoughts1 <- findThoughts(mod.8, texts=shopTM$Text, topics=1, n=5)$docs[[1]]
T8_thoughts2 <- findThoughts(mod.8, texts=shopTM$Text, topics=2, n=5)$docs[[1]]
T8_thoughts3 <- findThoughts(mod.8, texts=shopTM$Text, topics=3, n=5)$docs[[1]]

# 10-topic model examples
T10_thoughts1 <- findThoughts(mod.10, texts=shopTM$Text, topics=1, n=5)$docs[[1]]
T10_thoughts2 <- findThoughts(mod.10, texts=shopTM$Text, topics=2, n=5)$docs[[1]]
T10_thoughts3 <- findThoughts(mod.10, texts=shopTM$Text, topics=3, n=5)$docs[[1]]
T10_thoughts4 <- findThoughts(mod.10, texts=shopTM$Text, topics=4, n=5)$docs[[1]]

# Plot representative quotes for selected topics
par(mfrow=c(1,3), mar=c(1,1,2,2))
plotQuote(T8_thoughts1, width=50, maxwidth=400, text.cex=0.8, main="Topic 1")
plotQuote(T8_thoughts2, width=50, maxwidth=400, text.cex=0.8, main="Topic 2")
plotQuote(T8_thoughts3, width=50, maxwidth=400, text.cex=0.8, main="Topic 3")
par(mfrow=c(1, 1))  # Reset to single plot

# ============================================================================
# PART 7: TEMPORAL ANALYSIS OF TOPICS
# ============================================================================

# ----------------------------------------------------------------------------
# 26. PREPARING DATA FOR TEMPORAL VISUALIZATION
# ----------------------------------------------------------------------------
# Extract just the topic proportion columns (removing DocId)

topic6prop <- topicprop6 %>% select(c(2:7))      # Topics 1-6
topic7prop <- topicprop7 %>% select(c(2:8))      # Topics 1-7
topic8prop <- topicprop8 %>% select(c(2:9))      # Topics 1-8 (primary model)
topic10prop <- topicprop10 %>% select(c(2:11))   # Topics 1-10

# ----------------------------------------------------------------------------
# 27. COMPUTING YEARLY TOPIC PROPORTIONS
# ----------------------------------------------------------------------------
# Calculate the average proportion of each topic per year
# This shows how topics wax and wane over the newspaper's history

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

topic_proportion_per_year8 <- aggregate(
  topic8prop,
  by = list(Year = meta$Year),
  mean
)

topic_proportion_per_year10 <- aggregate(
  topic10prop,
  by = list(Year = meta$Year),
  mean
)

# Export temporal data for external analysis
write_csv(topic_proportion_per_year6, "topic_proportion_per_year6.csv")
write_csv(topic_proportion_per_year7, "topic_proportion_per_year7.csv")
write_csv(topic_proportion_per_year8, "topic_proportion_per_year8.csv")
write_csv(topic_proportion_per_year10, "topic_proportion_per_year10.csv")

# Export document-level topic proportions
write_csv(topic6prop, "topic6prop.csv")
write_csv(topic7prop, "topic7prop.csv")
write_csv(topic8prop, "topic8prop.csv")
write_csv(topic10prop, "topic10prop.csv")

# Also export the full make.dt() output (topic proportions + DocId + Year),
# which is what the figures in Part 8 below and the blog post's close
# reading both key off of
write_csv(topicprop6, "topicprop6.csv")
write_csv(topicprop7, "topicprop7.csv")
write_csv(topicprop8, "topicprop8.csv")
write_csv(topicprop10, "topicprop10.csv")

# ----------------------------------------------------------------------------
# 28. RESHAPING DATA FOR VISUALIZATION
# ----------------------------------------------------------------------------
# Convert from wide format (one column per topic) to long format
# (one row per year-topic combination) for ggplot2

library(reshape)

vizDataFrame6y <- melt(topic_proportion_per_year6, id.vars = "Year")
vizDataFrame7y <- melt(topic_proportion_per_year7, id.vars = "Year")
vizDataFrame8y <- melt(topic_proportion_per_year8, id.vars = "Year")
vizDataFrame10y <- melt(topic_proportion_per_year10, id.vars = "Year")

# ----------------------------------------------------------------------------
# 29. QUICK EXPLORATORY VISUALIZATION: STACKED BAR CHARTS
# ----------------------------------------------------------------------------
# Stacked bar charts show the changing composition of topics over time.
# Each year's bar shows the relative proportion of all topics. These are
# fast, generic-palette charts meant for exploring any candidate model --
# see Part 8 below for the polished, fixed-palette figures actually used
# in the blog post (currently only built for the 8-topic model).

library(pals)  # For color palettes

# 6-topic model temporal visualization
ggplot(vizDataFrame6y, aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  ylab("proportion") +
  scale_fill_manual(
    values = paste0(alphabet(20), "FF"),  # Color palette with full opacity
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

# 8-topic model temporal visualization (primary model)
ggplot(vizDataFrame8y, aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  ylab("proportion") +
  scale_fill_manual(
    values = paste0(alphabet(20), "FF"),
    name = "Topic"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Shopkeepers in the Shenbao (1872-1949)",
    subtitle = "Topic proportion over time (8-topic model)"
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

# Save progress
save.image('shoptm.RData')

# ============================================================================
# PART 8: PUBLICATION-QUALITY FIGURES (8-TOPIC MODEL)
# ============================================================================
# The two figures below reproduce, exactly, the figures used in the
# accompanying blog post. Both use a fixed topic-number-to-color mapping so
# a given topic keeps the same color across every figure in the post, and
# both write out to a PNG via a cairo device -- some installed graphics
# packages (e.g. ragg) silently override ggsave()'s `type =` argument
# otherwise, which is what the custom device wrapper below works around.
#
# CHANGE `out_dir` below to wherever you want the PNGs written before
# running this section.

out_dir <- "./figures"   # <- change to your own output folder
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cairo_png_device <- function(filename, width, height, res, ...) {
  grDevices::png(filename = filename, width = width, height = height,
                 units = "in", res = res, type = "cairo", ...)
}

# Fixed topic labels and fixed topic -> color slots, shared by both figures
# below (one color per topic NUMBER, never reassigned/recycled)
fig_labels <- c(
  Topic1 = "Medical & Pharmaceutical Advertising",
  Topic2 = "Police-Referred Civil & Criminal Cases",
  Topic3 = "Classified Ads: Lost & Contact Notices",
  Topic4 = "Political & Administrative Affairs",
  Topic5 = "China & Society: Everyday Life",
  Topic6 = "Municipal Governance & Labor Affairs",
  Topic7 = "Crime & Police Blotter",
  Topic8 = "Company & Public Notices"
)
fig_palette <- c(
  Topic1 = "#2a78d6", Topic2 = "#eb6834", Topic3 = "#1baf7a", Topic4 = "#eda100",
  Topic5 = "#e87ba4", Topic6 = "#008300", Topic7 = "#4a3aa7", Topic8 = "#e34948"
)

# ----------------------------------------------------------------------------
# 30. FIGURE 1: MEAN TOPIC SHARES (BAR CHART)
# ----------------------------------------------------------------------------
# What the corpus is about, in aggregate: mean document-topic share per
# topic, sorted largest to smallest. Topics used as narrative sources in
# the blog post's close-reading section are bolded in the value labels.

narrative_topics <- c("Topic2", "Topic4", "Topic5", "Topic6", "Topic7")

fig1_means <- topicprop8 %>%
  summarise(across(starts_with("Topic"), mean)) %>%
  pivot_longer(everything(), names_to = "Topic", values_to = "mean_share") %>%
  mutate(
    pct = mean_share * 100,
    label = fig_labels[Topic],
    is_narrative = Topic %in% narrative_topics
  ) %>%
  arrange(desc(pct))

# Order factor levels so the bars plot largest-at-top
fig1_means$label <- factor(fig1_means$label, levels = rev(fig1_means$label))

fig1 <- ggplot(fig1_means, aes(x = pct, y = label, fill = Topic)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct),
                fontface = ifelse(is_narrative, "bold", "plain")),
            hjust = -0.15, size = 3.4, color = "#0b0b0b") +
  scale_fill_manual(values = fig_palette, guide = "none") +
  scale_x_continuous(limits = c(0, 38), expand = c(0, 0)) +
  labs(
    title = "What the corpus is about: TM8 topic shares",
    x = "Mean document-topic share (%)", y = NULL,
    caption = "Topics used as narrative sources in the close-reading section shown in bold."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0),
    plot.caption = element_text(hjust = 0, size = 8.5, color = "#52514e", face = "italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#e3e2dd", linewidth = 0.4),
    axis.text.y = element_text(color = "#0b0b0b", size = 10.5),
    axis.text.x = element_text(color = "#52514e", size = 9.5),
    axis.title.x = element_text(color = "#52514e", size = 10.5)
  )

ggsave(file.path(out_dir, "figure1_topic_shares.png"), fig1, width = 9, height = 6,
       dpi = 200, bg = "white", device = cairo_png_device)
cat("saved figure1_topic_shares.png\n")

# ----------------------------------------------------------------------------
# 31. FIGURE 2: TOPIC SHARES OVER TIME (STACKED AREA)
# ----------------------------------------------------------------------------
# How the corpus's topic composition shifted, 1872-1949. Topics are
# stacked largest-to-smallest (bottom to top) rather than by topic number,
# so the dominant crime/court topics anchor the bottom of the chart.

fig2_long <- topic_proportion_per_year8 %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "proportion")

fig2_topic_order <- fig2_long %>%
  group_by(Topic) %>%
  summarise(mean_share = mean(proportion)) %>%
  arrange(desc(mean_share)) %>%
  pull(Topic)

# Reversed so the LARGEST topic draws first / sits at the BOTTOM of the stack
fig2_long$Topic <- factor(fig2_long$Topic, levels = rev(fig2_topic_order))

fig2 <- ggplot(fig2_long, aes(x = Year, y = proportion * 100, fill = Topic)) +
  geom_area(position = "stack", color = NA) +
  scale_fill_manual(values = fig_palette, labels = fig_labels[levels(fig2_long$Topic)],
                     breaks = rev(levels(fig2_long$Topic))) +  # legend reads largest-first
  scale_x_continuous(limits = c(min(topic_proportion_per_year8$Year),
                                 max(topic_proportion_per_year8$Year)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "How the Shenbao's shopkeeper coverage changed, 1872-1949",
    x = NULL, y = "Share of average document-topic weight (%)", fill = NULL
  ) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e3e2dd", linewidth = 0.4),
    axis.text = element_text(color = "#52514e", size = 9),
    axis.title.y = element_text(color = "#52514e", size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5),
    legend.key.size = unit(0.4, "cm")
  )

ggsave(file.path(out_dir, "figure2_topic_shares_over_time.png"), fig2, width = 11, height = 6.5,
       dpi = 200, bg = "white", device = cairo_png_device)
cat("saved figure2_topic_shares_over_time.png\n")

save.image('shoptm.RData')

# ============================================================================
# PART 9: INTERACTIVE VISUALIZATION
# ============================================================================

# ----------------------------------------------------------------------------
# 32. LDAVIS INTERACTIVE VISUALIZATION
# ----------------------------------------------------------------------------
# LDAvis provides an interactive web-based visualization of topic models
# Features include:
#   - Topic circles sized by prevalence
#   - Distance between topics (similarity)
#   - Top words for each topic
#   - Word saliency and relevance metrics
#
# This opens in a web browser and allows interactive exploration.
#
# reorder.topics = FALSE keeps LDAvis's Topic 1..K identical to the fitted
# model's native Topic 1..K. By default (reorder.topics = TRUE), toLDAvis()
# re-orders topics by decreasing token-share proportion for display, which
# means an LDAvis screenshot's "Topic 1" is not necessarily the fitted
# model's Topic 1 -- and will not match topicprop*.csv or any topic-label
# file keyed on the model's own numbering. Setting it to FALSE means any
# LDAvis screenshot's topic number can be read directly against
# topicprop*.csv and the labels file, with no separate topic-order mapping
# to keep track of.

set.seed(1111)  # For reproducible layouts

stm::toLDAvis(mod.6, doc=out$documents, reorder.topics = FALSE)   # 6-topic model
stm::toLDAvis(mod.7, doc=out$documents, reorder.topics = FALSE)   # 7-topic model
stm::toLDAvis(mod.8, doc=out$documents, reorder.topics = FALSE)   # 8-topic model (primary)
stm::toLDAvis(mod.10, doc=out$documents, reorder.topics = FALSE)  # 10-topic model

# ----------------------------------------------------------------------------
# 33. STMINSIGHTS DASHBOARD
# ----------------------------------------------------------------------------
# STMinsights provides a comprehensive Shiny dashboard for exploring STM results
# Features include:
#   - Topic prevalence and covariate effects
#   - Word-topic relationships
#   - Document-topic distributions
#   - Temporal trends
#   - Topic correlations
#
# This launches an interactive web application. This is also where the
# actual label-validation pass for the blog post was done: reading full
# word lists topic by topic against the underlying documents to check that
# each proposed label actually matched what was in the topic.

library(stminsights)
run_stminsights()

# ============================================================================
# END OF SCRIPT
# ============================================================================
#
# WORKFLOW SUMMARY:
# 1. Load and clean tokenized shopkeeper corpus
# 2. Remove stopwords (general Chinese + domain-specific) and stray
#    punctuation/symbol characters
# 3. Prepare texts and metadata for STM, with alignment safety checks
# 4. Test models with 5-10 topics using diagnostic metrics
# 5. Estimate models at K = 6, 7, 8, 10 (K=8 is the primary model)
# 6. Explore topics through words, documents, and visualizations
# 7. Analyze temporal trends in topic prevalence
# 8. Produce the publication-quality figures used in the blog post
# 9. Create interactive visualizations for detailed exploration
#
# NEXT STEPS:
# - Assign meaningful labels to topics based on top words and documents
#   (see stminsights in step 33, and the blog post's topic-label table)
# - Conduct deeper analysis of temporal trends for specific topics
# - Potentially estimate models with content covariates or topic correlations
#   (see stm::topicCorr(), used in the blog post's correlation-network figure)
# - Use topics as features for further analysis (classification, clustering, etc.)
#
# KEY FILES PRODUCED:
# - shopTM.csv: Cleaned text ready for modeling
# - topic_proportion_per_year*.csv: Mean topic proportion by year, per model
# - topic*prop.csv: Document-level topic proportions only
# - topicprop*.csv: Document-level topic proportions plus DocId and Year
#   (this is the file the blog post's figures and close reading are built on,
#   for K=8 specifically)
# - figures/figure1_topic_shares.png, figures/figure2_topic_shares_over_time.png:
#   the two publication-quality figures from Part 8
# - shoptm.RData: Complete workspace with all models
# ============================================================================
