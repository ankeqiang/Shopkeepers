# ==============================================================================
# COMPREHENSIVE COLLOCATION ANALYSIS FOR SHOPKEEPER TERMS ACROSS TIME PERIODS
# ==============================================================================
# This script performs collocation analysis on Chinese text data to identify
# words that frequently appear near shopkeeper terms in the Shenbao corpus
# across five historical periods.
#
# Input: shop_bind_tok2 dataframe
# Output: Period-specific collocation patterns, visualizations, and comparisons
# ==============================================================================

# LIBRARY SETUP ----------------------------------------------------------------
required_packages <- c("tidyverse", "tidytext", "stringr", "quanteda", 
                       "quanteda.textstats", "widyr", "igraph", "ggraph", 
                       "DT", "kableExtra", "lubridate", "patchwork")

# Check and install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(quanteda)
library(quanteda.textstats)
library(widyr)
library(igraph)
library(ggraph)
library(DT)
library(kableExtra)
library(lubridate)
library(patchwork)

# save objects in .RData file
save.image('shopcoloc.RData')


# Re-upload saved RData file
load(file = "shopcoloc.RData")


# CONFIGURATION ----------------------------------------------------------------

# Define shopkeeper terms to analyze
shopkeeper_terms <- c(
  "坊主", "店主", "棧主", "莊主", "號主", "行主", "鋪主", 
  "館主", "店主婦", "店東", "東家", "東主", "店家", "掌櫃", "商户", "業主"
)

# Define time periods
periods <- list(
  period1 = list(name = "1872-1894", start = 1872, end = 1894),
  period2 = list(name = "1895-1911", start = 1895, end = 1911),
  period3 = list(name = "1912-1927", start = 1912, end = 1927),
  period4 = list(name = "1928-1937", start = 1928, end = 1937),
  period5 = list(name = "1938-1949", start = 1938, end = 1949)
)

# Collocation parameters
WINDOW_SIZE <- 5          # Context window for collocations
MIN_COUNT <- 5            # Minimum frequency threshold
MIN_LAMBDA <- 5           # Minimum association strength
TOP_N_COLLOCATIONS <- 50  # Number of top collocations to analyze

# Chinese stopwords list (comprehensive)
chinese_stopwords <- c(
  # Original stopwords
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
  
  # Extended stopwords (Traditional Chinese)
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
  "則甚", "則", "咳", "哇", "哈", "哈哈", "哉", "哎", "哎呀", "哎喲",
  "嘩", "喲", "哦", "哩", "矣哉", "矣乎", "焉", "毋寧", "歟", "嘿嘿",
  "嘿", "嘻", "嘛", "噓", "嘎登", "嘎", "噯", "嗯", "嗬", "嗡嗡",
  "嗡", "嘍", "喔唷", "喏", "喂", "啷當", "啪噠", "啦", "啥", "啐",
  "啊", "唉", "哼唷", "哼", "咧", "咦", "咚", "咋", "呼哧", "呸",
  "呵呵", "呵", "呢", "嗚呼", "嗚", "唄", "嘔", "呃", "呀", "吱",
  "吧噠", "吧", "嗎", "嚇", "兮", "兒", "了", "乎"
)

# DATA PREPARATION -------------------------------------------------------------
  
  # First, let's check the data structure
  cat("Checking data structure...\n")
cat("Total documents in shop_bind_tok2:", nrow(shop_bind_tok2), "\n")
cat("Column names:", paste(names(shop_bind_tok2), collapse = ", "), "\n\n")

# Extract year from DocId (positions 5-8)
shop_bind_tok2 <- shop_bind_tok2 %>%
  mutate(year = as.numeric(str_sub(DocId, 5, 8)))

# Check year extraction
cat("Year range in data:", min(shop_bind_tok2$year, na.rm = TRUE), 
    "to", max(shop_bind_tok2$year, na.rm = TRUE), "\n")
cat("Documents with valid years:", sum(!is.na(shop_bind_tok2$year)), "\n\n")


# MAIN PROCESSING FUNCTION -----------------------------------------------------

process_period_collocations_tokenized <- function(data, period_name, start_year, end_year) {
  
  cat("\n", rep("=", 60), "\n")
  cat("Processing Period:", period_name, "\n")
  cat(rep("=", 60), "\n\n")
  
  # Extract year from DocId (positions 5-8)
  period_data <- data %>%
    mutate(year = as.numeric(str_sub(DocId, 5, 8))) %>%
    filter(year >= start_year & year <= end_year)
  
  cat("Documents in period:", nrow(period_data), "\n")
  
  if (nrow(period_data) == 0) {
    cat("WARNING: No documents found for this period\n")
    return(NULL)
  }
  
  # Filter documents containing shopkeeper terms
  docs_with_terms <- period_data %>%
    filter(str_detect(Text, paste(shopkeeper_terms, collapse = "|")))
  
  cat("Documents with shopkeeper terms:", nrow(docs_with_terms), "\n")
  
  if (nrow(docs_with_terms) < 10) {
    cat("WARNING: Too few documents for reliable analysis\n")
    return(NULL)
  }
  
  # Filter documents containing shopkeeper terms
  docs_with_terms <- period_data %>%
    filter(str_detect(Text, paste(shopkeeper_terms, collapse = "|"))) %>%
    mutate(row_id = row_number())  # NEW: unique ID
  
  # Create corpus
  corpus_period <- corpus(
    docs_with_terms,
    docid_field = "row_id",   # use unique row_id here
    text_field = "Text"
  )
  
  # IMPORTANT: Since text is already tokenized, we use as.tokens() instead of tokens()
  cat("Converting pre-tokenized text to tokens object...\n")
  
  # Split the pre-tokenized text by spaces
  tokens_list <- strsplit(as.character(corpus_period), " ")
  
  # Convert to quanteda tokens object
  tokens_period <- as.tokens(tokens_list,
                             docvars = docvars(corpus_period))
  
  # Remove stopwords if needed
  tokens_period <- tokens_remove(tokens_period, pattern = chinese_stopwords)
  
  cat("Tokenization complete. Documents:", length(tokens_period), "\n")
  
  # Get all collocations first
  cat("Extracting collocations...\n")
  
  all_collocations <- tibble()
  
  tryCatch({
    # Extract 2-gram collocations
    all_2gram <- textstat_collocations(
      tokens_period, 
      size = 2,
      min_count = MIN_COUNT
    )
    
    cat("Found", nrow(all_2gram), "2-gram collocations\n")
    
    if (nrow(all_2gram) > 0) {
      # Show sample
      cat("Sample collocations:\n")
      print(head(all_2gram, 10))
      
      # Filter for shopkeeper terms
      for (term in shopkeeper_terms) {
        term_colloc <- all_2gram %>%
          filter(str_detect(collocation, term))
        
        if (nrow(term_colloc) > 0) {
          cat("  -", term, ":", nrow(term_colloc), "collocations\n")
          
          term_colloc <- term_colloc %>%
            mutate(
              shopkeeper_term = term,
              period = period_name,
              type = "2-gram"
            )
          
          all_collocations <- bind_rows(all_collocations, term_colloc)
        }
      }
    }
    
    # Extract 3-gram collocations
    all_3gram <- textstat_collocations(
      tokens_period, 
      size = 3,
      min_count = MIN_COUNT
    )
    
    cat("Found", nrow(all_3gram), "3-gram collocations\n")
    
    if (nrow(all_3gram) > 0) {
      for (term in shopkeeper_terms) {
        term_colloc <- all_3gram %>%
          filter(str_detect(collocation, term))
        
        if (nrow(term_colloc) > 0) {
          term_colloc <- term_colloc %>%
            mutate(
              shopkeeper_term = term,
              period = period_name,
              type = "3-gram"
            )
          
          all_collocations <- bind_rows(all_collocations, term_colloc)
        }
      }
    }
    
  }, error = function(e) {
    cat("Error in collocation extraction:", e$message, "\n")
  })
  
  # Process results
  if (nrow(all_collocations) > 0) {
    cat("\nProcessing results...\n")
    
    all_collocations <- all_collocations %>%
      # Remove spaces from collocation (for consistency)
      mutate(collocation = str_replace_all(collocation, "\\s+", "")) %>%
      # Remove duplicates
      distinct(shopkeeper_term, collocation, type, .keep_all = TRUE) %>%
      # Keep significant associations (lower threshold for testing)
      filter(lambda >= 3 | z >= 2) %>%
      # Add normalized frequency
      mutate(
        normalized_freq = count / nrow(docs_with_terms) * 1000,
        doc_count = nrow(docs_with_terms)
      ) %>%
      arrange(shopkeeper_term, desc(lambda))
    
    cat("Final collocations:", nrow(all_collocations), "\n")
    cat("Columns:", paste(names(all_collocations), collapse = ", "), "\n")
  }
  
  return(list(
    collocations = all_collocations,
    doc_count = nrow(docs_with_terms),
    total_docs = nrow(period_data),
    period_data = period_data
  ))
}

# TEST: Check if text is already tokenized
cat("=== CHECKING TEXT FORMAT ===\n")
sample_texts <- head(shop_bind_tok2$Text, 3)
cat("Sample texts:\n")
for (i in 1:length(sample_texts)) {
  cat("Text", i, "(first 100 chars):", substr(sample_texts[i], 1, 100), "\n")
}

# Check if texts contain spaces (indicating tokenization)
has_spaces <- sum(str_detect(shop_bind_tok2$Text[1:min(100, nrow(shop_bind_tok2))], " "))
cat("\nTexts with spaces:", has_spaces, "out of", min(100, nrow(shop_bind_tok2)), "checked\n")

if (has_spaces > 50) {
  cat("\nText appears to be pre-tokenized (contains spaces).\n")
  cat("Use the corrected function: process_period_collocations_tokenized()\n")
} else {
  cat("\nText does not appear to be tokenized (few or no spaces).\n")
  cat("Original function should work, but may need Chinese word segmentation.\n")
}

# Test the corrected function
cat("\n=== TESTING CORRECTED FUNCTION ===\n")

# Configure parameters for testing
MIN_COUNT <- 3      # Lowered for testing
MIN_LAMBDA <- 3     # Lowered for testing
WINDOW_SIZE <- 5

# Test on one period
test_result <- process_period_collocations_tokenized(
  shop_bind_tok2,
  "1912-1927",
  1912,
  1927
)

if (!is.null(test_result) && nrow(test_result$collocations) > 0) {
  cat("\n=== SUCCESS! ===\n")
  cat("Found", nrow(test_result$collocations), "collocations\n")
  cat("\nSample results:\n")
  print(head(test_result$collocations, 10))
} else {
  cat("\n=== No collocations found ===\n")
  cat("Possible issues:\n")
  cat("1. Thresholds still too high (try MIN_COUNT = 2)\n")
  cat("2. Shopkeeper terms don't match exactly\n")
  cat("3. Text preprocessing issues\n")
}




# EXECUTE ANALYSIS FOR ALL PERIODS --------------------------------------------

cat("Starting comprehensive collocation analysis...\n")

# Process all periods
period_results <- list()

for (period_id in names(periods)) {
  period_info <- periods[[period_id]]
  
  result <- process_period_collocations_tokenized(
    shop_bind_tok2,
    period_info$name,
    period_info$start,
    period_info$end
  )
  
  if (!is.null(result)) {
    period_results[[period_id]] <- result
  }
}


# AGGREGATE AND ANALYZE RESULTS -----------------------------------------------

cat("\n", rep("=", 70), "\n")
cat("AGGREGATING RESULTS\n")
cat(rep("=", 70), "\n\n")

# First, let's check what we have in period_results
cat("Number of periods with results:", length(period_results), "\n")
cat("Period names:", names(period_results), "\n\n")

# Check the structure of each period's results
for (period_id in names(period_results)) {
  cat("Period", period_id, ":\n")
  cat("  - Number of collocations:", nrow(period_results[[period_id]]$collocations), "\n")
  
  if (nrow(period_results[[period_id]]$collocations) > 0) {
    cat("  - Column names:", names(period_results[[period_id]]$collocations), "\n")
    cat("  - First row:\n")
    print(head(period_results[[period_id]]$collocations, 1))
  }
  cat("\n")
}

# Combine all collocations with error checking
all_collocations_combined <- tibble()

for (period_id in names(period_results)) {
  period_collocations <- period_results[[period_id]]$collocations
  
  if (!is.null(period_collocations) && nrow(period_collocations) > 0) {
    cat("Adding", nrow(period_collocations), "collocations from", periods[[period_id]]$name, "\n")
    all_collocations_combined <- bind_rows(all_collocations_combined, period_collocations)
  }
}

# Check what we actually have
cat("\nCombined collocations info:\n")
cat("Total rows:", nrow(all_collocations_combined), "\n")
cat("Column names:", names(all_collocations_combined), "\n\n")

if (nrow(all_collocations_combined) > 0) {
  cat("Sample of combined data:\n")
  print(head(all_collocations_combined, 3))
  
  # Check data types
  cat("\nData types:\n")
  str(all_collocations_combined)
}

# Now we know what columns we have, let's create appropriate summaries
if (nrow(all_collocations_combined) == 0) {
  stop("No collocations found across all periods. Please check your data and parameters.")
}

# Create summary statistics based on available columns
available_cols <- names(all_collocations_combined)

# Check for required columns and suggest fixes
required_cols <- c("period", "shopkeeper_term", "lambda", "count")
missing_cols <- setdiff(required_cols, available_cols)

if (length(missing_cols) > 0) {
  cat("\nWARNING: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
  
  # Try to fix missing columns
  
  # Fix missing period column
  if (!"period" %in% available_cols) {
    cat("Attempting to reconstruct period information...\n")
    
    # If we can't get period info, we'll need to work without it
    # Check if there's any column that might contain period info
    period_like_cols <- grep("period|Period|时期|年代", available_cols, value = TRUE, ignore.case = TRUE)
    
    if (length(period_like_cols) > 0) {
      cat("Found possible period column:", period_like_cols[1], "\n")
      all_collocations_combined <- all_collocations_combined %>%
        rename(period = !!period_like_cols[1])
    } else {
      cat("ERROR: Cannot determine period information.\n")
      cat("Creating summary without period breakdown...\n")
    }
  }
  
  # Fix missing shopkeeper_term column
  if (!"shopkeeper_term" %in% available_cols) {
    cat("Attempting to extract shopkeeper terms from collocations...\n")
    
    # Check for alternative column names
    term_like_cols <- grep("term|target|shopkeeper", available_cols, value = TRUE, ignore.case = TRUE)
    
    if (length(term_like_cols) > 0) {
      cat("Found possible term column:", term_like_cols[1], "\n")
      all_collocations_combined <- all_collocations_combined %>%
        rename(shopkeeper_term = !!term_like_cols[1])
    } else {
      # Try to extract from collocation text
      all_collocations_combined <- all_collocations_combined %>%
        mutate(shopkeeper_term = case_when(
          str_detect(collocation, "店主") ~ "店主",
          str_detect(collocation, "館主") ~ "館主",
          str_detect(collocation, "行主") ~ "行主",
          str_detect(collocation, "鋪主") ~ "鋪主",
          str_detect(collocation, "坊主") ~ "坊主",
          str_detect(collocation, "棧主") ~ "棧主",
          str_detect(collocation, "莊主") ~ "莊主",
          str_detect(collocation, "號主") ~ "號主",
          str_detect(collocation, "店主婦") ~ "店主婦",
          str_detect(collocation, "店東") ~ "店東",
          str_detect(collocation, "東家") ~ "東家",
          str_detect(collocation, "東主") ~ "東主",
          str_detect(collocation, "店家") ~ "店家",
          str_detect(collocation, "掌櫃") ~ "掌櫃",
          str_detect(collocation, "商户") ~ "商户",
          str_detect(collocation, "業主") ~ "業主",
          TRUE ~ "Unknown"
        ))
      
      # Remove unknowns
      all_collocations_combined <- all_collocations_combined %>%
        filter(shopkeeper_term != "Unknown")
    }
  }
}

# Update available columns after fixes
available_cols <- names(all_collocations_combined)
cat("\nColumns after fixes:", paste(available_cols, collapse = ", "), "\n")

# Now create summaries based on what we have
if ("period" %in% available_cols && "shopkeeper_term" %in% available_cols) {
  cat("\nCreating full summary with period and term grouping...\n")
  
  collocation_summary <- all_collocations_combined %>%
    group_by(period, shopkeeper_term) %>%
    summarise(
      n_collocations = n(),
      avg_lambda = mean(lambda, na.rm = TRUE),
      avg_count = mean(count, na.rm = TRUE),
      max_lambda = max(lambda, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Period-specific collocations
  period_specific <- all_collocations_combined %>%
    group_by(collocation, shopkeeper_term) %>%
    mutate(n_periods = n_distinct(period)) %>%
    filter(n_periods == 1) %>%
    ungroup()
  
  # Persistent collocations
  persistent_collocations <- all_collocations_combined %>%
    group_by(collocation, shopkeeper_term) %>%
    mutate(n_periods = n_distinct(period)) %>%
    filter(n_periods >= 3) %>%
    ungroup()
  
} else if ("shopkeeper_term" %in% available_cols) {
  cat("\nCreating summary by shopkeeper term only...\n")
  
  collocation_summary <- all_collocations_combined %>%
    group_by(shopkeeper_term) %>%
    summarise(
      n_collocations = n(),
      avg_lambda = mean(lambda, na.rm = TRUE),
      avg_count = mean(count, na.rm = TRUE),
      max_lambda = max(lambda, na.rm = TRUE),
      .groups = "drop"
    )
  
  period_specific <- tibble()  # Can't calculate without period
  persistent_collocations <- tibble()  # Can't calculate without period
  
} else {
  cat("\nCreating basic summary...\n")
  
  collocation_summary <- all_collocations_combined %>%
    summarise(
      n_collocations = n(),
      avg_lambda = mean(lambda, na.rm = TRUE),
      avg_count = mean(count, na.rm = TRUE),
      max_lambda = max(lambda, na.rm = TRUE)
    )
  
  period_specific <- tibble()
  persistent_collocations <- tibble()
}

cat("\nSummary created successfully!\n")
print(head(collocation_summary))

write_csv(collocation_summary, "collocation_summary.csv")
write_csv(all_collocations_combined, "all_collocations_combined.csv")


# GENERATE OUTPUTS ------------------------------------------------------------

# 1. Create summary report
cat("\n", rep("=", 60), "\n")
cat("COLLOCATION ANALYSIS SUMMARY REPORT\n")
cat(rep("=", 60), "\n\n")

cat("Total documents analyzed:", sum(map_dbl(period_results, ~ .x$doc_count)), "\n")
cat("Total unique collocations found:", n_distinct(all_collocations_combined$collocation), "\n")
cat("Average collocations per period:", 
    round(nrow(all_collocations_combined) / length(periods), 1), "\n\n")

cat("Documents by period:\n")
for (period_id in names(period_results)) {
  cat(sprintf("  %s: %d documents\n", 
              periods[[period_id]]$name,
              period_results[[period_id]]$doc_count))
}

# 2. Display top collocations by period
cat("\n\nTOP COLLOCATIONS BY PERIOD:\n")
cat(rep("-", 40), "\n")

for (period_name in unique(all_collocations_combined$period)) {
  cat("\nPeriod:", period_name, "\n")
  
  top_in_period <- all_collocations_combined %>%
    filter(period == period_name) %>%
    group_by(shopkeeper_term) %>%
    slice_max(order_by = lambda, n = 3) %>%
    ungroup() %>%
    select(shopkeeper_term, collocation, count, lambda) %>%
    arrange(desc(lambda))
  
  print(top_in_period, n = 15)
}

# 3. Export detailed results
write_csv(all_collocations_combined, "collocations_all_periods.csv")
write_csv(collocation_summary, "collocation_summary_by_period.csv")
write_csv(period_specific, "period_specific_collocations.csv")
write_csv(persistent_collocations, "persistent_collocations.csv")
write_csv(top_in_period, "top_in_period.csv")

# 4. Create visualizations
cat("\n\nGenerating visualizations...\n")

create_period_comparison <- function(shop_term, metric = "lambda") {
  # metric should be one of: "lambda", "count", "normalized_freq" (if available)
  
  if (!metric %in% names(all_collocations_combined)) {
    stop("Metric '", metric, "' not found in all_collocations_combined.")
  }
  
  data_term <- all_collocations_combined %>%
    filter(shopkeeper_term == shop_term) %>%
    group_by(period) %>%
    summarise(
      value = mean(.data[[metric]], na.rm = TRUE),
      n_colloc = n(),
      .groups = "drop"
    )
  
  ggplot(data_term, aes(x = period, y = value, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      title    = paste("Evolution of", metric, "for", shop_term),
      x        = "Period",
      y        = metric
    ) +
    theme_minimal()
}



# Select top shopkeeper terms for visualization
top_shopkeeper_terms <- all_collocations_combined %>%
  count(shopkeeper_term) %>%
  slice_max(order_by = n, n = 3) %>%
  pull(shopkeeper_term)

# Create comparison plots
comparison_plots <- map(top_shopkeeper_terms, 
                        ~ create_period_comparison(.x, "lambda"))

# Save plots
pdf("collocation_analysis_plots.pdf", width = 12, height = 8)
walk(comparison_plots, print)
dev.off()

# 5. Create network visualization for each period
create_network_viz <- function(period_name, min_edge_weight = 5) {
  period_data <- all_collocations_combined %>%
    filter(period == period_name, count >= min_edge_weight)
  
  if (nrow(period_data) < 5) return(NULL)
  
  edges <- period_data %>%
    select(from = shopkeeper_term, to = collocation, weight = count)
  
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  set.seed(123)
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = weight, alpha = weight)) +
    geom_node_point(size = 5, color = "darkred") +
    labs(
      title = paste("Collocation Network:", period_name),
      subtitle = "Width indicates co-occurrence frequency"
    ) +
    theme_graph() +
    theme(legend.position = "none")
}


# Generate network plots
network_plots <- map(unique(all_collocations_combined$period),
                     ~ create_network_viz(.x))
print(network_plots[[1]])

# Save network visualizations
pdf("collocation_networks.pdf", width = 10, height = 10, family = "Helvetica")
walk(network_plots[!sapply(network_plots, is.null)], print)
dev.off()



# 6. Temporal trend analysis
cat("\n\nTEMPORAL TRENDS:\n")
cat(rep("-", 40), "\n")

# Identify terms with changing associations
temporal_changes <- all_collocations_combined %>%
  group_by(collocation, shopkeeper_term) %>%
  filter(n() >= 3) %>%  # Present in at least 3 periods
  summarise(
    periods_present = n(),
    lambda_variance = var(lambda, na.rm = TRUE),
    lambda_trend = cor(as.numeric(factor(period)), lambda, method = "spearman"),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(lambda_trend)))

cat("\nCollocations with strongest temporal trends:\n")
print(head(temporal_changes, 20))

# 7. Create interpretive summary
create_interpretive_summary <- function() {
  
  summary_file <- "collocation_analysis_interpretation.txt"
  
  sink(summary_file)
  
  cat("SHOPKEEPER COLLOCATIONS: INTERPRETIVE SUMMARY\n")
  cat("=============================================\n\n")
  
  cat("1. PERIOD-SPECIFIC PATTERNS\n")
  cat("---------------------------\n")
  
  for (period_name in unique(period_specific$period)) {
    cat("\n", period_name, ":\n")
    
    period_unique <- period_specific %>%
      filter(period == period_name) %>%
      slice_max(order_by = lambda, n = 10)
    
    if (nrow(period_unique) > 0) {
      cat("Unique collocations:", 
          paste(period_unique$collocation, collapse = ", "), "\n")
    }
  }
  
  cat("\n\n2. PERSISTENT PATTERNS ACROSS PERIODS\n")
  cat("-------------------------------------\n")
  
  persistent_summary <- persistent_collocations %>%
    group_by(collocation) %>%
    summarise(
      n_periods = n_distinct(period),
      avg_lambda = mean(lambda),
      terms = paste(unique(shopkeeper_term), collapse = ", ")
    ) %>%
    arrange(desc(n_periods), desc(avg_lambda))
  
  cat("\nMost persistent collocations:\n")
  for (i in 1:min(10, nrow(persistent_summary))) {
    cat(sprintf("- %s (appears in %d periods with terms: %s)\n",
                persistent_summary$collocation[i],
                persistent_summary$n_periods[i],
                persistent_summary$terms[i]))
  }
  
  cat("\n\n3. SEMANTIC CATEGORIES\n")
  cat("----------------------\n")
  
  # Categorize collocations (you may need to adjust these patterns)
  commercial_terms <- c("買", "賣", "價", "錢", "貨", "帳", "銀", "租", "稅")
  social_terms <- c("會", "社", "公", "私", "民", "官", "紳")
  spatial_terms <- c("街", "路", "巷", "區", "界", "內", "外", "前", "後")
  
  categorized <- all_collocations_combined %>%
    mutate(
      category = case_when(
        str_detect(collocation, paste(commercial_terms, collapse = "|")) ~ "Commercial",
        str_detect(collocation, paste(social_terms, collapse = "|")) ~ "Social",
        str_detect(collocation, paste(spatial_terms, collapse = "|")) ~ "Spatial",
        TRUE ~ "Other"
      )
    )
  
  category_summary <- categorized %>%
    group_by(period, category) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = category, values_from = n, values_fill = 0)
  
  cat("\nSemantic categories by period:\n")
  print(category_summary)
  
  sink()
  
  cat("\nInterpretive summary saved to:", summary_file, "\n")
}

# Generate interpretive summary
create_interpretive_summary()

# 8. Save workspace

cat("\n\nAnalysis complete! Files generated:\n")
cat("- collocations_all_periods.csv\n")
cat("- collocation_summary_by_period.csv\n")
cat("- period_specific_collocations.csv\n")
cat("- persistent_collocations.csv\n")
cat("- collocation_analysis_plots.pdf\n")
cat("- collocation_networks.pdf\n")
cat("- collocation_analysis_interpretation.txt\n")
cat("- shopkeeper_collocation_enhanced.RData\n")
