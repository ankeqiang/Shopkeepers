# ============================================================================
# SHENBAO SHOPKEEPER ARTICLE SEGMENTATION AND EXTRACTION SYSTEM
# ============================================================================
# This script addresses the problem of poorly segmented long articles in the
# Shenbao corpus (shops_ftext401: articles >400 characters). The issue stems
# from the newspaper's original format where multiple unrelated articles were
# published continuously without clear visual boundaries, leading to their
# erroneous aggregation into single "documents" during digitization.
#
# PROBLEM: Long documents often contain multiple distinct articles about 
# different topics concatenated together, making them unsuitable for focused
# analysis of shopkeeper-related content.
#
# SOLUTION: This script implements a three-stage pipeline:
#   Stage 1: FILTER - Identify documents likely containing shopkeeper content
#   Stage 2: SEGMENT - Split documents into individual articles using markers
#   Stage 3: EXTRACT - Identify and extract shopkeeper-specific articles
#
# METHODOLOGY: Uses pattern-based segmentation with hierarchical confidence
# assessment, identifying article boundaries through institutional referrals,
# temporal markers, court proceedings, and narrative structure indicators.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SETUP AND INITIALIZATION
# ----------------------------------------------------------------------------

library(stringr)       # For string manipulation and pattern matching
library(jsonlite)      # For JSON output format
library(purrr)         # For functional programming tools
library(dplyr)         # For data manipulation (loaded later)

# Save workspace (optional - for preserving work in progress)
save.image('shopsegment.RData')

# Load previously saved workspace (optional - uncomment if resuming work)
# load(file = "shopsegment.RData")

# ----------------------------------------------------------------------------
# 2. DEFINE REGEX PATTERNS FOR SEGMENTATION AND EXTRACTION
# ----------------------------------------------------------------------------
# These patterns are organized by their role in the three-stage pipeline

PATTERNS <- list(
  # -------------------------
  # STAGE 1: FILTERING PATTERNS
  # -------------------------
  # Identify documents that likely contain shopkeeper-related content
  
  # Primary shopkeeper terms (16 terms from corpus construction)
  primary_shopkeeper = "坊主|店主|棧主|莊主|號主|行主|鋪主|館主|店主婦|店東|東家|東主|店家|掌櫃|商户|業主",
  
  # Shop type indicators (specific business types)
  shop_types = "染坊|布庄|布店|米店|米行|皮坊|槽坊|書坊|醬園|縫衣店|洋行|茶肆|茶館|飯店|飯館|藥店|藥鋪|當鋪|押店|銀樓|錢莊",
  
  # Case context indicators (legal/dispute situations involving shopkeepers)
  case_context = "工人|夥計|傭工|買賣|交易|貨款|生意|營業|開張|訛詐|假票|欺騙|搶劫|失火|糾紛",
  
  # Exclusion patterns - remove documents that are market reports only
  market_report = "糧食市|絲市|茶市",
  
  # -------------------------
  # STAGE 2: SEGMENTATION PATTERNS
  # -------------------------
  # Identify boundaries between distinct articles within documents
  
  # Level 1 Boundary: Bullet point marker (strongest boundary indicator)
  boundary_bullet = "○",
  
  # Level 2A: Institutional referral patterns
  # Indicates formal transfer of cases between institutions
  # Format: [Institution](函送|解送|送案)[Destination/Details]
  institutional_referral = "([^\n，、。]{1,12})(函送|解送|送案)([^\n。]{1,40})",
  
  # Level 2B: Temporal opening markers
  # Articles typically begin with temporal phrases
  # Examples: 日前 (the other day), 昨日 (yesterday), 今日 (today)
  temporal_open = "(日前|前晚|昨(?:日)?|今(?:日)?|頃息)([^。\n]{0,60})",
  
  # Level 2C: Filing action markers
  # Indicates someone initiating legal action
  # Examples: [Name]投案 (turned himself in), [Name]控 (accused)
  filing_action = "([一-龥]{1,6})(投案|控[^\n，、。]{0,6}|扭(?:送至)?(?:縣|案)?|交到)",
  
  # Level 2D: Court session opening markers
  # Indicates magistrate opening a court session
  # Format: [Magistrate Title][Name/Details]升堂 (court in session)
  court_open = "([一-龥]{1,6})(太守|縣主|知縣|[一-龥]{1,4}大令)([^。\n]{0,8})升堂(?:研訊)?",
  
  # Level 2E: Geographic header pattern
  # Articles from different locations often start with place names
  # Examples: 松江: (Songjiang:), 上海: (Shanghai:)
  geo_header = "^(松江|蘇州|杭州|嘉興|常州|湖州|江寧|上海|吳江|[^：:]{2,6})[：:]",
  
  # Level 3: Closing indicators
  # Patterns that suggest an article is ending
  
  # Disposition markers - indicate case resolution
  # Examples: 笞責20板 (20 strokes with light bamboo), 交保 (released on bail)
  disposition = "笞責\\d+板|管押[^\n。]*|枷號[^\n。]*|交保|交人保出|交人保去|退去|領回|候訊|再候傳訊|押候",
  
  # Ceremonial closing phrases
  # Formal phrases that typically end legal case reports
  ceremonial_close = "叩謝而退|取具改過切結然後交人保出|遵從並請出示曉諭",
  
  # Continuation marker - indicates related subsequent case
  continuation = "又訊",
  
  # Reference to prior report
  related_prior = "前報",
  
  # -------------------------
  # STAGE 3: EXTRACTION PATTERNS
  # -------------------------
  # Identify legal roles and narrative structure
  
  # Plaintiff/accuser keywords
  plaintiff_keywords = "訴稱|控|原告|自陳",
  
  # Defendant keywords
  defendant_keywords = "被控|被告",
  
  # Testimony marker
  testimony = "供稱"
)

# ----------------------------------------------------------------------------
# 3. STAGE 1: DOCUMENT FILTERING FUNCTIONS
# ----------------------------------------------------------------------------
# Purpose: Pre-filter documents to identify those likely containing 
# shopkeeper-related content, reducing unnecessary processing

filter_shopkeeper_docs <- function(doc_text, doc_id) {
  # Step 1: Check for primary shopkeeper terms
  # If no shopkeeper terms present, document is irrelevant
  has_primary <- str_detect(doc_text, PATTERNS$primary_shopkeeper)
  
  if (!has_primary) {
    return(list(
      keep = FALSE, 
      reason = "無主要店主術語"  # No primary shopkeeper terms
    ))
  }
  
  # Step 2: Check for supporting context
  # Documents with shop types or case context are more likely substantive
  has_shop_type <- str_detect(doc_text, PATTERNS$shop_types)
  has_case_context <- str_detect(doc_text, PATTERNS$case_context)
  
  # Step 3: Apply exclusion rules
  # Market reports contain shopkeeper terms but aren't substantive articles
  # Exclude if it's a market report WITHOUT any case context
  is_market_report <- str_detect(doc_text, PATTERNS$market_report) && 
                      !str_detect(doc_text, PATTERNS$case_context)
  
  if (is_market_report) {
    return(list(
      keep = FALSE, 
      reason = "僅市場報告"  # Market report only
    ))
  }
  
  # Step 4: Final decision
  # Keep if has primary term AND either shop type or case context
  keep <- has_primary && (has_shop_type || has_case_context)
  
  return(list(
    keep = keep,
    reason = if(keep) "包含實質店主相關內容" else "僅外圍提及",
    has_primary = has_primary,
    has_shop_type = has_shop_type,
    has_case_context = has_case_context
  ))
}

# ----------------------------------------------------------------------------
# 4. STAGE 2: ARTICLE SEGMENTATION FUNCTIONS
# ----------------------------------------------------------------------------
# Purpose: Split documents into individual articles using hierarchical markers

segment_document <- function(doc_text, doc_id) {
  # STEP 1: Initial scan - split by ○ (bullet point) boundaries
  # The ○ symbol is the strongest boundary indicator in Shenbao
  primary_splits <- str_split(doc_text, "○")[[1]]
  
  # Remove empty blocks
  primary_splits <- primary_splits[nchar(str_trim(primary_splits)) > 0]
  
  # Initialize article collection
  articles <- list()
  article_id <- 1
  
  # STEP 2: Process each block from ○-based splitting
  for (block in primary_splits) {
    
    # STEP 2A: Check for institutional referrals (definitive boundaries)
    # Institutional referrals are strong indicators of article boundaries
    # because they represent formal case transfers between institutions
    inst_matches <- str_locate_all(block, PATTERNS$institutional_referral)[[1]]
    
    if (nrow(inst_matches) > 0) {
      # Found institutional referrals - use them as split points
      split_points <- c(1, inst_matches[, "start"], nchar(block) + 1)
      
      # Create segments between institutional referrals
      for (i in 1:(length(split_points) - 1)) {
        segment <- str_sub(block, split_points[i], split_points[i + 1] - 1)
        segment <- str_trim(segment)
        
        # Apply minimum length threshold to filter out fragments
        if (nchar(segment) > 20) {
          # Assess how confident we are in this segmentation
          confidence <- assess_segmentation_confidence(segment)
          
          # Store the article with metadata
          articles[[article_id]] <- list(
            text = segment,
            opening_marker = extract_opening_marker(segment),
            closing_marker = extract_closing_marker(segment),
            confidence = confidence,
            doc_id = doc_id,
            article_num = article_id
          )
          article_id <- article_id + 1
        }
      }
    } else {
      # STEP 2B: No institutional referrals found
      # Use alternative boundary markers (temporal, court, filing)
      segments <- segment_by_temporal_and_court_markers(block)
      
      for (segment in segments) {
        if (nchar(segment) > 20) {
          confidence <- assess_segmentation_confidence(segment)
          
          articles[[article_id]] <- list(
            text = segment,
            opening_marker = extract_opening_marker(segment),
            closing_marker = extract_closing_marker(segment),
            confidence = confidence,
            doc_id = doc_id,
            article_num = article_id
          )
          article_id <- article_id + 1
        }
      }
    }
  }
  
  return(articles)
}

segment_by_temporal_and_court_markers <- function(text) {
  # Purpose: Segment text using Level 2 opening indicators when
  # institutional referrals (Level 1) are not available
  
  # Find all Level 2 opening indicators
  temporal_locs <- str_locate_all(text, PATTERNS$temporal_open)[[1]]
  court_locs <- str_locate_all(text, PATTERNS$court_open)[[1]]
  filing_locs <- str_locate_all(text, PATTERNS$filing_action)[[1]]
  
  # Combine all opening markers
  all_openers <- rbind(temporal_locs, court_locs, filing_locs)
  
  # If no opening markers found, treat entire text as single article
  if (nrow(all_openers) == 0) {
    return(list(text))
  }
  
  # Sort markers by position in text
  all_openers <- all_openers[order(all_openers[, "start"]), , drop = FALSE]
  
  # Create segments using marker positions as boundaries
  segments <- list()
  split_points <- c(1, all_openers[, "start"], nchar(text) + 1)
  
  for (i in 1:(length(split_points) - 1)) {
    seg <- str_sub(text, split_points[i], split_points[i + 1] - 1)
    seg <- str_trim(seg)
    
    if (nchar(seg) > 0) {
      segments[[length(segments) + 1]] <- seg
    }
  }
  
  return(segments)
}

assess_segmentation_confidence <- function(text) {
  # Purpose: Evaluate confidence that this text represents a complete,
  # correctly segmented article using hierarchical indicators
  #
  # Confidence levels:
  # - High: Strong evidence of correct segmentation
  # - Medium: Some evidence of correct segmentation
  # - Low: Weak evidence, may be incorrectly segmented
  
  # Level 1: Institutional referral (strongest indicator)
  has_inst_ref <- str_detect(text, PATTERNS$institutional_referral)
  
  # Level 2: Opening indicators
  has_temporal_open <- str_detect(text, PATTERNS$temporal_open)
  has_court_open <- str_detect(text, PATTERNS$court_open)
  has_filing <- str_detect(text, PATTERNS$filing_action)
  has_strong_open <- has_temporal_open || has_court_open || has_filing
  
  # Level 3: Closing indicators
  has_disposition <- str_detect(text, PATTERNS$disposition)
  has_ceremonial <- str_detect(text, PATTERNS$ceremonial_close)
  has_strong_close <- has_disposition || has_ceremonial
  
  # Level 4: Narrative arc indicators
  has_testimony <- str_detect(text, PATTERNS$testimony)
  
  # Scoring logic: Hierarchical evaluation
  if (has_inst_ref || (has_strong_open && has_strong_close)) {
    # Has institutional referral OR both strong opening and closing
    return("High")
  } else if (has_strong_open || has_strong_close || has_testimony) {
    # Has at least one strong structural indicator
    return("Medium")
  } else {
    # Lacks strong structural indicators
    return("Low")
  }
}

extract_opening_marker <- function(text) {
  # Purpose: Extract the beginning of an article for human verification
  # Returns first 50 characters of the first sentence/line
  
  first_line <- str_split(text, "[\n。]")[[1]][1]
  marker <- str_sub(first_line, 1, 50)
  return(str_trim(marker))
}

extract_closing_marker <- function(text) {
  # Purpose: Extract the ending of an article for human verification
  # Returns last 50 characters of the last sentence/line
  
  lines <- str_split(text, "[\n。]")[[1]]
  last_line <- lines[length(lines)]
  marker <- str_sub(last_line, max(1, nchar(last_line) - 50), nchar(last_line))
  return(str_trim(marker))
}

# ----------------------------------------------------------------------------
# 5. STAGE 3: SHOPKEEPER ARTICLE EXTRACTION FUNCTIONS
# ----------------------------------------------------------------------------
# Purpose: From segmented articles, identify and extract those specifically
# related to shopkeepers

extract_shopkeeper_articles <- function(articles) {
  # Process each segmented article to determine shopkeeper relevance
  shopkeeper_articles <- list()
  
  for (article in articles) {
    analysis <- analyze_article(article)
    
    # DECISION RULE: Extract if contains primary shopkeeper term
    # This is a relaxed criterion - we prefer false positives to false negatives
    # Articles can be further filtered during analysis if needed
    if (analysis$has_shopkeeper_term) {
      
      # Enrich article with analytical metadata
      enriched_article <- list(
        案件編號 = sprintf("%s_%03d", article$doc_id, article$article_num),
        文檔來源 = article$doc_id,
        案件類型 = classify_case_type(article$text),
        涉案商鋪 = extract_shop_info(article$text),
        置信度 = list(
          分段可靠性 = article$confidence,
          店主相關性 = assess_shopkeeper_relevance(article$text)
        ),
        原文 = article$text,
        開頭標記 = article$opening_marker,
        結尾標記 = article$closing_marker
      )
      
      shopkeeper_articles[[length(shopkeeper_articles) + 1]] <- enriched_article
    }
  }
  
  return(shopkeeper_articles)
}

analyze_article <- function(article) {
  # Purpose: Analyze article content to determine shopkeeper relevance
  
  text <- article$text
  
  # Check for primary shopkeeper terms
  has_shopkeeper <- str_detect(text, PATTERNS$primary_shopkeeper)
  
  # Check for supporting context
  has_shop_type <- str_detect(text, PATTERNS$shop_types)
  has_case_context <- str_detect(text, PATTERNS$case_context)
  
  # Check narrative structure
  has_plaintiff <- str_detect(text, PATTERNS$plaintiff_keywords)
  has_defendant <- str_detect(text, PATTERNS$defendant_keywords)
  has_testimony <- str_detect(text, PATTERNS$testimony)
  
  return(list(
    has_shopkeeper_term = has_shopkeeper,
    has_shop_type = has_shop_type,
    has_case_context = has_case_context,
    has_plaintiff = has_plaintiff,
    has_defendant = has_defendant,
    has_testimony = has_testimony
  ))
}

classify_case_type <- function(text) {
  # Purpose: Classify the type of case described in the article
  # Based on keywords indicating different types of legal disputes
  
  if (str_detect(text, "搶劫|偷竊|竊盜")) {
    return("盜竊案")  # Theft case
  } else if (str_detect(text, "訛詐|欺騙|假票")) {
    return("詐騙案")  # Fraud case
  } else if (str_detect(text, "失火|火災")) {
    return("火災案")  # Fire case
  } else if (str_detect(text, "糾紛|爭執|毆打")) {
    return("糾紛案")  # Dispute case
  } else if (str_detect(text, "買賣|交易|貨款")) {
    return("商業案")  # Commercial case
  } else if (str_detect(text, "工人|夥計|傭工")) {
    return("勞資案")  # Labor case
  } else {
    return("其他")    # Other
  }
}

extract_shop_info <- function(text) {
  # Purpose: Extract information about the shop(s) mentioned in the article
  
  # Find shop type
  shop_type_match <- str_extract(text, PATTERNS$shop_types)
  
  # Find shopkeeper term used
  shopkeeper_match <- str_extract(text, PATTERNS$primary_shopkeeper)
  
  return(list(
    類型 = if(!is.na(shop_type_match)) shop_type_match else "未明",
    店主稱謂 = if(!is.na(shopkeeper_match)) shopkeeper_match else "未明"
  ))
}

assess_shopkeeper_relevance <- function(text) {
  # Purpose: Assess how relevant the article is to shopkeeper activities
  # Returns: High, Medium, or Low
  
  # Count shopkeeper term occurrences
  shopkeeper_count <- str_count(text, PATTERNS$primary_shopkeeper)
  
  # Check for substantive shopkeeper context
  has_shop_type <- str_detect(text, PATTERNS$shop_types)
  has_case_context <- str_detect(text, PATTERNS$case_context)
  
  # Scoring logic
  if (shopkeeper_count >= 3 && (has_shop_type || has_case_context)) {
    return("High")    # Multiple mentions with substantive context
  } else if (shopkeeper_count >= 2 || has_shop_type || has_case_context) {
    return("Medium")  # Some mentions or some context
  } else {
    return("Low")     # Minimal mentions, minimal context
  }
}

# ----------------------------------------------------------------------------
# 6. MAIN PROCESSING PIPELINE
# ----------------------------------------------------------------------------
# Purpose: Coordinate the three-stage pipeline for the entire corpus

process_shenbao_corpus <- function(documents) {
  # Initialize statistics tracking
  stats <- list(
    處理文檔總數 = 0,
    保留文檔數 = 0,
    丟棄文檔數 = 0,
    分段文章總數 = 0,
    提取店主相關文章數 = 0,
    高置信度文章數 = 0,
    中置信度文章數 = 0,
    低置信度文章數 = 0,
    不完整文章數 = 0
  )
  
  # Storage for results
  all_segmented_articles <- list()
  shopkeeper_articles <- list()
  filtered_docs <- list()
  
  # Process each document
  for (i in seq_along(documents)) {
    doc <- documents[[i]]
    stats$處理文檔總數 <- stats$處理文檔總數 + 1
    
    # Progress indicator (every 100 documents)
    if (stats$處理文檔總數 %% 100 == 0) {
      cat(sprintf("Processing document %d/%d...\n", 
                  stats$處理文檔總數, length(documents)))
    }
    
    # STAGE 1: FILTER
    filter_result <- filter_shopkeeper_docs(doc$text, doc$id)
    
    if (!filter_result$keep) {
      # Document doesn't contain relevant shopkeeper content
      stats$丟棄文檔數 <- stats$丟棄文檔數 + 1
      filtered_docs[[length(filtered_docs) + 1]] <- list(
        doc_id = doc$id,
        reason = filter_result$reason
      )
      next  # Skip to next document
    }
    
    stats$保留文檔數 <- stats$保留文檔數 + 1
    
    # STAGE 2: SEGMENT
    segmented <- segment_document(doc$text, doc$id)
    stats$分段文章總數 <- stats$分段文章總數 + length(segmented)
    
    # Store all segmented articles
    all_segmented_articles <- c(all_segmented_articles, segmented)
    
    # STAGE 3: EXTRACT
    extracted <- extract_shopkeeper_articles(segmented)
    
    # Update statistics
    stats$提取店主相關文章數 <- stats$提取店主相關文章數 + length(extracted)
    
    for (article in extracted) {
      # Count by confidence level
      if (article$置信度$分段可靠性 == "High") {
        stats$高置信度文章數 <- stats$高置信度文章數 + 1
      } else if (article$置信度$分段可靠性 == "Medium") {
        stats$中置信度文章數 <- stats$中置信度文章數 + 1
      } else {
        stats$低置信度文章數 <- stats$低置信度文章數 + 1
      }
      
      # Check for incomplete articles (heuristic: very short or lacking closing markers)
      if (nchar(article$原文) < 50 || article$結尾標記 == "") {
        stats$不完整文章數 <- stats$不完整文章數 + 1
      }
    }
    
    # Store extracted articles
    shopkeeper_articles <- c(shopkeeper_articles, extracted)
  }
  
  # Return comprehensive results
  return(list(
    statistics = stats,
    all_articles = all_segmented_articles,
    extracted_articles = shopkeeper_articles,
    filtered_documents = filtered_docs
  ))
}

# ----------------------------------------------------------------------------
# 7. INPUT/OUTPUT FUNCTIONS
# ----------------------------------------------------------------------------

read_shenbao_csv <- function(csv_file, text_column = "Text", id_column = "DocId") {
  # Purpose: Read CSV file and prepare it for processing
  
  cat(sprintf("Reading CSV file: %s\n", csv_file))
  
  # Read the CSV
  data <- read.csv(csv_file, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  # Check required columns exist
  if (!text_column %in% colnames(data)) {
    stop(sprintf("Text column '%s' not found in CSV", text_column))
  }
  if (!id_column %in% colnames(data)) {
    stop(sprintf("ID column '%s' not found in CSV", id_column))
  }
  
  # Convert to list format expected by processing functions
  documents <- lapply(1:nrow(data), function(i) {
    list(
      id = data[[id_column]][i],
      text = data[[text_column]][i]
    )
  })
  
  cat(sprintf("Loaded %d documents\n", length(documents)))
  
  return(list(
    documents = documents,
    original_data = data
  ))
}

output_results_enhanced <- function(results, 
                                   output_json = "shenbao_results.json",
                                   output_csv = "shenbao_summary.csv",
                                   output_stats = "shenbao_statistics.txt") {
  # Purpose: Save processing results in multiple formats
  
  cat("\nSaving results...\n")
  
  # 1. Save complete results as JSON (preserves all structure)
  write(toJSON(results, pretty = TRUE, auto_unbox = TRUE), output_json)
  cat(sprintf("Complete results saved to: %s\n", output_json))
  
  # 2. Save extracted articles as CSV (for easy analysis)
  if (length(results$extracted_articles) > 0) {
    csv_data <- data.frame(
      案件編號 = sapply(results$extracted_articles, function(x) x$案件編號),
      文檔來源 = sapply(results$extracted_articles, function(x) x$文檔來源),
      案件類型 = sapply(results$extracted_articles, function(x) x$案件類型),
      商鋪類型 = sapply(results$extracted_articles, function(x) x$涉案商鋪$類型),
      分段可靠性 = sapply(results$extracted_articles, function(x) x$置信度$分段可靠性),
      店主相關性 = sapply(results$extracted_articles, function(x) x$置信度$店主相關性),
      原文 = sapply(results$extracted_articles, function(x) x$原文),
      stringsAsFactors = FALSE
    )
    
    write.csv(csv_data, output_csv, row.names = FALSE, fileEncoding = "UTF-8")
    cat(sprintf("Summary CSV saved to: %s\n", output_csv))
  }
  
  # 3. Save statistics as readable text file
  stats_text <- sprintf(
    "===========================================
申報店主文章分段與提取統計報告
生成時間: %s
===========================================

第一階段：文檔過濾
- 處理文檔總數: %d
- 保留文檔數: %d
- 丟棄文檔數: %d
- 保留率: %.1f%%

第二階段：文章分段
- 分段文章總數: %d

第三階段：店主文章提取
- 提取店主相關文章數: %d
- 提取率: %.1f%%

置信度分布:
- 高置信度文章數: %d (%.1f%%)
- 中置信度文章數: %d (%.1f%%)
- 低置信度文章數: %d (%.1f%%)

完整性:
- 不完整文章數: %d
- 完整文章數: %d
===========================================
",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    results$statistics$處理文檔總數,
    results$statistics$保留文檔數,
    results$statistics$丟棄文檔數,
    100 * results$statistics$保留文檔數 / results$statistics$處理文檔總數,
    results$statistics$分段文章總數,
    results$statistics$提取店主相關文章數,
    100 * results$statistics$提取店主相關文章數 / max(results$statistics$分段文章總數, 1),
    results$statistics$高置信度文章數,
    100 * results$statistics$高置信度文章數 / max(results$statistics$提取店主相關文章數, 1),
    results$statistics$中置信度文章數,
    100 * results$statistics$中置信度文章數 / max(results$statistics$提取店主相關文章數, 1),
    results$statistics$低置信度文章數,
    100 * results$statistics$低置信度文章數 / max(results$statistics$提取店主相關文章數, 1),
    results$statistics$不完整文章數,
    results$statistics$提取店主相關文章數 - results$statistics$不完整文章數
  )
  
  writeLines(stats_text, output_stats, useBytes = TRUE)
  cat(sprintf("Statistics report saved to: %s\n", output_stats))
  
  # Print statistics to console
  cat("\n")
  cat(stats_text)
  cat("\n")
}

# ----------------------------------------------------------------------------
# 8. MAIN EXECUTION WRAPPER
# ----------------------------------------------------------------------------

process_shenbao_from_csv <- function(csv_file,
                                     text_column = "Text",
                                     id_column = "DocId",
                                     output_dir = "output") {
  # Purpose: Main entry point for processing a CSV file
  # This function wraps the entire pipeline for convenience
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("Created output directory: %s\n", output_dir))
  }
  
  # Read input CSV
  csv_data <- read_shenbao_csv(csv_file, text_column, id_column)
  
  # Process documents through three-stage pipeline
  cat("\nStarting three-stage processing pipeline...\n")
  cat("Stage 1: Filtering documents for shopkeeper content\n")
  cat("Stage 2: Segmenting documents into individual articles\n")
  cat("Stage 3: Extracting shopkeeper-specific articles\n\n")
  
  results <- process_shenbao_corpus(csv_data$documents)
  
  # Output results in multiple formats
  output_results_enhanced(
    results,
    output_json = file.path(output_dir, "shenbao_results.json"),
    output_csv = file.path(output_dir, "shenbao_summary.csv"),
    output_stats = file.path(output_dir, "shenbao_statistics.txt")
  )
  
  return(results)
}

# ============================================================================
# 9. IMPLEMENTATION AND POST-PROCESSING
# ============================================================================

# ----------------------------------------------------------------------------
# 9.1 BASIC USAGE - Process your CSV file
# ----------------------------------------------------------------------------

cat("\n=== Starting Shenbao Segmentation Process ===\n\n")

# Process the shops_ftext401.csv file (articles >400 characters)
results <- process_shenbao_from_csv("shops_ftext401.csv")

# Store results
shops_ftext401_segment <- results

# Save main results as CSV
write.csv(shops_ftext401_segment, "shops_ftext401_segment.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== Processing Complete ===\n")

# ----------------------------------------------------------------------------
# 9.2 ALTERNATIVE USAGE with custom parameters
# ----------------------------------------------------------------------------
# If you need to customize column names or output directory:
#
# results <- process_shenbao_from_csv(
#   csv_file = "your_file.csv",
#   text_column = "Text",          # Column containing article text
#   id_column = "DocId",            # Column containing document IDs
#   output_dir = "custom_output"    # Directory for output files
# )

# ----------------------------------------------------------------------------
# 9.3 POST-PROCESSING: Convert results to data frame and filter
# ----------------------------------------------------------------------------

library(dplyr)

cat("\n=== Post-Processing: Converting to Data Frame ===\n")

# Step 1: Convert extracted articles to a data frame
shopkeeper_df <- data.frame(
  案件編號 = sapply(results$extracted_articles, function(x) x$案件編號),
  文檔來源 = sapply(results$extracted_articles, function(x) x$文檔來源),
  案件類型 = sapply(results$extracted_articles, function(x) x$案件類型),
  商鋪類型 = sapply(results$extracted_articles, function(x) x$涉案商鋪$類型),
  分段可靠性 = sapply(results$extracted_articles, function(x) x$置信度$分段可靠性),
  店主相關性 = sapply(results$extracted_articles, function(x) x$置信度$店主相關性),
  原文 = sapply(results$extracted_articles, function(x) x$原文),
  stringsAsFactors = FALSE
)

cat(sprintf("Total extracted articles: %d\n", nrow(shopkeeper_df)))

# Step 2: Filter for articles containing shopkeeper terms
# This additional filter catches any articles that passed through extraction
# but may have weak shopkeeper relevance
shopkeeper_terms <- "坊主|店主|棧主|莊主|號主|行主|鋪主|館主|店主婦|店東|東家|東主|店家|掌櫃|商戶|業主"

shopkeeper_filtered <- shopkeeper_df %>%
  filter(str_detect(原文, shopkeeper_terms))

cat(sprintf("Articles with shopkeeper terms: %d\n", nrow(shopkeeper_filtered)))
cat(sprintf("Filtered out: %d\n", nrow(shopkeeper_df) - nrow(shopkeeper_filtered)))

# Step 3: Save the filtered results
write.csv(shopkeeper_filtered, 
          "shopkeeper_articles_filtered.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

cat("\nFiltered articles saved to: shopkeeper_articles_filtered.csv\n")

# ----------------------------------------------------------------------------
# 9.4 OPTIONAL: Get ALL segmented articles (not just shopkeeper-related)
# ----------------------------------------------------------------------------

cat("\n=== Processing All Segments ===\n")

# Convert all segmented articles to data frame
all_segments_df <- data.frame(
  article_num = sapply(results$all_articles, function(x) x$article_num),
  doc_id = sapply(results$all_articles, function(x) x$doc_id),
  text = sapply(results$all_articles, function(x) x$text),
  confidence = sapply(results$all_articles, function(x) x$confidence),
  stringsAsFactors = FALSE
)

# Filter for shopkeeper terms
segments_with_shopkeepers <- all_segments_df %>%
  filter(str_detect(text, shopkeeper_terms))

cat(sprintf("Total segments: %d\n", nrow(all_segments_df)))
cat(sprintf("Segments with shopkeeper terms: %d\n", nrow(segments_with_shopkeepers)))

# ----------------------------------------------------------------------------
# 9.5 QUALITY CONTROL: Identify missed segments
# ----------------------------------------------------------------------------

cat("\n=== Quality Control: Checking for Missed Segments ===\n")

# Compare what was extracted vs. what was segmented
extracted_texts <- shopkeeper_filtered$原文
segment_texts <- segments_with_shopkeepers$text

# Find segments that didn't make it to extraction
missing_segments <- segments_with_shopkeepers %>%
  filter(!text %in% extracted_texts)

cat(sprintf("Segments that didn't make it to extraction: %d\n", nrow(missing_segments)))

# Analyze why segments were excluded
if (nrow(missing_segments) > 0) {
  # Check length distribution
  missing_segments$text_length <- nchar(missing_segments$text)
  shopkeeper_filtered$text_length <- nchar(shopkeeper_filtered$原文)
  
  cat("\n--- Missing Segments Length Summary ---\n")
  print(summary(missing_segments$text_length))
  
  cat("\n--- Included Articles Length Summary ---\n")
  print(summary(shopkeeper_filtered$text_length))
  
  # Show a few examples
  cat("\n=== Sample of Missing Segments ===\n")
  for (i in 1:min(3, nrow(missing_segments))) {
    cat(sprintf("\nMissing segment %d (length: %d):\n", 
                i, missing_segments$text_length[i]))
    cat(missing_segments$text[i])
    cat("\n---\n")
  }
  
  # ----------------------------------------------------------------------------
  # 9.6 RECOVERY: Add back short missing segments
  # ----------------------------------------------------------------------------
  # After manual review, we determined that segments < 610 characters
  # that were missed should be included
  
  cat("\n=== Recovering Short Missing Segments ===\n")
  
  # Filter to keep only short segments
  missing_segments_short <- missing_segments %>%
    filter(text_length < 610)
  
  cat(sprintf("Missing segments with < 610 characters: %d\n", 
              nrow(missing_segments_short)))
  cat(sprintf("Filtered out (>= 610 characters): %d\n", 
              nrow(missing_segments) - nrow(missing_segments_short)))
  
  # Prepare missing segments in same format as shopkeeper_filtered
  missing_to_add <- data.frame(
    案件編號 = NA,
    文檔來源 = missing_segments_short$doc_id,
    案件類型 = "其他",           # Other
    商鋪類型 = "未明",           # Unknown
    分段可靠性 = "Low",          # Low confidence
    店主相關性 = "Medium",       # Medium relevance
    原文 = missing_segments_short$text,
    stringsAsFactors = FALSE
  )
  
  # Combine with existing
  shopkeeper_complete <- bind_rows(shopkeeper_filtered, missing_to_add)
  
  cat(sprintf("Original shopkeeper_filtered: %d\n", nrow(shopkeeper_filtered)))
  cat(sprintf("Added short missing segments: %d\n", nrow(missing_to_add)))
  cat(sprintf("Total combined: %d\n", nrow(shopkeeper_complete)))
  
  # Save the combined file
  write.csv(shopkeeper_complete, 
            "shopkeeper_complete.csv", 
            row.names = FALSE, 
            fileEncoding = "UTF-8")
  
  cat("\nComplete dataset saved to: shopkeeper_complete.csv\n")
  
} else {
  # No missing segments - use filtered version as complete
  shopkeeper_complete <- shopkeeper_filtered
  
  write.csv(shopkeeper_complete, 
            "shopkeeper_complete.csv", 
            row.names = FALSE, 
            fileEncoding = "UTF-8")
  
  cat("\nNo missing segments found. Complete dataset saved.\n")
}

# ----------------------------------------------------------------------------
# 9.7 PREPARE FINAL OUTPUT FOR TOKENIZATION
# ----------------------------------------------------------------------------

cat("\n=== Preparing Final Output for Tokenization ===\n")

# Select only necessary columns for tokenization
# 文檔來源 (doc_id) and 原文 (text) are needed for downstream processing
shopkeeper_comp4tok <- shopkeeper_complete %>% 
  select(文檔來源, 原文)

# Save for tokenization
write.csv(shopkeeper_comp4tok, 
          "shopkeeper_comp4tok.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

cat("Tokenization-ready file saved to: shopkeeper_comp4tok.csv\n")

# Save workspace for future use
save.image('shopsegment.RData')

cat("\n=== All Processing Complete ===\n")
cat("\nOutput files created:\n")
cat("  1. shenbao_results.json - Complete results in JSON format\n")
cat("  2. shenbao_summary.csv - Summary of extracted articles\n")
cat("  3. shenbao_statistics.txt - Processing statistics report\n")
cat("  4. shopkeeper_articles_filtered.csv - Filtered shopkeeper articles\n")
cat("  5. shopkeeper_complete.csv - Complete dataset with recovered segments\n")
cat("  6. shopkeeper_comp4tok.csv - Ready for tokenization\n")
cat("  7. shopsegment.RData - Saved workspace\n\n")

# ============================================================================
# END OF SCRIPT
# ============================================================================
#
# NOTES FOR USERS:
# ---------------
# 1. PURPOSE:
#    This script solves the problem of poorly segmented long documents in
#    historical Chinese newspapers. Multiple articles were published
#    continuously without clear boundaries, causing digitization processes
#    to incorrectly aggregate them into single documents.
#
# 2. THREE-STAGE PIPELINE:
#    Stage 1 (FILTER): Pre-screens documents for shopkeeper relevance
#    Stage 2 (SEGMENT): Splits documents using hierarchical boundary markers
#    Stage 3 (EXTRACT): Identifies and extracts shopkeeper-specific articles
#
# 3. SEGMENTATION MARKERS (Hierarchical):
#    Level 1 (Strongest): ○ bullet points, institutional referrals
#    Level 2 (Strong): Temporal markers, court proceedings, filing actions
#    Level 3 (Moderate): Dispositions, ceremonial closings
#    Level 4 (Weak): Narrative elements (testimony, plaintiff/defendant)
#
# 4. CONFIDENCE ASSESSMENT:
#    High: Has institutional referral OR (strong opening AND strong closing)
#    Medium: Has strong opening OR strong closing OR testimony
#    Low: Lacks strong structural indicators
#
# 5. POST-PROCESSING WORKFLOW:
#    a. Extract articles from results
#    b. Filter for shopkeeper term presence
#    c. Identify missed segments through comparison
#    d. Manually review missed segments
#    e. Recover short segments after verification
#    f. Prepare final dataset for tokenization
#
# 6. QUALITY CONTROL:
#    The script includes multiple quality checks:
#    - Minimum length thresholds (>20 characters for segments)
#    - Pattern-based filtering for market reports
#    - Comparison of segmented vs. extracted articles
#    - Manual review opportunity for edge cases
#    - Length-based recovery of missed segments (<610 characters)
#
# 7. ADAPTING TO OTHER CORPORA:
#    To adapt this script for other historical Chinese newspapers:
#    - Review and modify PATTERNS based on your corpus conventions
#    - Adjust minimum length thresholds (currently 20 characters)
#    - Modify confidence assessment logic if needed
#    - Update case classification categories
#    - Adjust recovery threshold (currently 610 characters)
#
# 8. PERFORMANCE CONSIDERATIONS:
#    - Progress printed every 100 documents
#    - Results saved incrementally (JSON, CSV, TXT)
#    - Workspace saved for recovery if needed
#    - Memory-efficient list-based processing
#
# 9. OUTPUT FILES:
#    JSON: Complete structured results with all metadata
#    CSV: Tabular format for analysis in spreadsheets/R/Python
#    TXT: Human-readable statistics report
#
# 10. NEXT STEPS:
#     After running this script:
#     - Use shopkeeper_comp4tok.csv for Chinese word segmentation
#     - Use shopkeeper_complete.csv for analysis
#     - Review confidence levels to prioritize high-confidence articles
#     - Conduct further filtering based on case types if needed
#
# ============================================================================
