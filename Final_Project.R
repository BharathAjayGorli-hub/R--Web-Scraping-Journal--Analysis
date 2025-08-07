library(rvest)
library(httr)
library(tidyverse)
library(lubridate)
library(purrr)
library(ggplot2)

# Configure scraping parameters
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
delay_sec <- 5

# Helper function for safe extraction
safe_extract <- function(page, selector, attr = NULL) {
  result <- tryCatch({
    if (is.null(attr)) {
      page %>% html_node(selector) %>% html_text(trim = TRUE)
    } else {
      page %>% html_node(selector) %>% html_attr(attr)
    }
  }, error = function(e) NA_character_)
  
  if (length(result) == 0 || is.na(result) || result == "") NA_character_ else result
}

# FUNCTION TO GET ARTICLE LINKS WITH PAGINATION
get_article_urls_with_pagination <- function(pages = 3) {
  base_url <- "https://link.springer.com/journal/13064/articles"
  all_urls <- c()
  
  for (page in 1:pages) {
    url <- ifelse(page == 1, 
                  base_url,
                  paste0(base_url, "?page=", page))
    
    response <- GET(url, user_agent(user_agent), timeout(15))
    if (status_code(response) != 200) {
      message("Failed to load page ", page)
      next
    }
    
    page_content <- read_html(response)
    
    # Extract article links
    article_links <- page_content %>%
      html_nodes(xpath = '//a[contains(@href, "/article/")]') %>%
      html_attr("href") %>%
      na.omit() %>%
      unique() %>%
      url_absolute("https://link.springer.com")
    
    all_urls <- c(all_urls, article_links)
    message("Found ", length(article_links), " articles on page ", page)
    
    Sys.sleep(delay_sec)
  }
  
  unique(all_urls)
}

# IMPROVED ARTICLE DATA EXTRACTOR
extract_complete_article_data <- function(article_url) {
  Sys.sleep(delay_sec)
  
  response <- GET(article_url, user_agent(user_agent), timeout(15))
  if (status_code(response) != 200) {
    message("Failed to load article: ", article_url)
    return(NULL)
  }
  
  article_page <- read_html(response)
  
  # Extract data using safe methods
  title <- safe_extract(article_page, "h1.c-article-title")
  
  authors <- article_page %>%
    html_nodes("ul.c-article-author-list a[data-test='author-name']") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = ", ")
  
  correspondence_author <- safe_extract(article_page, "p#corresponding-author-list a")
  
  correspondence_email <- safe_extract(article_page, "p#corresponding-author-list a", "href") %>%
    gsub("mailto:", "", .)
  
  publish_date <- safe_extract(article_page, "time", "datetime")
  
  abstract <- safe_extract(article_page, "section[aria-labelledby='Abs1'] p")
  
  # Extract keywords - try multiple selectors
  keywords <- article_page %>%
    html_nodes("ul.c-article-subject-list a, .Keyword, .c-article-subject__item") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = ", ")
  
  # If no keywords found, extract meaningful terms from abstract
  if (is.na(keywords) || keywords == "") {
    keyword_phrases <- str_extract_all(abstract, "\\b[A-Z][a-zA-Z]+(?:\\s+[A-Z][a-zA-Z]+)*\\b")[[1]]
    keywords <- keyword_phrases %>%
      unique() %>%
      head(10) %>%  # Limit to 10 keywords
      paste(collapse = ", ")
  }
  
  # Extract publish year from date
  publish_year <- ifelse(!is.na(publish_date), 
                         year(ymd(publish_date)), 
                         NA_integer_)
  
  # Return as tibble with proper NA handling
  tibble(
    Title = title,
    Authors = ifelse(is.na(authors) || authors == "", NA_character_, authors),
    Correspondence_Author = correspondence_author,
    Correspondence_Email = correspondence_email,
    Publish_Date = publish_date,
    Abstract = ifelse(is.na(abstract) || abstract == "", NA_character_, abstract),
    Keywords = ifelse(is.na(keywords) || keywords == "", NA_character_, keywords),
    Publish_Year = publish_year,
    URL = article_url
  )
}

# MAIN SCRAPING FUNCTION
scrape_complete_articles <- function(min_articles = 20) {
  message("Starting comprehensive scrape of Neural Development articles...")
  
  # Get article URLs with pagination
  article_urls <- get_article_urls_with_pagination(pages = 3)
  
  if (length(article_urls) < min_articles) {
    message("Warning: Only found ", length(article_urls), " articles")
  }
  
  # Process articles with progress bar
  results <- map_dfr(article_urls, function(url) {
    message("Processing: ", url)
    tryCatch({
      extract_complete_article_data(url)
    }, error = function(e) {
      message("Error processing: ", url, " - ", conditionMessage(e))
      NULL
    })
  }, .id = "Article_ID")
  
  # Clean and process results
  if (nrow(results) > 0) {
    final_data <- results %>%
      mutate(
        Publish_Date = ymd(Publish_Date),
        Publish_Year = year(Publish_Date),
        # Clean up keywords formatting
        Keywords = str_replace_all(Keywords, "\\s*,\\s*", ", "),
        Keywords = ifelse(is.na(Keywords), 
                          str_extract_all(Abstract, "\\b[A-Z][a-zA-Z]+(?:\\s+[A-Z][a-zA-Z]+)*\\b") %>% 
                            map(~paste(head(unique(.), 10), collapse = ", ")) %>% 
                            unlist(),
                          Keywords)
      ) %>%
      distinct(URL, .keep_all = TRUE) %>%
      select(Title, Authors, Correspondence_Author, Correspondence_Email, 
             Publish_Date, Abstract, Keywords, Publish_Year, URL)
    
    # Save results
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("neural_development_articles_", timestamp, ".csv")
    write_csv(final_data, filename)
    message("Successfully saved ", nrow(final_data), " articles to ", filename)
    
    return(final_data)
  } else {
    message("No articles were successfully scraped")
    return(NULL)
  }
}

# RUN THE SCRAPER AND CREATE VISUALIZATIONS
complete_articles <- scrape_complete_articles(min_articles = 20)

if (!is.null(complete_articles)) {
  # Prepare data for visualizations
  articles_long <- complete_articles %>%
    mutate(Keywords = ifelse(is.na(Keywords), "NA", Keywords)) %>%
    separate_rows(Keywords, sep = ",\\s*") %>%
    filter(Keywords != "NA")
  
  # 1. Number of Articles Published by Year (now as line graph)
  articles_by_year <- complete_articles %>%
    filter(!is.na(Publish_Year)) %>%
    count(Publish_Year, name = "Count") %>%
    complete(Publish_Year = seq(min(Publish_Year), max(Publish_Year), by = 1), 
             fill = list(Count = 0))
  
  p1 <- ggplot(articles_by_year, aes(x = Publish_Year, y = Count)) +
    geom_line(color = "#4E79A7", linewidth = 1.5) +  # Changed from size to linewidth
    geom_point(color = "#4E79A7", size = 3) +
    labs(title = "Number of Articles Published by Year",
         x = "Year",
         y = "Number of Articles") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    scale_x_continuous(breaks = seq(min(articles_by_year$Publish_Year), 
                                    max(articles_by_year$Publish_Year), by = 1))
  
  # 2. Top Keyword Appearances (without numbers)
  top_keywords <- articles_long %>%
    count(Keywords, sort = TRUE) %>%
    slice_head(n = 20)  # Top 20 keywords
  
  p2 <- ggplot(top_keywords, aes(x = reorder(Keywords, n), y = n)) +
    geom_col(fill = "#E15759", width = 0.7) +
    labs(title = "Top 20 Keyword Appearances",
         x = "Keywords",
         y = "Number of Appearances") +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.y = element_text(margin = margin(r = 5))
    ) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  
  # Display plots
  print(p1)
  print(p2)
  
  # Save plots
  ggsave("articles_by_year_line.png", p1, width = 10, height = 6, dpi = 300)
  ggsave("keyword_appearances_clean.png", p2, width = 10, height = 6, dpi = 300)
  
  # Summary statistics
  cat("\nScraping Summary:\n")
  cat("----------------\n")
  cat("Total articles:", nrow(complete_articles), "\n")
  cat("Articles with abstracts:", sum(!is.na(complete_articles$Abstract)), "\n")
  cat("Articles with keywords:", sum(!is.na(complete_articles$Keywords)), "\n")
  cat("Date range:", 
      as.character(min(complete_articles$Publish_Date, na.rm = TRUE)), 
      "to", 
      as.character(max(complete_articles$Publish_Date, na.rm = TRUE)), "\n")
  
  # Show top keywords
  cat("\nTop Keywords:\n")
  print(top_keywords)
}