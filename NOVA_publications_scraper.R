library(rvest) # for scraping
library(stringr) # for string manipulation

# Get NOVA publications html content
scrape <- read_html("https://www.novasbe.unl.pt/en/faculty-research/research/publications")

# Get the two accordions for articles and books publications
accord_articles <- scrape %>% html_nodes("[class='accordion js-accordion']") %>% .[1]

# Get raw articles html
articles <- accord_articles %>% html_nodes("p") %>% html_text()

# Initialize final matrix & author total publications counter
m_pub_collab <- matrix(ncol=4)
l_all_pubs   <- list()

# Iterate over all raw articles
for (article in articles) {
  
  # Get year inbetween first brackets
  article_year <- gsub("[\\(\\)]", "", regmatches(article, gregexpr("\\(.*?\\)", article))[[1]][1])
  
  # Get only first part of article string until year
  article_info <- strsplit(article, split=article_year)[[1]][1]
  
  # Get last part of article after year and remove first three characters
  article_title <- sub("...", "",
                       strsplit(article, split=article_year)[[1]][2])
  
  # Get authors by spliting at ".," to separate authors and clean from all non-
  # alphanumeric characters except ","
  article_authors_raw <- strsplit(article_info, ".,", fixed=TRUE)[[1]]
  article_authors     <- str_replace_all(article_authors_raw, "[^[:alnum:],]", "")
  
  # Append authors to count publications tracker
  l_all_pubs <- c(l_all_pubs, article_authors)
  
  # If more than one author
  if (length(article_authors) > 1) {
  
    # Get unique combinations of authors
    article_authors_combinations <- t(combn(unique(article_authors),2))
    
    # Append publishing year column
    article_authors_combinations <- cbind(article_authors_combinations, article_year)
    
    # Append article title column
    article_authors_combinations <- cbind(article_authors_combinations, article_title)
    
    # Append to final data frame
    m_pub_collab <- rbind(m_pub_collab, article_authors_combinations)
  }
} 

# Build data table from matrix
df_pub_collab <- as.data.frame(m_pub_collab) 
colnames(df_pub_collab) <- c("author_a", "author_b", "year", "title")

# Remove some NA
df_pub_collab <- na.omit(df_pub_collab)
rownames(df_pub_collab) <- NULL

df_pub_collab
#write.csv(df_pub_collab, "/Users/fynn/Desktop/nova_publications_collaborations.csv", row.names=FALSE)

#-------------------------------------------------------------------------------

# Get NOVA authors by tag "strong"
nova_authors_raw  <- accord_articles %>% html_nodes("strong") %>% html_text()

# Remove all special characters except ","
nova_authors_raw2 <- str_replace_all(nova_authors_raw, "[^[:alnum:],]", "")

# Remove all instances with more than 1 "," (meaning there were two authors in strong tag)
nova_authors_raw_3 <- Filter(function(x) str_count(x, ",") == 1, nova_authors_raw2)

# Get unique nova authors
nova_authors <- unique(nova_authors_raw_3)


# Unlist and clean list of all author publication occurrences
l_all_pubs_clean <- Filter(function(x) str_count(x, ",") == 1, unlist(l_all_pubs, recursive=FALSE))
count_publications <- table(unlist(l_all_pubs_clean, recursive=FALSE))

# Put into data frame
df_total_pub_cnt <- data.frame(count_publications)
colnames(df_total_pub_cnt) <- c("author", "count_publications")

# Merge NOVA author information
df_total_pub_cnt["nova_author"] = ifelse(df_total_pub_cnt$author %in% nova_authors, "yes", "no")

df_total_pub_cnt[order(-df_total_pub_cnt$count_publications),]
#write.csv(df_total_pub_cnt, "/Users/fynn/Desktop/nova_publications_totalcount.csv", row.names=FALSE)

