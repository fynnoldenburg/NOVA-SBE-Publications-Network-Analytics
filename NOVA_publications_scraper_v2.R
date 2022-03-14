library(rvest) # for scraping
library(stringr) # for string manipulation
library(stringi) # for string manipulation pt.2

# Get NOVA publications html content
scrape <- read_html("https://www.novasbe.unl.pt/en/faculty-research/research/publications")

# Get the two accordions for articles and books publications
accord_articles <- scrape %>% html_nodes("[class='accordion js-accordion']") %>% .[1]

# Get raw articles html
articles <- accord_articles %>% html_nodes("p") %>% html_text()

# Initialize final matrix
m_pub_collab <- matrix(ncol=3)

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
  article_authors     <- str_replace_all(article_authors_raw, "[^[:alnum:],]", "") %>% stri_trans_general(id = "Latin-ASCII")
  
  # Create sub-matrix for article
  m_article <- cbind(article_authors, article_title, article_year)
  
  # Append to final data frame
  m_pub_collab <- rbind(m_pub_collab, m_article)
} 

# Build data table from matrix
df_pub_collab <- as.data.frame(m_pub_collab) 
colnames(df_pub_collab) <- c("author", "title", "year")

# Remove NA
df_pub_collab <- na.omit(df_pub_collab)
rownames(df_pub_collab) <- NULL

# Remove mistakes (some authors that couldnt be split)
#df_pub_collab <- df_pub_collab[! (df_pub_collab$author.count(",") > 1), ]

#-------------------------------------------------------------------------------

# Get NOVA authors by tag "strong"
nova_authors_raw  <- accord_articles %>% html_nodes("strong") %>% html_text() %>% stri_trans_general(id = "Latin-ASCII")

# Remove all special characters except ","
nova_authors_raw2 <- str_replace_all(nova_authors_raw, "[^[:alnum:],]", "")

# Remove all instances with more than 1 "," (meaning there were two authors in strong tag)
nova_authors_raw_3 <- Filter(function(x) str_count(x, ",") == 1, nova_authors_raw2)

# Get unique nova authors
nova_authors <- unique(nova_authors_raw_3)

# Add nova authors column to df_pub_collab
df_pub_collab$nova_author <- ifelse(df_pub_collab$author %in% nova_authors, "yes", "no")

#-------------------------------------------------------------------------------

# Count number of publications per author
pubs_count <- as.data.frame(table(df_pub_collab$author))

# Merge to df_pub_collab
df_pub_collab <- merge(df_pub_collab, pubs_count, by.x="author", by.y="Var1")
df_pub_collab

#-------------------------------------------------------------------------------

# Save object as .Rdata file
save(df_pub_collab, file = "NovaNetworkData.RData")
