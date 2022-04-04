# NOVA SBE Publications Network Analytics
# Fynn Oldenburg (MSc Business Analytics 2023)


#-----------------------------------------------------------------------------------------------------------------------
# 1. WEB SCRAPING
#-----------------------------------------------------------------------------------------------------------------------
# Scrape data from https://www.novasbe.unl.pt/en/faculty-research/research/publications and process to clean data frame

# Load packages
library(data.table)
library(rvest) # for scraping
library(stringr) # for string manipulation
library(stringi) # for string manipulation

# Get NOVA publications html content
scrape <- read_html("https://www.novasbe.unl.pt/en/faculty-research/research/publications")

# Get the two accordions for articles and books publications
accord.articles <- scrape %>% html_nodes("[class='accordion js-accordion']") %>% .[1]

# Get raw articles html
articles <- accord.articles %>% html_nodes("p") %>% html_text()

# Initialize final matrix
m.pub.collab <- matrix(ncol=3)

# Iterate over all raw articles
for (article in articles) {
  
  # Get year between first brackets
  article.year <- gsub("[\\(\\)]", "", regmatches(article, gregexpr("\\(.*?\\)", article))[[1]][1])
  
  # Get only first part of article string until year
  article.info <- strsplit(article, split=article.year)[[1]][1]
  
  # Get last part of article after year and remove first three characters
  article.title <- sub("...", "",
                       strsplit(article, split=article.year)[[1]][2])
  
  # Get authors by spliting at ".," to separate authors and clean from all non-
  # alphanumeric characters except ","
  article.authors.raw <- strsplit(article.info, ".,", fixed=TRUE)[[1]] 
  article.authors     <- str_replace_all(article.authors.raw, "[^[:alnum:],]", "") %>% stri_trans_general(id = "Latin-ASCII")
  
  # Create sub-matrix for article
  m.article <- cbind(article.authors, article.title, article.year)
  
  # Append to final data frame
  m.pub.collab <- rbind(m.pub.collab, m.article)
} 

# Build data table from matrix
dt.pub.collab <- as.data.table(m.pub.collab) 
colnames(dt.pub.collab) <- c("author", "title", "year")

#-----------------------------------------------------------------------------------------------------------------------

# Get NOVA authors by tag "strong"
nova.authors.raw  <- accord.articles %>% html_nodes("strong") %>% html_text() %>% stri_trans_general(id = "Latin-ASCII")

# Remove all special characters except ","
nova.authors.raw2 <- str_replace_all(nova.authors.raw, "[^[:alnum:],]", "")

# Remove all instances with more than 1 "," (meaning there were two authors in strong tag)
nova.authors.raw.3 <- Filter(function(x) str_count(x, ",") == 1, nova.authors.raw2)

# Get unique nova authors
nova.authors <- unique(nova.authors.raw.3)

# Add nova authors column to dt.pub.collab
dt.pub.collab$nova_author <- ifelse(dt.pub.collab$author %in% nova.authors, "yes", "no")

#-----------------------------------------------------------------------------------------------------------------------

# Remove NA
dt.pub.collab <- na.omit(dt.pub.collab)
rownames(dt.pub.collab) <- NULL

# Replace small misscrapes
dt.pub.collab$author[dt.pub.collab$author == "Cunha,MP"] <- "Cunha,MPE"
dt.pub.collab$author[dt.pub.collab$author == "Clegg,S"] <- "Clegg,SR"

# Set "Accepted/ in press" to 2022 and create new column with "Accepted/ in press" info
dt.pub.collab$accepted_inpress <- ifelse(dt.pub.collab$year == "Accepted/In press", "yes", "no")
dt.pub.collab$year <- ifelse(dt.pub.collab$year == "Accepted/In press", "2022", dt.pub.collab$year)

# Remove "EXPH" assigned years and reclassify one instance
dt.pub.collab <- dt.pub.collab[! (year == "EXPH"), ]
#remove on wrongly scriped date
dt.pub.collab$year <- ifelse(dt.pub.collab$year == "November 2013", "2013", dt.pub.collab$year)

# Save object as .Rdata file
save(dt.pub.collab, file = "NovaNetworkData.RData")


#-----------------------------------------------------------------------------------------------------------------------
# 2. NETWORK VISUALIZATION
#-----------------------------------------------------------------------------------------------------------------------
# Visualize the authors network as a graph

# Load packages
library(igraph)

plot_network <- function(dt.pub.collab, min_verts_size=1, verts_size_range=4, min_edge_weight=10, min_edge_size=0.1,
                         edge_size_range=3){
  # Plots the NOVA SBE authors collaborations network according to input parameter configuration
  #
  # Parameters:
  # ---------------
  # min_verts_size (float): minimum vertex size
  # verts_size_range (float): vertex sizes range
  # min_edge_weight (int): minimum edge weight to be included in viz (removes vertices that have no such edge)
  # min_edge_size (float): minimum edge size
  # edge_size_range (float): edge sizes range
  
  # Preparing vertices for bipartite projection
  all.authors  <- dt.pub.collab[, list(name=unique(author), type=TRUE)]
  all.articles <- dt.pub.collab[, list(name=unique(title), type=FALSE)]
  all.vertices <- rbind(all.authors, all.articles)
  
  # Create bipartite projection in the authors space
  g.authors.articles <- graph.data.frame(dt.pub.collab[, list(author, title)], directed=FALSE, vertices=all.vertices)
  g.authors <- bipartite.projection(g.authors.articles)$proj2
  
  #---------------------------------------------------------------------------------------------------------------------
  
  # Add degree and size attributes to vertices according to min_verts_size & verts_size_range
  V(g.authors)$degree <- degree(g.authors)
  map01 <- function(x){(x-min(x))/(max(x)-min(x))} # map values between 0 and 1
  V(g.authors)$size <- (map01(degree(g.authors))*verts_size_range)+min_verts_size
  
  # Add size attribute to edges according to min_edge_size & edge_size_range
  E(g.authors)$size <- (map01(E(g.authors)$weight)*edge_size_range)+min_edge_size
  
  # Add nova author attribute to vertices
  V(g.authors)$nova_author <- ifelse(V(g.authors)$name %in% nova.authors, TRUE, FALSE)
  
  # Filter vertices that are part of at least one connection with weight larger than min_edge_weight
  eids.weight.cond <- which(E(g.authors)$weight >= min_weight) # get edge indices satisfying min_edge_weight condition
  g.authors.filtered <- subgraph.edges(g.authors, eids = eids.weight.cond)
  
  #---------------------------------------------------------------------------------------------------------------------
  
  # Plot graph network
  plot(g.authors.filtered,
       
       vertex.label = NA,
       vertex.size = V(g.authors.filtered)$size,
       vertex.color = ifelse(V(g.authors.filtered)$nova_author, "#B7A364", "grey"),
       vertex.frame.color = ifelse(V(g.authors.filtered)$nova_author, "black", NA),
       
       edge.width = E(g.authors.filtered)$size)
}


# Plot network configuration
plot_network(dt.pub.collab)


