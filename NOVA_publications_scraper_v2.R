library(rvest) # for scraping
library(stringr) # for string manipulation
library(stringi) # for string manipulation pt.2
library(shiny)
library(ggplot2)
library(data.table)
library(lubridate)



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
dt_pub_collab <- as.data.table(m_pub_collab) 
colnames(dt_pub_collab) <- c("author", "title", "year")

# Remove NA
dt_pub_collab <- na.omit(dt_pub_collab)
rownames(dt_pub_collab) <- NULL

# Remove mistakes (some authors that couldnt be split)
#dt_pub_collab <- dt_pub_collab[! (dt_pub_collab$author.count(",") > 1), ]

#-------------------------------------------------------------------------------

# Get NOVA authors by tag "strong"
nova_authors_raw  <- accord_articles %>% html_nodes("strong") %>% html_text() %>% stri_trans_general(id = "Latin-ASCII")

# Remove all special characters except ","
nova_authors_raw2 <- str_replace_all(nova_authors_raw, "[^[:alnum:],]", "")

# Remove all instances with more than 1 "," (meaning there were two authors in strong tag)
nova_authors_raw_3 <- Filter(function(x) str_count(x, ",") == 1, nova_authors_raw2)

# Get unique nova authors
nova_authors <- unique(nova_authors_raw_3)

# Add nova authors column to dt_pub_collab
dt_pub_collab$nova_author <- ifelse(dt_pub_collab$author %in% nova_authors, "yes", "no")


#-------------------------------------------------------------------------------
# Set "Accepted/ in press" to 2022 and create new column with "Accepted/ in press" info
dt_pub_collab$accepted_inpress <- ifelse(dt_pub_collab$year == "Accepted/In press", "yes", "no")
dt_pub_collab$year <- ifelse(dt_pub_collab$year == "Accepted/In press", "2022", dt_pub_collab$year)

# Remove "EXPH" assigned years and reclassify one instance
dt_pub_collab <- dt_pub_collab[! (year == "EXPH"), ]
dt_pub_collab$year <- ifelse(dt_pub_collab$year == "November 2013", "2013", dt_pub_collab$year)
dt_pub_collab$year <- strtoi(dt_pub_collab$year, base=0L)
#dt_pub_collab$year <- lubridate::ymd(dt_pub_collab$year, truncated = 2L)
#dt_pub_collab$year <- as.Date((dt_pub_collab$year, format = "%Y")



#-------------------------------------------------------------------------------

# Save object as .Rdata file
save(dt_pub_collab, file = "NovaNetworkData.RData")

View(dt_pub_collab)

#-------------------------------------------------------------------------------
#analysis

all.authors <- dt_pub_collab[, list(name=unique(author), type=TRUE)]
all.titles <- dt_pub_collab[, list(name=unique(title), type=FALSE)]
all.vertices <- rbind(all.authors, all.titles)

g.thesis <- graph.data.frame(dt_pub_collab[, list(author, title)], directed=FALSE, vertices=all.vertices)
summary(g.thesis) 
g.authors <- bipartite.projection(g.thesis)$proj2
summary(g.authors)
plot(g.authors)

#cum of titles per year
dt_pub_collab[, n_titles := .N, by=author]
dt_pub_collab[, n_authors := .N, by=list(title, year)]

#plot
ggplot() + geom_histogram(aes(x=dt_pub_collab[,list(unique(title),year)]$year), stat = "count")


#Shiny app start

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Nova_publication_analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Specification of range within an year interval ----
      sliderInput("year.range", "Range:",
                  min = min(dt_pub_collab$year), max = max(dt_pub_collab$year),
                  value = c(min(dt_pub_collab$year),max(dt_pub_collab$year))), step = 1,
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
 
  output$distPlot <- renderPlot({
    
    #range from year slicer inputed in new data table
    year.range <- input$year.range
    year.range.min <- min(year.range)
    year.range.max <- max(year.range)
    #new filtered datateble by range
    dt_pub_collab_range <- dt_pub_collab[(year >= year.range.min) & (year <= year.range.max)]
    
    #poting histogram of titles per year
    ggplot() + geom_histogram(aes(x=dt_pub_collab_range[,list(unique(title),year)]$year), stat = "count")
    
  })
  
}

#examples for ideas

runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer

# See above for the definitions of ui and server

shinyApp(ui = ui, server = server)
