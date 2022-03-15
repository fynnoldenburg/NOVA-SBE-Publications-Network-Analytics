library(rvest) # for scraping
library(stringr) # for string manipulation
library(stringi) # for string manipulation pt.2
library(shiny)
library(ggplot2)
library(data.table)
library(igraph)



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
  
  # Get year inbetween first brackets
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

# Remove NA
dt.pub.collab <- na.omit(dt.pub.collab)
rownames(dt.pub.collab) <- NULL

# Remove mistakes (some authors that couldnt be split)
#dt.pub.collab <- dt.pub.collab[! (dt.pub.collab$author.count(",") > 1), ]

#-------------------------------------------------------------------------------

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


#-------------------------------------------------------------------------------
# Set "Accepted/ in press" to 2022 and create new column with "Accepted/ in press" info
dt.pub.collab$accepted_inpress <- ifelse(dt.pub.collab$year == "Accepted/In press", "yes", "no")
dt.pub.collab$year <- ifelse(dt.pub.collab$year == "Accepted/In press", "2022", dt.pub.collab$year)

# Remove "EXPH" assigned years and reclassify one instance
dt.pub.collab <- dt.pub.collab[! (year == "EXPH"), ]
dt.pub.collab$year <- ifelse(dt.pub.collab$year == "November 2013", "2013", dt.pub.collab$year)
dt.pub.collab$year <- strtoi(dt.pub.collab$year, base=0L)
#dt.pub.collab$year <- lubridate::ymd(dt.pub.collab$year, truncated = 2L)
#dt.pub.collab$year <- as.Date((dt.pub.collab$year, format = "%Y")



#-------------------------------------------------------------------------------

# Save object as .Rdata file
save(dt.pub.collab, file = "NovaNetworkData.RData")

View(dt.pub.collab)

#-------------------------------------------------------------------------------
#analysis

all.authors <- dt.pub.collab[, list(name=unique(author), type=TRUE)]
all.titles <- dt.pub.collab[, list(name=unique(title), type=FALSE)]
all.vertices <- rbind(all.authors, all.titles)

g.thesis <- graph.data.frame(dt.pub.collab[, list(author, title)], directed=FALSE, vertices=all.vertices)
summary(g.thesis) 
g.authors <- bipartite.projection(g.thesis)$proj2
summary(g.authors)
plot(g.authors)

#cum of titles per year
dt.pub.collab[, n_titles := .N, by=author]
dt.pub.collab[, n_authors := .N, by=list(title, year)]


#maarten------------------------------------------------------------------------

###1. STATIC ANALYSIS DASHBOARD
# This part does not need filtering, should just show up
# in a section called "About the source dataset"
dt.unique.authors <- unique(dt.pub.collab[, .(author, n_titles, nova_author)])

#Number of unique authors considered in the network
dt.unique.authors[, .N]

#Number of NOVA Authors
dt.unique.authors[nova_author == 'yes', length(author)]

#Number of not-NOVA Authors
dt.unique.authors[nova_author == 'no', length(author)]

#Average number of publications per NOVA author in the network
dt.unique.authors[nova_author == 'yes', mean(n_titles)]

#Average number of publications per non-NOVA author in the network
dt.unique.authors[nova_author == 'no', mean(n_titles)]

#Average number of authors per paper
dt.pub.collab[, .N, by= title][, mean(N)]





#Shiny app start----------------------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Nova_publication_analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #horizontal decoranation line
      tags$hr(),
      
      #checkbox nova and not nova
      checkboxInput("nova.authors", "Nova authors", TRUE),
      checkboxInput("other.authors", "Other authors", TRUE),
      
      # Input: Specification of range within an year interval ----
      sliderInput("year.range", "Range:",
                  min = min(dt.pub.collab$year), max = max(dt.pub.collab$year),
                  value = c(min(dt.pub.collab$year),max(dt.pub.collab$year))), step = 1,
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #basic stats values
      tableOutput("basic.analysis"),
      
      # Output: Histogram ----
      plotOutput(outputId = "works.year.plot"),
      
      # Output: Maarten shitty cumsum of publications
      plotOutput(outputId = "cumsum.title.plot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  #reactive expression to create filtered data table---------
  dt <- reactive({
    
    #filtering of nova and non nova authors
    #inputs
    nova.authors <- input$nova.authors
    other.authors <- input$other.authors
    
    #filtering nova authors
    if (nova.authors == "TRUE") {
      dt.pub.collab.nova.author <- dt.pub.collab[(nova_author == "yes")]
    } else {
      dt.pub.collab.nova.author <- dt.pub.collab[!(nova_author == "yes"),][!(nova_author == "no"),]
    }
    #filtering non nova authors
    if (other.authors == "TRUE") {
      dt.pub.collab.other.author <-  dt.pub.collab[nova_author == "no"]
    } else {
      dt.pub.collab.other.author <- dt.pub.collab[!(nova_author == "no"),][!(nova_author == "yes"),]
    }
    #appending them together
    dt.pub.collab.author <- rbind(dt.pub.collab.nova.author, dt.pub.collab.other.author)
    
    
    #range from year slicer inputted in new data table
    year.range <- input$year.range
    year.range.min <- min(year.range)
    year.range.max <- max(year.range)
    
    #new filtered data table by range
    dt.pub.collab.range <- dt.pub.collab.author[(year >= year.range.min) & (year <= year.range.max)]
    dt.pub.collab.range
  })
  
  #table of basic analysis---------------------------------------
  output$basic.analysis <- renderTable({
    
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    #unique table for analysis
    dt.unique.authors <- unique(dt.pub.collab.range[, .(author, n_titles, nova_author)])
    
    #creating data frame from analysis
    data.frame(
      Name = c("Number of authors",
               "Number of Nova authors",
               "Number of other authors",
               "Average number of publication per NOVA author",
               "Average number of publications per non-NOVA author",
               "Average number of authors per paper"),
      
      Value = as.character(c(dt.unique.authors[, .N],#Number of unique authors considered in the network
                             #Number of NOVA Authors
                             dt.unique.authors[nova_author == 'yes', length(author)],
                             #Number of not-NOVA Authors
                             dt.unique.authors[nova_author == 'no', length(author)],
                             #Average number of publications per NOVA author in the network
                             dt.unique.authors[nova_author == 'yes', mean(n_titles)],
                             #Average number of publications per non-NOVA author in the network
                             dt.unique.authors[nova_author == 'no', mean(n_titles)],
                             #Average number of authors per paper
                             dt.pub.collab[, .N, by= title][, mean(N)])),
      stringsAsFactors = FALSE)
  })
  
  
  #histogram to plot output----------------------------------------
  output$works.year.plot <- renderPlot({
    
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    #plotting histogram of titles per year
    ggplot() + geom_histogram(aes(x=dt.pub.collab.range[,list(unique(title),year)]$year), stat = "count")
    
  })
  
  #cumsum plot of titles-------------------------------------------
  output$cumsum.title.plot <- renderPlot({
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    #Cumulative count of papers per year; excluding accepted/inpress and bad scrapes
    dt.pub.collab.stats <- dt.pub.collab.range[, .(n_pub = length(unique(title))), by = year][order(year)]
    dt.pub.collab.stats <- dt.pub.collab.stats[, csum_n_pub := cumsum(n_pub)]
    ggplot(data=dt.pub.collab.stats[order(-year)], aes(x=year, y=csum_n_pub, group=1)) +
      geom_line()+
      geom_point()
    
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
