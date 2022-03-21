library(rvest) # for scraping
library(stringr) # for string manipulation
library(stringi) # for string manipulation pt.2
library(shiny)
library(ggplot2)
library(reshape2)
library(data.table)
library(tidyr)
#install.packages("tcltk")
library(shinythemes)




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
#remove on wrongly scriped date
dt.pub.collab$year <- ifelse(dt.pub.collab$year == "November 2013", "2013", dt.pub.collab$year)




#-------------------------------------------------------------------------------

# Save object as .Rdata file
save(dt.pub.collab, file = "NovaNetworkData.RData")

#-------------------------------------------------------------------------------
#analysis

#cum of titles per year
dt.pub.collab[, n_titles := .N, by=author]
dt.pub.collab[, n_authors := .N, by=list(title, year)]


#-------------------------------------------------------------------------------
# SHINY APP LAYOUT
#-------------------------------------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # Set theme
  theme = shinytheme("cosmo"),
  
  # Set slider color
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #C6007E;
                                                                                                                               border: #C6007E}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #C6007E;
                                                                                                                               border: #C6007E}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #C6007E;
                                                                                                                               border: #C6007E}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-from, .js-irs-3 .irs-to, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #C6007E;
                                                                                                                               border: #C6007E}")),
  
  
  # App title ----
  titlePanel(title = div(img(src="Title_NOVA_PNA.png"))),
  
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
      #save as integer, cause it made some mess in range step by 0.1 
      sliderInput("year.range", "Range of years:",
                  min = as.integer(min(dt.pub.collab$year)), max = as.integer(max(dt.pub.collab$year)),
                  value = c(as.integer(min(dt.pub.collab$year)),as.integer(max(dt.pub.collab$year))), step = 1),
      
      #Input: Specify bin width with histogram graphs
      sliderInput("bin.width", "Bin width:",
                  min = 1, max = 8,
                  value = 5, step = 0.1),
      
      #Inputs: degree slicer
      sliderInput("degree", "Degree:",
                  min = 0, max = 25,
                  value = 25),
      #Inputs: weight threshold slicer
      sliderInput("weight.threshold", "Weight threshold:",
                  min = 1, max = 4,
                  value = 3),
      ,width = 3),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset 1 analysis, 2 analysis, 3 analysis
      tabsetPanel(type = "tabs",
                  
                  ###1. STATIC ANALYSIS DASHBOARD
                  tabPanel("Descriptive Analysis",
                  
                    fluidRow(         
                            
                      column(4,
                             br(),
                             #basic stats values
                             tableOutput("basic.analysis")
                            ),
                             
                      column(8,
                             #Description
                             br(),
                             strong("Description:"),
                             div("Our first analysis page displays some descriptive data statistics about
                             the article publications of authors at NOVA SBE and related to NOVA SBE. You
                                 are able to adjust the time range via the slider on the left panel."),
                             br(),
                             strong("Proposed configuration:"),
                             div("Nova authors --> True, Other authors --> True, Range of years --> All, 
                                 Bin width --> Not applicable, Degree --> Not applicable, Weight threshold --> Not applicable")
                             )
                    ),
                    fluidRow(
                      # Output: Histogram
                      plotOutput(outputId = "works.year.plot"),
                      
                      # Output: Maartens cumsum of publications
                      plotOutput(outputId = "cumsum.title.plot")
                    )
                      
                  ),
                  
                  ###2. EXPLORATORY ANALYSIS - DYANAMIC
                  tabPanel("Network Exploration",
                  
                    fluidRow(
                      column(6,
                             # Output: Degree centrality
                             plotOutput(outputId = "degree.centrality",
                                        width = "550px", height = "550px")
                      ),
                      column(6,
                        #Description
                        br(),
                        strong("Description:"),
                        div("Our second analysis page displays the network of authors at NOVA SBE and
                            their collaborating authors from other institutions. You can zoom in on the
                            clusters of the network to the left by selecting and double-clicking (double-
                            click again to zoom back out again)."),
                        br(),
                        strong("Proposed configuration:"),
                        div("Nova authors --> True, Other authors --> False, Range of years --> 2019-2022, 
                            Bin width --> 1, Degree --> 3, Weight threshold --> Not applicable")
                      )
                    ),
                    fluidRow(
                      column(6,
                        # Output: Degree distribution
                        plotOutput(outputId = "degree.distribution", height = "200px")
                      ),
                      column(6,
                        #Output: Clustering coefficient
                        plotOutput(outputId = "clustering.coefficient", height = "200px")
                      )
                    )
                  ),
    
                  
                  ###3. NETWORK ANALYSIS
                  tabPanel("Third analysis",
                           
                            # Output: Network analysis graph
                            plotOutput(outputId = "network.analysis.graph"),
                            
                            #table of names (edges)
                            tableOutput("network.edges")
                          
                          )
      )
    ,width = 9)
  )
)

#-------------------------------------------------------------------------------
# SHINY SERVER
#-------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #reactive expression to create filtered data table
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
  
  #reactive graphs filtered with new data table-----------------------
  g.authors <- reactive({
    
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    #preparing vertices for bipartite projection
    all.authors <- dt.pub.collab.range[, list(name=unique(author), type=TRUE)]
    all.titles <- dt.pub.collab.range[, list(name=unique(title), type=FALSE)]
    all.vertices <- rbind(all.authors, all.titles)
    #bipartite projection
    g.thesis <- graph.data.frame(dt.pub.collab.range[, list(author, title)], directed=FALSE, vertices=all.vertices)
    g.authors <- bipartite.projection(g.thesis)$proj2
    V(g.authors)$degree <- degree(g.authors)
    g.authors
    
  })
  #table of basic analysis---------------------------------------
  output$basic.analysis <- renderTable({
    
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    #unique table for analysis
    dt.unique.authors <- unique(dt.pub.collab.range[, .(author, n_titles, nova_author)])
    
    
    ####1. STATIC ANALYSIS DASHBOARD
    # This part does not need filtering, should just show up
    # in a section called "About the source dataset"
    #creating dataset for analysis
    data.frame(
      Descriptive_Statistics = c("Number of authors",
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
                             round(dt.unique.authors[nova_author == 'yes', mean(n_titles)], 
                                   digits=2),
                             #Average number of publications per non-NOVA author in the network
                             round(dt.unique.authors[nova_author == 'no', mean(n_titles)],
                                   digits=2),
                             #Average number of authors per paper
                             round(dt.pub.collab[, .N, by= title][, mean(N)])), digits=2),
      stringsAsFactors = FALSE)
  })
  
  
  #histogram to plot output----------------------------------------
  output$works.year.plot <- renderPlot({
    
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    #plotting histogram of titles per year
    ggplot() + geom_histogram(aes(x=dt.pub.collab.range[,list(unique(title),year)]$year), 
                              fill = "#C6007E", stat = "count") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      labs(x="Year", y = "Count of publications") +
      ggtitle("Number of Author Publications over the Years")
    
  })
  

  #cumsum plot of titles-------------------------------------------
  output$cumsum.title.plot <- renderPlot({
    
    #loading filtered reactive data table
    dt.pub.collab.range <- dt()
    
    # Get publications and cumulative publications per year
    dt.pub.collab.stats <- dt.pub.collab.range[, .(n_pub = length(unique(title))), 
                                               by = year][order(year)]
    dt.pub.collab.stats <- dt.pub.collab.stats[, csum_n_pub := cumsum(n_pub)]

    
    # Process & plot
    df.plot <- tidyr::pivot_longer(dt.pub.collab.stats, cols=c("n_pub","csum_n_pub"),
                                    names_to="variable", values_to="value")
    # Colours
    colours <- c(rep(c("black","#C6007E"), 11))
    
    ggplot(df.plot, aes(x=year, y=value, fill=variable)) +
      geom_bar(stat="identity", position="dodge") + 
      theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold")) +
      labs(x="Year", y="Cumulative count of publications") +
      scale_fill_manual(values=colours) +
      ggtitle("Cumulative Number of Author Publications over the Years")
  })
  
  
  ###2. EXPLORATORY ANALYSIS - DYANAMIC
  # This part does need filtering
  # Filter 1: YEAR INTERVAL
  # Filter 2: binwidth
  # Filter 3: DEGREE FILTER
  
  #histogram degree distribution--------------------------------------
  output$degree.distribution <- renderPlot({
    #filtering bin width
    bin.width <- input$bin.width
    
    #adding filtered table by date and nova or not nova
    g.authors <- g.authors()
    
    # Degree distribution histogram (filter1, filter2)
    qplot(degree(g.authors), geom="histogram", binwidth = bin.width) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      ggtitle("Degree Distribution of Author Publications")
  })
  
  #Authors with higher degree centrality------------------------------------
  output$degree.centrality <- renderPlot({
    
    #adding filtered table by date and nova or not nova
    g.authors <- g.authors()
    
    #input degree slicer
    degree_slider <- input$degree
    
    # Identify and Plot the authors with highest degree centrality
    g.authors.highest <- induced.subgraph(g.authors, vids = V(g.authors)$degree > degree_slider)

    plot(g.authors.highest)
  })
  
  
  #clustering coefficient plot----------------------------------------
  output$clustering.coefficient <- renderPlot({
    
    #adding filtered table by date and nova or not nova
    g.authors <- g.authors()
    
    # Clustering Coefficient distribution histogram (filter1 only)
    qplot(transitivity(g.authors, type="local"), geom="histogram")
  })
  
  output$network.analysis.graph <- renderPlot({
    
    ###3. Network Analysis (more elaborate) - DYANAMIC
    # This part does need filtering
    # Filter 1: YEAR INTERVAL
    # Filter 2: WEIGHT THRESHOLD SLIDER. Weight is a measure of possible connection between two profs.
    
    #NOVA Authors and their interactions. Which NOVA profs might connect on a paper soon?
    
    #loading range table
    dt.pub.collab.range <- dt()
    
    #adding filtered table by date and nova or not nova
    dt.authors.nova <- dt.pub.collab.range[nova_author == 'yes', list(name=unique(author), type=TRUE)]
    dt.titles.nova  <- dt.pub.collab.range[nova_author == 'yes', list(name=unique(title), type=FALSE)]
    dt.vertices.nova  <- rbind(dt.authors.nova, dt.titles.nova)
    
    g.thesis.nova <- graph.data.frame(dt.pub.collab.range[nova_author == 'yes', list(author, title)], directed=FALSE, vertices=dt.vertices.nova)
    g.authors.nova <- bipartite.projection(g.thesis.nova)$proj2
    
    #adding weight threshold slider to the plot input
    weight.threshold.slider <- input$weight.threshold
    
    #for test
    #weight.threshold.slider <- 3
    
    m.predicted.edges <-  as.matrix(cocitation(g.authors.nova) * (1-get.adjacency(g.authors.nova)))
    g.predicted.edges <-  graph_from_adjacency_matrix(m.predicted.edges, mode = "undirected", weighted = TRUE)
    
    #Remove authors that have degree zero and thus no possible cooperation.
    Isolated = which(degree(g.predicted.edges)==0)
    G2 = delete.vertices(g.predicted.edges, Isolated)
    
    #set width for plotting
    E(G2)$width <- E(G2)$weight
    
    
    E(G2)[which(E(G2)$weight < weight.threshold.slider)]$color <- "lightgrey"
    E(G2)[which(E(G2)$weight >= weight.threshold.slider)]$color <- "green"
    
    plot(G2, canvas.width = 1400, canvas.height = 720, layout = layout_with_kk(G2, maxiter = 15000, kkconst = 140),
           vertex.size = 3, vertex.label.dist=1)
  })
  
  output$network.edges <- renderTable({
    
    #loading range table
    dt.pub.collab.range <- dt()
    dt.pub.collab.range <- dt.pub.collab
    #adding filtered table by date and nova or not nova
    dt.authors.nova <- dt.pub.collab.range[nova_author == 'yes', list(name=unique(author), type=TRUE)]
    dt.titles.nova  <- dt.pub.collab.range[nova_author == 'yes', list(name=unique(title), type=FALSE)]
    dt.vertices.nova  <- rbind(dt.authors.nova, dt.titles.nova)
    
    g.thesis.nova <- graph.data.frame(dt.pub.collab.range[nova_author == 'yes', list(author, title)], directed=FALSE, vertices=dt.vertices.nova)
    g.authors.nova <- bipartite.projection(g.thesis.nova)$proj2
    
    #adding weight threshold slider to the plot input
    weight.threshold.slider <- input$weight.threshold
    
    m.predicted.edges <-  as.matrix(cocitation(g.authors.nova) * (1-get.adjacency(g.authors.nova)))
    g.predicted.edges <-  graph_from_adjacency_matrix(m.predicted.edges, mode = "undirected", weighted = TRUE)
    
    #get edges where weight is high, thus above threshold.
    # This is basically a textual version of the marked edges in the graph.
    data.frame(
      Connection = as.character((c(as_ids(E(g.predicted.edges)[[ weight >= weight.threshold.slider ]]))),
                           stringsAsFactors = FALSE)
    )
    
  })
}

#examples for ideas
#runExample("01_hello")      # a histogram
#runExample("02_text")       # tables and data frames
#runExample("03_reactivity") # a reactive expression
#runExample("04_mpg")        # global variables
#runExample("05_sliders")    # slider bars
#runExample("06_tabsets")    # tabbed panels
#runExample("07_widgets")    # help text and submit buttons
#runExample("08_html")       # Shiny app built from HTML
#runExample("09_upload")     # file upload wizard
#runExample("10_download")   # file download wizard
#runExample("11_timer")      # an automated timer

# See above for the definitions of ui and server

shinyApp(ui = ui, server = server)
