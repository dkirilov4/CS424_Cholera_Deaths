library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
library(leaflet)
library(dplyr)
library(plotly)
library(rsconnect)
library(shinyjs)

# UI:
ui <- dashboardPage(
  # Header
  dashboardHeader(title = "Cholera Deaths", titleWidth = 350),
  
  # Sidebar
  dashboardSidebar(
    disable = FALSE,
    width = 250,
    
    sidebarMenu(
      menuItem(text = " Visualization", tabName = "vis", icon = icon("area-chart")),
      menuItem(text = "Info", tabName = "info", icon = icon("info"))
    )
  ),
  
  # Body
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-weight: bold;
        font-size: 32px;
      }

      .main-sidebar {
              font-weight: bold;
              font-size: 24px;
            }
    '))),
    
    tabItems(
      tabItem(tabName = "vis",

        fluidRow(
          tabBox(
            id = "visTabSet",
            width = 12,
            
            #Cholera Deaths Vis
            
            tabPanel("Cholera Deaths",
              fluidPage(
                width = 12,
                
                tags$style(type = "text/css", "#lineChart {height: calc(65vh) !important;}"),
                plotlyOutput("lineChart"),
                DT::dataTableOutput("choleraDeathsTable"),
                inlineCSS(list("table" = "font-size: 14px"))
              
              )
            ),
            
            #Cholera Age/Sex Vis
            tabPanel("Age/Sex",
              fluidRow(
                width = 12,
                
                tags$style(type = "text/css", "#choleraAgeSexBarChartMale {height: calc(65vh) !important;}"),
                column(6, plotOutput("choleraAgeSexBarChartMale")),
                tags$style(type = "text/css", "#choleraAgeSexBarChartFemale {height: calc(65vh) !important;}"),
                column(6, plotOutput("choleraAgeSexBarChartFemale"))
              ),
              fluidRow(
                width = 12,
                column(12, DT::dataTableOutput("choleraAgeSexTable"))
              )
            ),
            
            #UK Census Data
            tabPanel("UK Census Data",
               fluidRow(
                 width = 6,
                 
                 tags$style(type = "text/css", "#UKCensusPieChartMale {height: calc(35vh) !important;}"),
                 column(4, plotOutput("UKCensusPieChartMale")),
                 tags$style(type = "text/css", "#UKCensusPieChartFemale {height: calc(35vh) !important;}"),
                 column(4, plotOutput("UKCensusPieChartFemale")),
                 tags$style(type = "text/css", "#UKCensusPieChartTotal {height: calc(35vh) !important;}"),
                 column(4, plotOutput("UKCensusPieChartTotal"))
               ),
               fluidRow(
                 width = 6,
                 tags$style(type = "text/css", "#UKCensusBarChartMale {height: calc(35vh) !important;}"),
                 column(6, plotOutput("UKCensusBarChartMale")),
                 tags$style(type = "text/css", "#UKCensusBarChartFemale {height: calc(35vh) !important;}"),
                 column(6, plotOutput("UKCensusBarChartFemale"))
               ),
               fluidRow(
                 width = 12,
                 column(12, DT::dataTableOutput("UKCensusTable"))
               )
            ),
            
            #Death Locations
            tabPanel("Death Locations",
              # Leaflet Map
              fluidRow(
                width = 12,
                tags$style(type = "text/css", "#choleraDeathsMap {height: calc(80vh) !important;}"),
                column(10, offset = 1, leafletOutput("choleraDeathsMap"))
              )
            )
          )
        )
      ),
      
      tabItem(tabName = "info",
          mainPanel(
            h2(strong("Info")),
            h3(paste("This is a project made by Dimitar Kirilov for CS 424 (Visualization and Visual Analytics).")),
            h2(strong("Libraries")),
            h3(paste("shiny, shinydashboard, DT, ggplot2, reshape2, scales, leaflet, dplyr, plotly")), br(),
            h2(strong("How-To-Use:")),
            h3(paste("The application is relatively straight forward. There is a left navigation bar which links access to either the visualization page or to the info page (this bar can be hidden).  On the visualization page, there are 4 tabs with separate visualizations which the user can analyze.

The first tab - Cholera Deaths - visualizes the number of people affected by the cholera outbreak via a line chart. The chart shows 4 lines - number of people attacked, number of people died, total number of people attacked so far, and total number of people who have died so far. Each line can be hidden by clicking on the appropriate element in the legend, and the user can see a tooltip of the actual data point when hovering over the line. A certain region can also be selected to analyze by dragging the mouse over the desired area on the plot. Under the line chart there is also a table of the data represented which allows for sorting and filtering.

The second tab - Age/Sex - visualizes the different people and their demographic regarding who was affected by the cholera outbreak. The visualization features two bar chart plots, colored blue for males and red for females showing the number of people of each gender and specific age group who were affected by the cholera outbreak. Under the plots there is also a table showing the data visualized.

The third tab - UK Census Data - visualizes the number of men and women in different age groups in that same time period of which the outbreak occurred. The visualization features three pie charts (one for male, one for female, and one for total) which show percent of people affected by the outbreak as well as a bar chart which shows the specific number of people (corresponding to the percentile in the pie charts). Under all the plots there is also a table showing the data visualized.

The fourth tab - Death Locations - visualizes the locations of all the deaths as well as the water pumps. The water pumps are represented as blue circles on the map, and the death locations are represented as red circles. The radius of the red circles represents the number of people who died at that location, and clicking on a circle shows the actual number.")
          ))
      )
    )
  )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
)

# Server Logic:
server <- function(input, output) {
  
  #Font/Text Size:
  #----
  textSizeMultiplier <- 1
  axisTextSize <- 10 * textSizeMultiplier
  axisTitleSize <- 16 * textSizeMultiplier
  plotTitleSize <- 20 * textSizeMultiplier
  legendTextSize <- 10 * textSizeMultiplier
  pieChartTextSize <- 7 * textSizeMultiplier
  
  properTextSizeTheme <- theme(
    plot.title = element_text(size = plotTitleSize, face = "bold"),
    axis.title = element_text(size = axisTitleSize),
    axis.text = element_text(size = axisTextSize),
    legend.text=element_text(size= legendTextSize)
  )
  #----
  
  
  
  # Data
  #----
  choleraDeathsDataset <- read.table(file = "data/choleraDeaths.tsv", sep = "\t", header = TRUE)
  choleraDeathsDataset$TotalDeaths <- cumsum(choleraDeathsDataset$Death)
  choleraDeathsDataset$TotalAttacks <- cumsum(choleraDeathsDataset$Attack)
  choleraDeathsDataset$Date <- as.Date(choleraDeathsDataset$Date, format = "%d-%b-%Y")
  choleraDeathsDataset$Date <- format(as.Date(choleraDeathsDataset$Date), format = "%b-%d-%Y")
  
  meltedCholeraDeathsDataset <- melt(data = choleraDeathsDataset, id.vars = "Date")
  colnames(meltedCholeraDeathsDataset)[3] <- "People"
  
  
  
  choleraAgeSexDataset <- read.table(file = "data/naplesCholeraAgeSexData.tsv", comment.char = '#', sep = "\t", header = TRUE)
  
  UKCensusDataset <- read.csv(file = "data/UKcensus1851.csv", header = TRUE, comment.char = "#")
  
  censusTotals <- rowSums(UKCensusDataset[,c(2,3)])
  UKCensusDataset <- cbind(UKCensusDataset, Total = censusTotals)
  
  UKCensusDatasetMelt <- UKCensusDataset
  UKCensusDatasetMelt$male <- formatC(x = UKCensusDatasetMelt$male, format = "d", big.mark = ",")
  UKCensusDatasetMelt$female <- formatC(x = UKCensusDatasetMelt$female, format = "d", big.mark = ",")
  UKCensusDatasetMelt$Total <- formatC(x = UKCensusDatasetMelt$Total, format = "d", big.mark = ",")
  
  totalMaleFatalities <- sum(UKCensusDataset$male)
  totalFemaleFatalities <- sum(UKCensusDataset$female)
  totalFatalities <- sum(UKCensusDataset$Total)
  
  #totalCensusData <- as.data.frame(matrix(c("Male", "Female", totalMaleFatalities, totalFemaleFatalities), ncol = 2, nrow = 2))
  gender <- c("Male", "Female")
  people <- c(totalMaleFatalities, totalFemaleFatalities)
  
  totalCensusData <- data.frame(gender, people)
  
  #----

  
  
  # Tab 1:
  #----
  
  # Table:
  output$choleraDeathsTable = DT::renderDataTable(choleraDeathsDataset)
  
  # Line Chart
  cbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  output$lineChart <- renderPlotly(
  {
    # Graph:
    ggplot(meltedCholeraDeathsDataset, aes(x = Date, 
                                             y = People, 
                                             color = variable, 
                                             group = variable, 
                                             text = paste("Date: ", Date, "\n", 
                                                          "Num. People: ", People, "\n"))) +
    geom_line(size = 1.5, alpha = 0.7) +
    geom_point() +
    scale_color_manual(values = cbPalette) +
    
    # Margins:
    theme(plot.margin = margin(2, 2, 2, 2, "cm")) +
      
    # Labels:
    labs(title = "Cholera Deaths", x = "Date", y = "Number of People", color = "Legend") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
    properTextSizeTheme
    

    ggplotly(tooltip = "text")
  })
  #----
  
  
  
  # Tab 2 - Cholera Age/Sex Data
  #----
  
    # Tables:
    #====
    output$choleraAgeSexTable = DT::renderDataTable({
      datatable(choleraAgeSexDataset, escape = FALSE,
        options = list(sDom = '<"top">t'))
    })
    
    output$UKCensusTable = DT::renderDataTable({
      datatable(UKCensusDatasetMelt, escape = FALSE,
                options = list(sDom = '<"top">t'))
    })
    #====
  
    # Pie Charts:
    #====
    blankTheme <- 
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = plotTitleSize, face = "bold"),
        axis.title = element_text(size = axisTitleSize),
        axis.text = element_text(size = axisTextSize),
        legend.text=element_text(size= legendTextSize),
      )
    
    output$UKCensusPieChartMale <- renderPlot({
      ggplot(UKCensusDataset, aes(x = "", y = male, fill = age)) +
        labs(title = "Male Population") + 
        geom_bar(width = 1, stat = "identity", color = "grey") +
        coord_polar("y", start = 0) +
        blankTheme +
        theme(axis.text.x = element_blank()) + 
        scale_fill_brewer(type = "seq") + 
        geom_text(aes(label = percent(male / totalMaleFatalities)), position = position_stack(vjust = 0.5), check_overlap = TRUE, size = pieChartTextSize)
    })
    
    output$UKCensusPieChartFemale <- renderPlot({
      ggplot(UKCensusDataset, aes(x = "", y = female, fill = age)) +
        labs(title = "Female Population") + 
        geom_bar(width = 1, stat = "identity", color = "grey") +
        coord_polar("y", start = 0) +
        blankTheme +
        theme(axis.text.x = element_blank()) + 
        scale_fill_brewer(type = "seq", palette = "Reds") + 
        geom_text(aes(label = percent(female / totalFemaleFatalities)), position = position_stack(vjust = 0.5), check_overlap = TRUE, size = pieChartTextSize)
    })
    
    output$UKCensusPieChartTotal <- renderPlot({
      ggplot(totalCensusData, aes(x = "", y = people, fill = gender)) +
        labs(title = "Total Population") + 
        geom_bar(width = 1, stat = "identity", color = "black") +
        coord_polar("y", start = 0) +
        blankTheme +
        theme(axis.text.x = element_blank()) +
        geom_text(aes(label = percent(people / totalFatalities)), position = position_stack(vjust = 0.5), check_overlap = TRUE, size = pieChartTextSize)
    })
    #====
  
    # Bar Charts:
    #====
    
    choleraAgeSexDataset$age <- factor(choleraAgeSexDataset$age, levels = choleraAgeSexDataset$age)
    
    # Age/Sex:
    output$choleraAgeSexBarChartMale <- renderPlot({
      ggplot(choleraAgeSexDataset, aes(x = age, y = male, fill = age)) +
        labs(title = "Male", x = "Age Group", y = "Deaths Per 10,000 People") + 
        properTextSizeTheme +
        geom_bar(width = 0.8, stat = "identity", color = "black") +
        scale_y_continuous(expand = c(0, 0)) + 
        scale_fill_brewer(type = "seq")
    })
    
    output$choleraAgeSexBarChartFemale <- renderPlot({
      ggplot(choleraAgeSexDataset, aes(x = age, y = female, fill = age)) +
        labs(title = "Female", x = "Age Group", y = "Deaths Per 10,000 People") + 
        properTextSizeTheme +
        geom_bar(width = 0.8, stat = "identity", color = "black") +
        scale_y_continuous(expand = c(0, 0)) + 
        scale_fill_brewer(type = "seq", palette = "Reds")
    })
    
  
    # UKCensus:
    output$UKCensusBarChartMale <- renderPlot({
      ggplot(UKCensusDataset, aes(x = age, y = male, fill = age)) +
        labs(title = "Male Population", x = "Age Group", y = "Num. of People") + 
        properTextSizeTheme +
        geom_bar(width = 0.8, stat = "identity", color = "black") +
        scale_y_continuous(expand = c(0, 0)) + 
        scale_fill_brewer(type = "seq")
    })
    
    output$UKCensusBarChartFemale <- renderPlot({
      ggplot(UKCensusDataset, aes(x = age, y = female, fill = age)) +
        labs(title = "Female Population", x = "Age Group", y = "Num. of People") + 
        properTextSizeTheme +
        geom_bar(width = 0.8, stat = "identity", color = "black") +
        scale_y_continuous(expand = c(0, 0)) + 
        scale_fill_brewer(type = "seq", palette = "Reds")
    })
    #====
  #----
  
  #Cholera Deaths Locations
  choleraPumpLocations <- read.csv(file = "data/choleraPumpLocations.csv", header = FALSE)
  colnames(choleraPumpLocations) <- c("lng", "lat")
  choleraDeathLocations <- read.csv(file = "data/choleraDeathLocations.csv", header = FALSE)
  colnames(choleraDeathLocations) <- c("count", "lng", "lat")
  
  output$choleraDeathsMap <- renderLeaflet({
    map <- leaflet() %>% 
      addTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(opacity = 0.80)) %>%
      # addProviderTiles(providers$Stamen.TonerLabels) %>%
      setView(lng = -0.136668, lat = 51.513341, zoom = 16) %>%
      addCircleMarkers(data = choleraPumpLocations, ~lng, ~lat, weight = 2, radius = 10, color = "blue", stroke = TRUE, fillOpacity = 0.7) %>%
      addCircleMarkers(data = choleraDeathLocations, popup = ~paste("Deaths: ", count), ~lng, ~lat, weight = 2, radius = ~(1 * as.numeric(count)), color = "red", stroke = TRUE, fillOpacity = 0.7) %>%
      addLegend("bottomright", colors = c("red", "blue"), labels = c("Deaths", "Pumps"), title="Cholera Deaths")    
    
    # addProviderTiles(providers$Stamen.TonerLines,
    #                  options = providerTileOptions(opacity = 0.35)) %>%
    #   addProviderTiles(providers$Stamen.TonerLabels) %>%
  })
  
}

# Run the Application:
shinyApp(ui = ui, server = server)