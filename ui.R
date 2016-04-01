library(shiny)
senior.agg.by.sta.all.weather <-read.csv("senior.agg.by.sta.all.weather.csv",head = T)[,-1]
shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      ".container {padding-left: 10px !important; margin-left: 0px !important;}
      .navbar-nav {margin-left: 20px !important;}
      .progress-text {position:absolute; top: 70px !important; width:auto !important;}
      "
    )
    )),
  navbarPage(
    title = 'Taipei MRT Traffics of Seniors',
    tabPanel(
      'Table-Traffic Data Aggregate by Station',
      titlePanel(h2(
        "Table - Traffic Data Aggregate by Station", align = "center"
      )),
      DT::dataTableOutput('ex1')
    ),
    tabPanel('Graph-Traffic Data Aggregate by Station',
             # Define the overall UI
             shinyUI(
               # Use a fluid Bootstrap layout
               fluidPage(
                 # Give the page a title
                 titlePanel(h2("Graph - Traffic Histogram", align = "center")),
                 hr(),
                 # Generate a row with a sidebar
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "viewby", "View by",
                       choices = c("Day" = "d","Month" = "m"), selected = "m"
                     ),
                     
                     selectInput("region", "Taipei MRT Stations:",
                                 choices = c("全部", unique(unlist(
                                   lapply(senior.agg.by.sta.all.weather$station,as.character)
                                 )))),
                     
                     selected = "All",
                     
                     hr(),
                     helpText(HTML(
                       paste(
                         "Data from Taipei MRT",
                         " ",
                         "Spring:",
                         "2014/1/19~1/25",
                         "2014/3/16~3/22",
                         " ",
                         "Summer:",
                         "2014/4/13~4/19",
                         "2014/6/15~6/21",
                         " ",
                         "Fall:",
                         "2014/7/20~7/26",
                         "2014/9/14~9/20",
                         " ",
                         "Winter:",
                         "2014/10/19~10/25",
                         "2014/12/7~12/13", sep = "<br>"
                       )
                     )),
                     tags$head(tags$style(
                       "#AggByStaPlot{height:70vh !important;}"
                     )),
                     width = 3
                   ),
                   
                   # Create a spot for the barplot
                   mainPanel(h3(textOutput("selectedVal"),align = "center"),
                             plotOutput("AggByStaPlot"))
                   
                 )
               )
             )),
    
    tabPanel(
      HTML(
        '<li style="position: absolute; right:0px;"><a href="https://jeffweilee.shinyapps.io/App-seniorMRTRawData/" target="_blank">Table-Traffic Raw Data</a></li>'
      )
    )
  )
    ))