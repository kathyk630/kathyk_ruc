library(shiny)
# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tabsets"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    radioButtons("lang", "Different Languages:",
                 list("English" = "english",
                      "German" = "german",
                      "Spanish" = "spanish",
                      "French" = "french",
                      "Japanese" = "japanese",
                      "Russian" = "russian",
                      "Chinese" = "chinese")),
    br(),
    
    radioButtons("access", "Different Accesses:",
                 list("All-access" = "all_access",
                      "Desktop" = "desktop",
                      "Mobile-web" = "mobile_web")),
    br(),
    
    radioButtons("agent", "Different Agents:",
                 list("All-agents" = "all_agents",
                      "Spider" = "spider")),
    br(),
    
    sliderInput("rowNumber", "Row Number:", 
                min=0, max=145063, value=72531),
    br()
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot1", plotOutput("plot1")),
      tabPanel("Overall Plot1", plotOutput("overallPlot1"))
    ),
    tabsetPanel(
      tabPanel("Plot2", plotOutput("plot2")),
      tabPanel("Overall Plot2", plotOutput("overallPlot2"))
    ),
    tabsetPanel(
      tabPanel("Plot3", plotOutput("plot3")),
      tabPanel("Overall Plot3", plotOutput("overallPlot3"))
    ),
    tabsetPanel(
      tabPanel("Plot4", plotOutput("plot4"))
    )
  )
))