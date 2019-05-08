library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(stringr)
library(ggplot2)
library(lubridate)
library(reshape2)

data = read_csv("web-traffic-time-series-forecasting/data.csv")

glimpse(key)
head(key)
dim(data)
select(head(data,10), 1:5, 800:804)
select(tail(data,10), 1:5, 800:804)
sum(is.na(data)) / (nrow(data) * ncol(data))
head(data$Page, 10)
data = rownames_to_column(data)
wikipedia = filter(data, str_detect(data$Page, "wikipedia.org")) %>% select(rowname, Page)
nrow(wikipedia)
wikipedia = wikipedia %>% separate(Page, into = c("first","second"), sep=".wikipedia.org_") %>% 
  separate(first, c("name", "project"), sep=-3) %>% separate(second, c("access", "agent"), sep = "_") %>% 
  mutate(project = str_sub(project, 2, 3))
wikipedia[1,]
wikimedia = filter(data, str_detect(data$Page, "wikimedia.org")) %>% select(rowname, Page)
nrow(wikimedia)
wikimedia = wikimedia %>% separate(Page, into = c("name", "second"), sep=".commons.wikimedia.org_") %>% 
  separate(second, c("access", "agent"), sep = "_") %>% mutate(project = "wikimedia")
wikimedia[1,]
mediawiki = filter(data, str_detect(data$Page, "mediawiki.org")) %>% select(rowname, Page)
nrow(mediawiki) 
mediawiki = mediawiki %>% separate(Page, into = c("name", "second"), sep=".www.mediawiki.org_") %>% 
  separate(second, c("access", "agent"), sep = "_") %>% mutate(project = "mediawiki")
mediawiki[1,]
nrow(mediawiki) + nrow(wikipedia) + nrow(wikimedia) == nrow(data)
Pages = full_join(wikipedia, wikimedia,  by = c("rowname", "name", "project", "access", "agent")) %>% 
  full_join(mediawiki,  by = c("rowname", "name", "project", "access", "agent"))
head(Pages)
rowsnum = Pages %>% filter(project == "en") %>% select(rowname)
english = data %>% filter(rowname %in% as.vector(t(rowsnum))) %>% select(-c(rowname, Page))
german = data %>% filter(rowname %in% Pages$rowname[which(Pages$project == "de")])  %>% 
  select(-c(rowname, Page))
spanish = data %>% filter(rowname %in% Pages$rowname[which(Pages$project == "es")])  %>% 
  select(-c(rowname, Page))
french = data %>% filter(rowname %in% Pages$rowname[which(Pages$project == "fr")])  %>% 
  select(-c(rowname, Page))
japanese = data %>% filter(rowname %in% Pages$rowname[which(Pages$project == "ja")])  %>% 
  select(-c(rowname, Page))
russian = data %>% filter(rowname %in% Pages$rowname[which(Pages$project == "ru")])  %>% 
  select(-c(rowname, Page))
chinese = data %>% filter(rowname %in% Pages$rowname[which(Pages$project == "zh")])  %>% 
  select(-c(rowname, Page))
languages = list(english=english, german=german, spanish=spanish, french=french, 
                 japanese=japanese, russian=russian, chinese=chinese)
langs = names(languages)
languagesSum = list()
for (l in langs){
  languagesSum[[l]] = as.data.frame(t(sapply(languages[[l]], margin = 2, sum, na.rm = TRUE)))
}
plotLang = melt(languagesSum)

nrow(data)
Data = merge(data, Pages, by = 'rowname', sort = FALSE)
nrow(Data)
Data = Data %>% select(-c(rowname, Page)) %>% gather(dates, traffic, -c(name, project, access, agent))
head(Data)
nrow(Data)
plotAccess = Data %>% select(dates, access, traffic) %>% group_by(dates, access) %>% 
  summarize(views = sum(traffic, na.rm = TRUE))
plotAgent = Data %>% select(dates, agent, traffic) %>% group_by(dates, agent) %>% 
  summarize(views = sum(traffic, na.rm = TRUE))

extractTimeSeries <- function(rowNumber){
  curPageName = data[rowNumber,]['Page']
  data[rowNumber,] %>% select(-c(rowname, Page)) %>% t %>% as.data.frame %>% 
    rownames_to_column %>% dplyr::rename(dates = rowname, traffic = V1) %>% 
    mutate(dates = as.Date(dates)) %>% ggplot(aes(dates, traffic)) + 
    geom_line() + geom_smooth(method = "loess") + labs(title = str_c(curPageName))
}

library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  data <- reactive({  
    lang <- switch(input$lang,
                   english = filter(plotLang, L1 == "english"),
                   german = filter(plotLang, L1 == "german"),
                   spanish = filter(plotLang, L1 == "spanish"),
                   french = filter(plotLang, L1 == "french"),
                   japanese = filter(plotLang, L1 == "japanese"),
                   russian = filter(plotLang, L1 == "russian"),
                   chinese = filter(plotLang, L1 == "chinese"))
  })
  data2 <- reactive({  
    access <- switch(input$access,
                   all_access = filter(plotAccess, access == "all-access"),
                   desktop = filter(plotAccess, access == "desktop"),
                   mobile_web = filter(plotAccess, access == "mobile-web"))
    
  })
  data3 <- reactive({  
    agent <- switch(input$agent,
                     all_agents = filter(plotAgent, agent == "all-agents"),
                     spider = filter(plotAgent, agent == "spider"))
    
  })
  data4 <-reactive({
    rowNumber <- input$rowNumber
  })

  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the 'data' reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot1 <- renderPlot({
    lang <- input$lang
    ggplot(data(), aes(as.Date(variable), value)) + geom_line() + 
      labs(x="dates", y="traffic", title = paste("Total Web Traffic of Language ", lang))
  })
  output$overallPlot1 <- renderPlot({
    ggplot(plotLang, aes(as.Date(variable), value)) + geom_line(aes(color=L1)) + 
      labs(x="dates", y="traffic", title = "Total Web Traffic of Different Languages")
  })
  output$plot2 <- renderPlot({
    access <- input$access
    data2() %>% ggplot(aes(ymd(dates), views, color = access)) + geom_line() + 
      labs(x = "dates", y = "views", title = paste("Total Web Traffic of Access ", access))
  })
  output$overallPlot2 <- renderPlot({
    plotAccess %>% ggplot(aes(ymd(dates), views, color = access)) + geom_line() + 
      labs(x = "dates", y = "views", title = "Total Web Traffic of Different Access")
  })
  output$plot3 <- renderPlot({
    agent <- input$agent
    data3() %>% ggplot(aes(ymd(dates), views)) + geom_line() + 
      labs(x = "dates", y = "views", title = paste("Total Web Traffic of Agent ", agent))
  })
  output$overallPlot3 <- renderPlot({
    plotAgent %>% ggplot(aes(ymd(dates), views, color = agent)) + geom_line() + 
      labs(x = "dates", y = "views", title = "Total Web Traffic of Different Agent")
  })
  output$plot4 <- renderPlot({
    extractTimeSeries(data4())
  })
})