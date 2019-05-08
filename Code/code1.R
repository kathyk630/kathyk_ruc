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

# key = read_csv("web-traffic-time-series-forecasting/key.csv", n_max = 100)
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
temp = data %>% filter(str_detect(Page, "wikipedia")) %>% select(-c(rowname, Page)) 
wikipediaTotal = as.data.frame(t(sapply(temp, margin = 2, sum, na.rm = TRUE)))
temp = data %>% filter(str_detect(Page, "wikimedia")) %>% select(-c(rowname, Page)) 
wikimediaTotal = as.data.frame(t(sapply(temp, margin = 2, sum, na.rm = TRUE)))
temp = data %>% filter(str_detect(Page, "mediawiki")) %>% select(-c(rowname, Page)) 
mediawikiTotal = as.data.frame(t(sapply(temp, margin = 2, sum, na.rm = TRUE)))
wikipediaTotal = wikipediaTotal %>% t() %>% as.data.frame %>% rownames_to_column %>%
  rename(dates = rowname, traffic = V1) %>% mutate(dates = as.Date(dates))
wikimediaTotal = wikimediaTotal %>% t() %>% as.data.frame %>% rownames_to_column %>%
  rename(dates = rowname, traffic = V1) %>% mutate(dates = as.Date(dates))
mediawikiTotal = mediawikiTotal %>% t() %>% as.data.frame %>% rownames_to_column %>%
  rename(dates = rowname, traffic = V1) %>% mutate(dates = as.Date(dates))
ggplot(wikipediaTotal, aes(dates, traffic)) + geom_line() + geom_smooth(method = 'loess') + 
  labs(title = "Total Web Traffic of Wikipedia")
ggplot(wikimediaTotal, aes(dates, traffic)) + geom_line() + geom_smooth(method = 'loess') + 
  labs(title = "Total Web Traffic of Wikimedia")
ggplot(mediawikiTotal, aes(dates, traffic)) + geom_line() + geom_smooth(method = 'loess') + 
  labs(title = "Total Web Traffic of Mediawiki")
table(Pages$project)
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
sample_n(plotLang, 6)
ggplot(plotLang, aes(as.Date(variable), value)) + geom_line(aes(color=L1)) + 
  labs(x="dates", y="traffic", title = "Total Web Traffic of Different Languages")
table(Pages$project)
table(Pages$access)
table(Pages$agent)
library(grid)
#library(Rmisc)
p1 = ggplot(Pages, aes(project, fill=project)) + geom_bar(show.legend = FALSE)
p2 = ggplot(Pages, aes(access)) + geom_bar(fill = 'blue')
p3 = ggplot(Pages, aes(agent)) + geom_bar(fill = 'blue')

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout = function(x,y)viewport(layout.pos.row = x,layout.pos.col = y)
print(p1,vp = vplayout(1,1:2))
print(p2,vp = vplayout(2,1))
print(p3,vp = vplayout(2,2))
dev.off()
# multiplot(plotlist = list(p1,p2,p3), layout = matrix(c(1,1,2,3), nrow = 2, byrow = TRUE))
nrow(data)
Data = merge(data, Pages, by = 'rowname', sort = FALSE)
nrow(Data)
Data = Data %>% select(-c(rowname, Page)) %>% gather(dates, traffic, -c(name, project, access, agent))
head(Data)
nrow(Data)
temp = Data %>% select(dates, access, traffic) %>% group_by(dates, access) %>% 
  summarize(views = sum(traffic, na.rm = TRUE))
temp %>% ggplot(aes(ymd(dates), views, color = access)) + geom_line() + 
  labs(x = "dates", y = "views", title = "Total Web Traffic of Different Access")
temp = Data %>% select(dates, agent, traffic) %>% group_by(dates, agent) %>% 
  summarize(views = sum(traffic, na.rm = TRUE))
temp %>% ggplot(aes(ymd(dates), views, color = agent)) + geom_line() + 
  labs(x = "dates", y = "views", title = "Total Web Traffic of Different Agent")
Data %>% group_by(project, name) %>% summarize(views = sum(as.numeric(traffic), na.rm = TRUE)) %>% top_n(3, views) 
extractTimeSeries <- function(rowNumber){
  curPageName = data[rowNumber,]['Page']
  data[rowNumber,] %>% select(-c(rowname, Page)) %>% t %>% as.data.frame %>% 
    rownames_to_column %>% dplyr::rename(dates = rowname, traffic = V1) %>% 
    mutate(dates = as.Date(dates)) %>% ggplot(aes(dates, traffic)) + 
    geom_line() + geom_smooth(method = "loess") + labs(title = str_c(curPageName))
}
extractTimeSeries(11214)
plotZoomIn <- function(rowNumber, beginDate, endDate){
  curPageName = data[rowNumber,]['Page']
  data[rowNumber,] %>% select(-c(rowname, Page)) %>% t %>% as.data.frame %>%   
    rownames_to_column %>% dplyr::rename(dates = rowname, traffic = V1) %>%
    mutate(dates = as.Date(dates)) %>% filter(dates > beginDate & dates < endDate) %>% 
    ggplot(aes(dates, traffic)) + geom_line() + 
    labs(title = str_c(curPageName, "\n", beginDate, " -- ", endDate))
}

p1 <- plotZoomIn(10404, "2016-10-01", "2016-12-01")
p2 <- plotZoomIn(9775, "2015-09-01", "2015-11-01")
p3 <- plotZoomIn(139120, "2016-10-01", "2016-12-01")
p4 <- plotZoomIn(110658, "2016-07-01", "2016-09-01")
library(Rmisc)
layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
data[is.na(data)] = 0 
Max = apply(data[,-c(1,2)], 1, max)
Min = apply(data[,-c(1,2)], 1, min)
Avg = apply(data[,-c(1,2)], 1, mean)
Std = apply(data[,-c(1,2)], 1, sd)

# dataSet = data[, -c(1,2)] %>% as.matrix() %>% t() %>% scale() %>% t() %>% as.data.frame() %>% rownames_to_column()
# head(dataSet)
dataSet = log1p(data[, -c(1,2)]) %>% rownames_to_column()
# head(dataSet)
i = match("2017-06-30", names(dataSet))
trainSet = select(dataSet, 1:i)
testSet = select(dataSet, c(1,(i+1):804))
trainSet %>% names %>% head(10)
testSet %>% names %>% head(10)
library(doSNOW)
library(doRNG)
library(foreach)
library(parallel)
library(tseries)
library(forecast)
cl = makeCluster(20)
registerDoSNOW(cl)
fc.wiki <- foreach(i=1:nrow(trainSet), .combine=rbind, .packages="forecast") %dopar% {
  y <- tsclean(as.ts(unlist(trainSet[i, -1]), frequency = 7))
  forecast(auto.arima(y, max.p=2, max.d=2, max.q=1), h=72)$mean
}
stopCluster(cl)

plotARIMA <- function(index){
y <- tsclean(ts(unlist(trainSet[index, -1]), frequency = 7))
fc.index = forecast(auto.arima(y, max.p=2, max.d=2, max.q=1), h=72, level = c(50,95))
true.index = testSet[index, -1] %>% t %>% as.data.frame() %>% rownames_to_column()
colnames(true.index) = c("dates", "traffic")
autoplot(fc.index) + geom_line(aes(c(732:803)/7, traffic), data = true.index, color = "grey40") + labs(x = "Time/weeks", y = "Views")
}
plotARIMA(95786)
plotARIMA(139120)
plotARIMA(6597)
plotARIMA(37862)
