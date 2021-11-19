library(tidyverse)
library(highcharter)
library(data.table)
library(htmltools)
library(htmlwidgets)

#Loading multiple .csv files into the same data frame
folder <- "~/datasets/"
file_list <- list.files(path = folder, pattern = '*.csv')
data <- do.call("rbind", lapply(file_list, function(x)
                         read.csv(paste(folder, x, sep = ''),
                                  stringsAsFactors = FALSE)))
df <- as.data.frame(data)
glimpse(df)
#remove all rows that dont match with multiple strings in one column
patterns <- c("X60","X61","X62","X63","X64","X65","X66","X67","X68","X69","X70",
              "X71","X72","X73","X74","X75","X76","X77","X78","X79","X80","X81","X82","X83","X84")
df2 <- filter(df, grepl(paste(patterns, collapse="|"), Cause))

#merging 2 dfs
countriesdf <- read_csv("~/countries.csv")
glimpse(countriesdf)
df3 <- merge(x=countriesdf, y=df2, all.y = TRUE)

#clean df
df3 <- df3[,-c(1,3:4,6:7,9:10,37:40)]
df3$Sex <- gsub("1", "Male", df3$Sex)
df3$Sex <- gsub("2", "Female", df3$Sex)
df3$Sex <- gsub("9", "Sex unspecified", df3$Sex)
names(df3)[4:29] <- c("Deaths at all ages","Deaths at age 0 year","Deaths at age 1 year","Deaths at age 2 years",
                      "Deaths at age 3 years","Deaths at age 4 years","Deaths at age 5-9 years","Deaths at age 10-14 years",
                      "Deaths at age 15-19 years","Deaths at age 20-24 years","Deaths at age 25-29 years",
                      "Deaths at age 30-34 years","Deaths at age 35-39 years","Deaths at age 40-44 years",
                      "Deaths at age 45-49 years","Deaths at age 50-54 years","Deaths at age 55-59 years",
                      "Deaths at age 60-64 years","Deaths at age 65-69 years","Deaths at age 70-74 years",
                      "Deaths at age 75-79 years","Deaths at age 80-84 years","Deaths at age 85-89 years",
                      "Deaths at age 90-94 years","Deaths at age 95 years and above","Deaths at age unspecified")
#asignar 0 a NA
df3[is.na(df3)] <- 0
names(df3)[1] <- 'Country'

#remove rows containing specific strings
years <- c("1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998")
df4 <- filter(df3, !grepl(paste(years, collapse="|"), Year))

#plots
p <- df4 %>% 
  group_by(Year) %>%
  summarise(`Deaths at all ages` = mean(`Deaths at all ages`)) %>%
  hchart('lollipop', hcaes(x = Year, y = `Deaths at all ages`)) %>%
  hc_title(text = 'Timeline behaviour of suicides between 1999-2019',
           style = list(fontWeight = 'bold', fontSize = '22px'), align = 'left') %>%
  hc_tooltip(useHTML = TRUE, headerFormat = '', pointFormat = '<b> {point.x}') %>%
  hc_yAxis(title = FALSE, labels = FALSE) %>%
  hc_xAxis(title = list(style = list(fontWeight = 'bold', fontSize = '14px')),
           labels = list(style = list(fontWeight = 'bold', fontSize = '13px')))
p

df5 <- df4 %>% group_by(Sex) %>% summarise(`Deaths at all ages` = mean(`Deaths at all ages`))
glimpse(df5)
df5 <- df5 %>% mutate_if(is.numeric, ~round(., 2))
p2 <- hchart(df5, 'pie', hcaes(Sex, `Deaths at all ages`)) %>%
  hc_title(text = 'Suicide rate by genre', style = list(fontWeight = 'bold', fontSize = '22px'), 
           align = 'left') %>%
  hc_tooltip(useHTML = TRUE, headerFormat = '', pointFormat = '<b> {point.name}: {point.y}%')
p2

df6 <- as.data.frame(mapply(sum,df4[,-(1:4)]))
df6 <- setDT(df6, keep.rownames = TRUE)[]
names(df6)[1] <- 'Age range'
names(df6)[2] <- 'Number of suicides from 1999-2019'
df6 <- df6[-(1:6), ]
p3 <- hchart(df6, 'column', hcaes(df6$`Age range`, df6$`Number of suicides from 1999-2019`)) %>%
  hc_xAxis(title = list(text = 'Age range', style = list(fontWeight = 'bold', fontSize = '14px')),
           labels = list(style = list(fontWeight = 'bold', fontSize = '13px'))) %>%
  hc_title(text = 'Age range of suicides between 1999-2009', style = list(fontWeight = 'bold', fontSize = '22px'), 
           align = 'left') %>%
  hc_tooltip(useHTML = TRUE, headerFormat = '', pointFormat = '<b> {point.name} <br> between 1999-2009: {point.y}') %>%
  hc_yAxis(title = FALSE, labels = FALSE)
p3

df7 <- df4 %>% group_by(Country) %>% summarise(`Deaths at all ages` = mean(`Deaths at all ages`))
p4 <- hchart(df7, 'treemap', hcaes(x = Country, value = `Deaths at all ages`, color = `Deaths at all ages`)) %>%
  hc_tooltip(useHTML = TRUE, pointFormat = '<b> {point.Country}') %>%
  hc_title(text = 'Countries with higher suicide rate', style = list(fontWeight = 'bold', fontSize = '22px'),
           align = 'left')
p4

subplot <- hw_grid(p4,p3,p2,p)
subplot
saveWidget(p, 'whosui1.html')
saveWidget(p2, 'who_sui_2.html')
saveWidget(p3, 'who_sui_3.html')
saveWidget(p4, 'who_sui_4.html')
save_html(subplot, 'plot.html')
