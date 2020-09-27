library(readr)
library(ggplot2)
library(dplyr)
library(openair)
library(ggpubr)
install.packages('ggpubr')
#reading data
WHO_COVID_19 <- read_csv("WHO-COVID-19-global-data.csv")
#filtering iran data
iran_covid_data <-
  WHO_COVID_19%>%
  filter(Country=="Iran (Islamic Republic of)")%>%
  rename('date'='Date_reported')
#converting dates from class "Data" to class "POSIXct"
for (i in iran_covid_data[,1]){
    iran_covid_data[,1,i] = as.POSIXct(i)
}
# Fixing dataset problems
iran_covid_data[59,5] = 385
iran_covid_data[60,5] = 523
iran_covid_data[59,7] = 11
iran_covid_data[60,7] = 12
for (i in 141:267){
  iran_covid_data[i,5] = iran_covid_data[i+1,5]
  iran_covid_data[i,6] = iran_covid_data[i+1,6]
  iran_covid_data[i,7] = iran_covid_data[i+1,7]
  iran_covid_data[i,8] = iran_covid_data[i+1,8]
}
iran_covid_data[268,5] = 3204
iran_covid_data[268,6] = iran_covid_data[267,6]+ 3204
iran_covid_data[268,7] = 172
iran_covid_data[268,8] = iran_covid_data[267,8]+172
# Viewing final fixed data
View(iran_covid_data)
iran_covid_data_sliced<-
  iran_covid_data%>%
  slice(30:268)
summaryPlot(select(iran_covid_data, date:New_cases))
timePlot(iran_covid_data, pollutant = c('New_cases','New_deaths'),y.relation = 'free')
calendarPlot(iran_covid_data,pollutant = c('New_cases','New_deaths'))
timeVariation(iran_covid_data,pollutant = 'New_cases')
timePlot(iran_covid_data_sliced, pollutant = c('New_cases','New_deaths','Cumulative_cases','Cumulative_deaths')
         ,y.relation = 'free')



daily_cases <- calendarPlot(iran_covid_data_sliced,pollutant = 'New_cases',annotate = 'value', lim=3000, 
             layout = c(4,2), cols = c("lightgreen","yellow",'orange',"red"),
             col.lim = c('Black','Black'), key.header = 'بسیار خطرناک',
             key.footer = 'عادی',key.position = 'right',main='تعداد مبتلایان جدید هر روز کووید-۱۹ در  ایران'
             , auto.text = FALSE, cex.lim = c(0.6,1))
daily_deaths <- calendarPlot(iran_covid_data_sliced,pollutant = 'New_deaths',annotate = 'value', lim=200, 
             layout = c(4,2), cols = c("lightgreen","yellow",'orange',"red"),
             col.lim = c('Black','Black'), key.header = 'بسیار خطرناک',
             key.footer = 'عادی',key.position = 'right',main='تعداد مرگ و میر جدید هر روز کووید-۱۹ در  ایران'
             , auto.text = FALSE, cex.lim = c(0.6,1))
