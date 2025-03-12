library(lubridate)
library(readxl)

years<-2002:2023
files<-paste0("H:/Mój dysk/do_upo_2024/Urodzenia/pl_uro_",years,"_00_7.xls")
urodzenia <- data.frame(date=as.Date("1999-01-01"), ur = 0)
files[years>2015]<-paste0(files[years>2015],"x")
sheet<-rep("Żywe", length(years))
sheet[years<2015]<-"Żywe - ogółem"
sheet[years>2017]<-"Żywe - ogółem"

for (i in 1:length(years)){
  year = years[i]
  print(i)
  print(years[i])
  print(files[i])
  print(sheet[i])

  a<-readxl::read_excel(files[i],
                   sheet[i], 
                   "A11:N41", col_names = FALSE, na="-")

  for (month in 1:12){
  dates_in_month <- make_date(year, month, 1) + days(0:(days_in_month(make_date(year, month, 1)) - 1))
  tmp<-data.frame(date = dates_in_month, ur = unname(unlist(a[1:length(dates_in_month),2+month])))
  urodzenia<-rbind(urodzenia, tmp)}}

urodzenia<-urodzenia[-1,]

plot(urodzenia$date, urodzenia$ur, type='l')
saveRDS(urodzenia, "urodzenia.rds")
weekdays(urodzenia$date)
year(urodzenia$date)

library(dplyr)
urodzenia %>% 
  group_by(year=year(urodzenia$date)) %>%
  summarize(liczba_ur=sum(ur), liczba_ur_wt=sum(ur[weekdays(date)=="wtorek"])) %>%
  mutate(frakcja_wt = liczba_ur_wt/liczba_ur*100) %>%
  select(year, frakcja_wt) -> wt
plot(wt)
summary(lm(wt$frakcja_wt~wt$year))
#write.csv2(wt, "wt.csv")
#wt<-read.csv2("wt.csv")
