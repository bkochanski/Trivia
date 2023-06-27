url2<-'https://www.ctdatacollaborative.org/sites/g/files/tmzbdl2011/files/CTDC_synthetic_20210825.tsv'

dt<-readr::read_tsv(url2)
View(dt)

table(replace(dt$isForcedLabour,is.na(dt$isForcedLabour),0))/length(dt$isForcedLabour)*100
table(replace(dt$isSexualExploit,is.na(dt$isSexualExploit),0))/length(dt$isForcedLabour)*100

dt %>%
  mutate(isf = replace(isForcedLabour,is.na(isForcedLabour),0)) %>%
  mutate(iss = replace(isSexualExploit,is.na(isSexualExploit),0)) %>%
  group_by(yearOfRegistration) %>%
  summarise(forced_share = mean(isf)*100,
            sex_share = mean (iss)) %>% View()

