url2<-'https://www.ctdatacollaborative.org/sites/g/files/tmzbdl2011/files/CTDC_synthetic_20210825.tsv'

dt<-readr::read_tsv(url2)
View(dt)

table(replace(dt$isForcedLabour,is.na(dt$isForcedLabour),0))/length(dt$isForcedLabour)*100
table(replace(dt$isSexualExploit,is.na(dt$isSexualExploit),0))/length(dt$isForcedLabour)*100

table(dt$isForcedLabour, useNA="ifany")/length(dt$isForcedLabour)*100


dt %>%
  mutate(isf = replace(isForcedLabour,is.na(isForcedLabour),0)) %>%
  mutate(iss = replace(isSexualExploit,is.na(isSexualExploit),0)) %>%
  group_by(yearOfRegistration) %>%
  summarise(forced_share = mean(isf)*100,
            forded_sum = sum(isf), 
            sex_share = mean (iss),
            sex_sum = sum(iss),
            n = n()) %>% View()

dtx<-readxl::read_excel("CTDC_wersja kontynenty.xlsx")

table(dt$ageBroad, useNA="ifany")
table(dt$gender, useNA="ifany")

table(dtx$UN_COO_Region)
table(dtx$UN_COO_Afryka)

View(dtx)

dtx2<-dtx[dtx$isForcedLabour==1,]
dtx2$UN_COE_Afryka[dtx2$UN_COE_Afryka==99]<-NA
dtx2$UN_COO_Afryka[dtx2$UN_COO_Afryka==99]<-NA
dtx2$`UN_COO_Ameryka/Oceania`[dtx2$`UN_COO_Ameryka/Oceania`==99]<-NA
dtx2$`UN_COE_Ameryka/Oceania`[dtx2$`UN_COE_Ameryka/Oceania`==99]<-NA
dtx2$UN_COO_Azja[dtx2$UN_COO_Azja==99]<-NA
dtx2$UN_COE_Azja[dtx2$UN_COE_Azja==99]<-NA
dtx2$UN_COO_Europa[dtx2$UN_COO_Europa==99]<-NA
dtx2$UN_COE_Europa[dtx2$UN_COE_Europa==99]<-NA

kol=c(3, 2, 1, 4)
round(cor(x=dtx2[kol+5], y=dtx2[kol+10], use="pairwise.complete.obs", method="spearman"),4)
#cor(x=dtx[c(6:9)], y=dtx[c(11:14)], use="pairwise.complete.obs", method="spearman")


round(colMeans(dtx2[c(6:9, 11:14)], na.rm=TRUE),2)

cor(dtx2$UN_COO_Afryka, dtx2$UN_COE_Afryka, use="pairwise.complete.obs")
cor(dtx$UN_COO_Afryka, dtx$UN_COE_Afryka, use="pairwise.complete.obs")


