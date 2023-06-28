url1<-'https://www.parp.gov.pl/images/publications/BKL/Poczona_baza_danych_z_badania_ludnoci_z_lat_20172019_i_2021.sav'

library(haven)

data1<-read_sav(url1)
tmp<-as.data.frame(lapply(data1, attr, "label"))
View(t(tmp))

#saveRDS(data1, "bkl_data1.rds")

data2<-data1[data1$edycja<2021,]

table(data2$n1)/length(data2$n1)*100

# tylko nieformalni
data2n<-data2[data2$n1==1,]

# płeć
table(data2n$m2)/length(data2n$m2)*100

library(dplyr)

data2 %>% group_by(miejsce==1) %>% summarize(nief = mean(n1))

table(data2$n1, data2$miejsce)

chisq.test(table(data2$n1, data2$miejsce==1))

# 0. miasto
chisq.test(table(data2$n1, data2$miejsce==1))
rcompanion::cramerV(table(data2$n1, data2$miejsce==1), ci=TRUE)

# 1. płeć
chisq.test(table(data2$n1, data2$m2))
rcompanion::cramerV(table(data2$n1, data2$m2), ci=TRUE)
data2 %>% group_by(m2) %>% summarize(nief = mean(n1)) %>% mutate (niefodds = nief/(1-nief))


model1<-glm(formula = n1 ~ m2, data=data2, family=binomial)
exp(model1$coefficients[1])
exp(model1$coefficients[1])/(1+exp(model1$coefficients[1]))
exp(model1$coefficients[2])


# Analiza
summarise(data2)

library(dplyr)
dane = readxl::read_excel("danePL.xlsx") %>%
  mutate_if(is.character, as.factor) 

data2d<-merge(data2, dane)

table(data2d$bezrej, data2d$`Zarejestrowany bezrobotny`)
table(data2d$bezrej, useNA = "always")
table(data2d$`Zarejestrowany bezrobotny`)

table(data2d$e1, useNA = "always")
table(data2d$`Zatrudniony na umowe o prace`)
data2d$bezrej2<-''
data2d$bezrej2[data2d$bezrej==1]<-'TAK'
data2d$bezrej2[data2d$bezrej==0]<-'NIE'
data2d$bezrej2[is.na(data2d$bezrej)]<-'N/A'

table(data2d$`Zarejestrowany bezrobotny`)
table(data2d$bezrej2)

table(data2d$bezrej2, data2d$e1)
table(data2d$`Zarejestrowany bezrobotny`, data2d$`Zatrudniony na umowe o prace`)


3832-2098
2450-1188

data2d$bezrej2[data2d$e1==1]<-'N/A'
#data2d$bezrej2[data2d$e1==0 & is.na(data2d$bezrej)]<-'NIE'

table(paste(data2d$bezrej2, data2d$e1), useNA = "always")
table(data2d$`Zarejestrowany bezrobotny`)
table(data2d$bezrej2)
5094-3832
2450-1188


table(data2d$miejsce, data2d$`miejsce zamieszkania`)
table(data2d$miejsce)
table(data2d$`miejsce zamieszkania`)

mean(data2$m9, na.rm=TRUE)
mean(data2$m9d, na.rm=TRUE)
mean(data1$m9, na.rm=TRUE)
mean(data1$m9d, na.rm=TRUE)
max(data2$m9, na.rm=TRUE)
max(data2$m9d, na.rm=TRUE)
max(data1$m9, na.rm=TRUE)
max(data1$m9d, na.rm=TRUE)
