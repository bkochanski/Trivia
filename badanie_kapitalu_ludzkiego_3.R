library(haven)
bkl<-read_sav("BKL 2017-2021.sav")
bkl2<-read_sav("BKL BAZA 5-95 percentyl.sav")
tmp<-as.data.frame(lapply(bkl, attr, "label"))
View(t(tmp))
sort(unique(bkl$m9d))
max(bkl$m9d, na.rm=TRUE)
length(bkl$m9d)
length(na.omit(bkl$m9))
length(na.omit(bkl$m9))*.9
quantile(bkl$m9, c(.05, .95), na.rm=TRUE)
sum(bkl$m9<=1100, na.rm=TRUE)
sum(bkl$m9>7000, na.rm=TRUE)
9118-8935

table(bkl2$ZAROBKI)
table(bkl$m9)

library(dplyr)
data.frame(z1=bkl2$ZAROBKI) %>% filter(!is.na(z1)) %>% group_by(z1) %>% summarise(z1_count=n()) -> t1
data.frame(z1=bkl$m9) %>% filter(!is.na(z1)) %>% group_by(z1) %>% summarise(z2_count=n()) -> t2

mt1<-merge(t1, t2, all = TRUE)

summary(lm(formula = ZAROBKI ~ PLEC +ROK + WYKSZ + UMIE + MSC + LATPRAC + DZIECI + ZGOD + CYW + ZDRO, 
           data= bkl2))

table(bkl2$ZAROBKI, useNA="ifany")
sum(bkl$m9<=1100, na.rm=TRUE)

attr(bkl2$UMIE, "labels")
mean(bkl2$UMIE, "labels")

table(bkl$g2_3)
table(bkl2$UMIE)
table(bkl$g2_3[is.na(bkl$m9) | bkl$m9<=7000])
