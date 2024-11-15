library(rvest)
library(tidyr)
library(dplyr)

CzytajWygrane <- function(lotto_rok){
  html_to_read<-paste("https://www.multipasko.pl/wygrane-lotto/duzy-lotek",lotto_rok,"", sep="/")
  read1<-read_html(html_to_read)
  read2<-read1 %>% html_node("table.wygrane-year") %>% html_table(header=TRUE)
  read2<-read2[!(read2$Losowanie=="Suma"),]
  read3<-read2 %>% separate("6/6", c("liczba_6", "wygrana_6"), " x ") %>% 
    separate("5/6", c("liczba_5", "wygrana_5"), " x ") %>%
    separate("4/6", c("liczba_4", "wygrana_4"), " x ") %>%
    separate("3/6", c("liczba_3", "wygrana_3"), " x ") %>%
    separate("Losowanie", c("Nr", "Data"), -10)
  return(read3)}

#lotto_wygrane_do2023<-CzytajWygrane(2023)
#for (lotto_rok in 2022:2003){  lotto_wygrane_do2023<-rbind(lotto_wygrane_do2023,CzytajWygrane(lotto_rok))}
#saveRDS(lotto_wygrane_do2023, file="lotto_wygrane_do2023")
lotto_wygrane_do2023<-readRDS("lotto_wygrane_do2023")

lotto_wygrane <- rbind(CzytajWygrane(2024), lotto_wygrane_do2023)

CzytajWyniki <- function(lotto_Nr){
  html_to_read<-paste("https://www.multipasko.pl/wyniki-lotto/duzy-lotek/sortowane/",lotto_Nr,"/",sep='')
  read1<-read_html(html_to_read)
  read2<-read1 %>% html_node("table.wynikiLosowan.bigballs") %>% html_table(header=FALSE)
  read3<-read2 %>% 
    mutate(Data=gsub("[^0-9\\.]", "", X2)) %>% 
    separate("X3", cbind(paste("L_",c(1:6),sep=''),paste("L2_",c(1:6),sep='')), " ") %>% 
    select(-one_of(c("X4","X5","X6"))) %>%
    rename("Nr"="X1") %>%
    select(c(Nr,Data,everything()))
  return(read3)
}


#lotto_wyniki_do6981<-CzytajWyniki(6981)
#for (lotto_Nr in seq(6981-100,1,-100)){lotto_wyniki_do6981<-rbind(lotto_wyniki_do6981,CzytajWyniki(lotto_Nr))}
#saveRDS(lotto_wyniki_do6981, file="lotto_wyniki_do6981")
lotto_wyniki_do6981<-readRDS("lotto_wyniki_do6981")

max_NR<-as.numeric(max(lotto_wygrane$Nr))
lotto_wyniki_reszta<-CzytajWyniki(max_NR)
for (lotto_Nr in seq(max_NR-100,6981,-100)){
  lotto_wyniki_reszta<-rbind(lotto_wyniki_reszta,CzytajWyniki(lotto_Nr))
}

lotto_wyniki<-rbind(lotto_wyniki_reszta[lotto_wyniki_reszta$Nr>6981,], lotto_wyniki_do6981)


######### Czyszczenie #########

lotto_dane<-merge(lotto_wygrane, lotto_wyniki, by="Nr")
names(lotto_dane)[names(lotto_dane) == 'Zakładów'] <- 'Zakladow'
names(lotto_dane)[names(lotto_dane) == 'Data.x'] <- 'data_los'

######### czyszczenie zmiennych numerycznych ##########
WyczyscLiczby<-function(liczba){as.numeric(sub(",",".",gsub(" ","",sub(" zł", "", liczba))))}
kolumny<-c("Nr", "liczba_6", "wygrana_6", "liczba_5", "wygrana_5", "liczba_4", "wygrana_4", "liczba_3",
           "wygrana_3", "Zakladow", "L_1", "L_2", "L_3", "L_4", "L_5", "L_6", "L2_1", "L2_2", "L2_3",
           "L2_4", "L2_5", "L2_6")
numery_kolumn<-which(colnames(lotto_dane) %in% kolumny)
for (j in numery_kolumn) {lotto_dane[,j]<-WyczyscLiczby(lotto_dane[,j])}

#** czyszczenie dat
lotto_dane$data_los<-as.Date(lotto_dane$data_los,"%d-%m-%Y")
lotto_dane$dzientyg<-factor(weekdays(as.Date(lotto_dane$data_los,"%d-%m-%Y")))
lotto_dane$sobota<-as.numeric(lotto_dane$dzientyg=="sobota")
lotto_dane$czwartek<-as.numeric(lotto_dane$dzientyg=="czwartek")

lotto_dane$ZakladowBez6<-head(c(0,ave(lotto_dane$Zakladow*(lotto_dane$liczba_6==0), 
                                      cumsum(lotto_dane$liczba_6>0), FUN=cumsum)),-1)

for (i in 1:49) {
  col_name <- paste0("R", i)
  lotto_dane[[col_name]] <- rowSums(df[, 14:19] == i)
}

df<-lotto_dane
boxplot(df$liczba_3/df$Zakladow ~ df$R7, horizontal=TRUE)

boxplot(df$liczba_3/df$Zakladow ~ I(df$R1+df$R11+df$R21), horizontal=TRUE)

boxplot(df$liczba_4/df$Zakladow ~ I(df$R1+df$R11+df$R21), horizontal=TRUE)



#** lotto_dane obcięte
min(lotto_dane[lotto_dane$wygrana_3==24,]$data_los)
lotto_dane_24<-lotto_dane[lotto_dane$data_los>='2011-09-06',]


