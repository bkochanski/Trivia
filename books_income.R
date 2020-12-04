
library(ggplot2)
library(ggrepel)
library(Cairo)
library(weights)

#load(url("https://github.com/pbiecek/Diagnoza/raw/master/data/osoby.rda"))
extract<-osoby[,c('wojewodztwo', 'hp65', 'hp111_1', 'waga_2015_ind')]
extract<-na.omit(extract)
woje_dict<-c(
  "", "DOLNOŚLĄSKIE", 
  "", "KUJAWSKO-POM.", 
  "", "LUBELSKIE", 
  "", "LUBUSKIE",
  "", "ŁÓDZKIE", 
  "", "MAŁOPOLSKIE", 
  "", "MAZOWIECKIE", 
  "", "OPOLSKIE",
  "", "PODKARPACKIE", 
  "", "PODLASKIE", 
  "", "POMORSKIE", 
  "", "ŚLĄSKIE",
  "", "ŚWIĘTOKRZYSKIE", 
  "", "WARMIŃSKO-MAZURSKIE", 
  "", "WIELKOPOLSKIE", 
  "", "ZACHODNIOPOMORSKIE")
extract$wojewodztwo<-as.factor(woje_dict[extract$wojewodztwo])
names(extract)<-c("wojewodztwo", "dochod", "ksiazki", "waga_sondazowa")
dochod_przecietnie<-aggregate(extract$dochod*extract$waga_sondazowa, by=list(extract$wojewodztwo), sum)[,2]/
  aggregate(extract$waga_sondazowa, by=list(extract$wojewodztwo), sum)[,2]
ksiazki_przecietnie<-aggregate(extract$ksiazki*extract$waga_sondazowa, by=list(extract$wojewodztwo), sum)[,2]/
  aggregate(extract$waga_sondazowa, by=list(extract$wojewodztwo), sum)[,2]
woje<-levels(extract$wojewodztwo)

labelc1<-paste("Korelacja:", 
               round(cor(ksiazki_przecietnie, dochod_przecietnie),4))
labelr1<-paste("Regresja: \n",
              "1 książka => średnio ",
              round(lm(dochod_przecietnie~ksiazki_przecietnie)$coefficients[2]) ,
              " zł więcej", 
              sep="")

plotA<-ggplot(data=data.frame(woje, ksiazki_przecietnie, dochod_przecietnie), 
       mapping=aes(x=ksiazki_przecietnie, y=dochod_przecietnie))+geom_point(colour='lightblue', size=4)+
  xlab('przeciętna liczba książek przeczytanych w ciągu roku') + ylab('przeciętny miesięczny dochód') +
  xlim(c(1.5,6.5))+
  geom_text_repel(aes(label=woje, vjust=0), size=3)+theme_classic()+
  geom_smooth(method='lm',se = FALSE, lty=2) +
  annotate("text", label = labelc1, x = 1.5, y = 2500, size = 6, colour = "dark blue", hjust=0) +
  annotate("text", label = labelr1, x = 1.5, y = 2300, size = 4, colour = "dark blue", hjust=0)

print(plotA)

ggsave(filename="plotA.pdf", plot=plotA+theme(plot.margin=unit(c(3,3,3,3),"cm")), width = 297/1.2, height = 210/1.2, units = "mm", device=cairo_pdf)

labelc2<-paste("Korelacja:", 
               round(wtd.cor(extract$dochod, extract$ksiazki, extract$waga_sondazowa)[1],4))
labelr2<-paste("Regresja: \n",
               "1 książka => średnio ",
              round(lm(extract$dochod~extract$ksiazki, weights=extract$waga_sondazowa)$coefficients[2],2) ,
              " zł więcej", 
              sep="")

plotB<-ggplot(data=extract, mapping=aes(x=ksiazki, y=dochod))+
  geom_jitter(alpha=0.1)+ 
  coord_cartesian(xlim = c(0, 100), ylim=c(0,10000)) +
  xlab('książki przeczytane w ciągu roku') + ylab('miesięczny dochód')+theme_classic()+
  geom_smooth(method='lm',se = FALSE, lty=2) +
  annotate("text", label = labelc2, x = 50, y = 9600, size = 6, colour = "dark blue", hjust=0)+
  annotate("text", label = labelr2, x = 50, y = 8000, size = 4, colour = "dark blue", hjust=0)

plot(plotB)

ggsave(filename="plotB.pdf", plot=plotB+theme(plot.margin=unit(c(3,3,3,3),"cm")), width = 297/1.2, height = 210/1.2, units = "mm", device=cairo_pdf)

