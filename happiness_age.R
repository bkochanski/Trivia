#load(url("https://github.com/pbiecek/Diagnoza/raw/master/data/osoby.rda"))
extract <- osoby[,c('wiek2015', 'hp3', 'hp34', 'waga_2015_ind')]
extract <- na.omit(extract)
names(extract)<-c('wiek', 'zycie',  'szczescie', 'waga_sondazowa')
width<-5
extract$przedzial_wieku<-floor(extract$wiek/width)*width

library(dplyr)
library(ggplot2)

#View(extract)

wyniki<-extract%>%
  filter(przedzial_wieku>=20 & przedzial_wieku<=80)%>%
  group_by(przedzial_wieku)%>%
  summarise(
    udzial_szczesliwych=weighted.mean(szczescie<3, waga_sondazowa),
    udzial_szczesliwych_bez_wag=mean(szczescie<3),
    udzial_bardzo_szczesliwych=weighted.mean(szczescie<2, waga_sondazowa),
    udzial_zycie_wspaniale=weighted.mean(zycie<2, waga_sondazowa),
    udzial_zycie_wsp_udane=weighted.mean(zycie<3, waga_sondazowa),
    udzial_zycie_wsp_ud_dosycdobre=weighted.mean(zycie<4, waga_sondazowa)
  )
#View(wyniki)

wyniki$przedzial_wieku2<-ordered(paste(wyniki$przedzial_wieku, wyniki$przedzial_wieku+width-1, sep="-\n")) 

plot_happy_A<-ggplot(data=wyniki, mapping=aes(x=przedzial_wieku2, group=1))+
  geom_line(aes(y=udzial_szczesliwych, col="bardzo szczęśliwy/a + dosyć szczęśliwy/a"), size=1)+
  geom_point(aes(y=udzial_szczesliwych, col="bardzo szczęśliwy/a + dosyć szczęśliwy/a"), size=1)+
  geom_line(aes(y=udzial_bardzo_szczesliwych, col="bardzo szczęśliwy/a"), size=1)+
  geom_point(aes(y=udzial_bardzo_szczesliwych, col="bardzo szczęśliwy/a"), size=1)+
  ggthemes::theme_economist()+  
  scale_colour_manual("", 
                      breaks = c("bardzo szczęśliwy/a", "bardzo szczęśliwy/a + dosyć szczęśliwy/a"),
                      values = c("darkblue", "blue"))+
  ylab("udział osób szczęśliwych")+
  xlab("przedział wiekowy")+ 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))+ 
  theme(legend.text=element_text(size=8.5))

print(plot_happy_A)
ggsave(filename="plot_happy_A.pdf", plot=plot_happy_A+theme(plot.margin=unit(c(3,3,3,3),"cm")), width = 297/1.5, height = 210/1.5, units = "mm", device=cairo_pdf)

plot_happy_B<-ggplot(data=wyniki, mapping=aes(x=przedzial_wieku2, group=1))+
  geom_line(aes(y=udzial_zycie_wsp_ud_dosycdobre, col="wspaniałe + udane + dosyć dobre"), size=1)+
  geom_point(aes(y=udzial_zycie_wsp_ud_dosycdobre, col="wspaniałe + udane + dosyć dobre"), size=1)+
  geom_line(aes(y=udzial_zycie_wsp_udane, col="wspaniałe + udane"), size=1)+
  geom_point(aes(y=udzial_zycie_wsp_udane, col="wspaniałe + udane"), size=1)+
  geom_line(aes(y=udzial_zycie_wspaniale, col="wspaniałe"), size=1)+
  geom_point(aes(y=udzial_zycie_wspaniale, col="wspaniałe"), size=1)+
  ggthemes::theme_economist()+  
  scale_colour_manual("", 
                      breaks = c("wspaniałe + udane + dosyć dobre", "wspaniałe + udane", "wspaniałe"),
                      values = c("black", "darkblue", "blue"))+
  ylab("udział osób oceniających swoje życie jako...")+
  xlab("przedział wiekowy")+ 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))+ 
  theme(legend.text=element_text(size=8.5))

print(plot_happy_B)

ggsave(filename="plot_happy_B.pdf", plot=plot_happy_B+theme(plot.margin=unit(c(3,3,3,3),"cm")), width = 297/1.5, height = 210/1.5, units = "mm", device=cairo_pdf)
