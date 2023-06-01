library(googlesheets4)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(bdl)

a<-read_sheet("1PTBhcw75JHH49v6_hERnET0IrCsdJg3FV7KpPJnML6g", sheet="wakaty_23.05.31")
bnw<-as.character(t(as.matrix(a[3,3:18])))
bn<-as.numeric(t(as.matrix(a[10,3:18])))*100

dane<-bdl::get_data_by_variable("461683",unitLevel = "2", year=2023)
bezw<-dane$name
bez<-dane$val

woj<-bnw
woj[woj=="kuj-pom"]<-"kuj"

sort(bezw)
sort(bnw)

bno<-bn[order(bnw)]
bezo<-bez[order(bezw)]
bezwo<-bezw[order(bezw)]

df<-data.frame(woj = bezwo, stopa_bezrob = bezo, braki_nau = bno)
cor(bezo, bno)

ggplot(data=df, aes(x=stopa_bezrob, y=braki_nau, label=woj)) + 
  xlab("stopa bezrobocia rejestrowanego w kwietniu 2023 (w %, GUS)") +
  ylab("braki nauczycieli w maju 2023 (w %, Dealerzy Wiedzy)") +
  xlim(c(0, round(max(bezo)+1))) +
  ylim(c(0, 4)) +
  ggtitle(paste0("Korelacja regionalna pomiędzy poziomem bezrobocia \na wskaźnikiem braków nauczycieli wynosi ", format(round(cor(bezo, bno),2), decimal.mark=','))) +
  theme_bw() +
  geom_point(size=3) +
  geom_text_repel(size=3) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", 
              se=FALSE, 
              colour='blue')
