bilet<-bdl::get_data_by_variable("5083", unitLevel=0, lang='en')[,c("year", "val")]
names(bilet)<-c("rok", "bilet")
wodka<-bdl::get_data_by_variable("8265", unitLevel=0, lang='en')[,c("year", "val")]
names(wodka)<-c("rok", "wodka")
sokjablkowy<-bdl::get_data_by_variable("4990", unitLevel=0, lang='en')[,c("year", "val")]
names(sokjablkowy)<-c("rok", "sokjablkowy")
kurcze<-bdl::get_data_by_variable("4924", unitLevel=0, lang='en')[,c("year", "val")]
names(kurcze)<-c("rok", "kurcze")
piwo<-bdl::get_data_by_variable("5003", unitLevel=0, lang='en')[,c("year", "val")]
names(piwo)<-c("rok", "piwo")

df_list <- list(bilet, kurcze, piwo, sokjablkowy, wodka)
# Merge all data frames in the list
library(purrr)
df <- reduce(df_list, full_join, by = "rok")
dfl<-tidyr::pivot_longer(df, c("bilet", "kurcze", "piwo", "sokjablkowy", "wodka"))
dfl$name<-factor(dfl$name)
dfl$rok<-as.numeric(dfl$rok)
library(dplyr)
dfl%>%filter(name %in% c("kurcze", "bilet"))->dfl2

library(ggplot2)
ggplot(data=dfl2, aes(x=rok, y=value, colour=name, group=name))+
  geom_line() +
  geom_point() +
  theme_minimal()+  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),    
    axis.line = element_line(color = "black")
  ) + xlab('') + ylab('Cena [zł]') + ylim(c(0,70))

library(ggplot2)
df$rok<-as.numeric(df$rok)
cols <- c("bilet do teatru"="blue","kurczak patroszony – 1 kg"="red")
ggplot(data=df)+
  geom_line(aes(x=rok, y=bilet, colour="bilet do teatru")) +
  geom_point(aes(x=rok, y=bilet, colour="bilet do teatru"))+  
  geom_line(aes(x=rok, y=kurcze*4, colour='kurczak patroszony – 1 kg')) +
  geom_point(aes(x=rok, y=kurcze*4, colour='kurczak patroszony – 1 kg'))+  
  theme_minimal()+ xlab('') +  
  scale_y_continuous(
    name = "Cena biletu [zł]", limits = c(0, 70),
    sec.axis = sec_axis(~ . / 4, name = "Cena 1 kg kurczaka [zł]")  
  ) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),    
    axis.line = element_line(color = "black"),
    legend.position = "bottom"                        
  ) +scale_colour_manual(name="",values=cols)

kino<-bdl::get_data_by_variable("5082", unitLevel=0, lang='en')[,c("year", "val")]
names(kino)<-c("rok", "kino")
piwo<-bdl::get_data_by_variable("5003", unitLevel=0, lang='en')[,c("year", "val")]
names(piwo)<-c("rok", "piwo")

df<-merge(kino,piwo)
df$rok<-as.numeric(df$rok)
cols <- c("bilet do kina"="blue","piwo"="red")
ggplot(data=df)+
  geom_line(aes(x=rok, y=kino, colour="bilet do kina")) +
  geom_point(aes(x=rok, y=kino, colour="bilet do kina"))+  
  geom_line(aes(x=rok, y=piwo*3, colour='piwo')) +
  geom_point(aes(x=rok, y=piwo*3, colour='piwo'))+  
  theme_minimal()+ xlab('') +  
  scale_y_continuous(
    name = "Cena biletu [zł]", limits = c(0, 22),
    sec.axis = sec_axis(~ . / 3, name = "Cena piwa [zł]")  
  ) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),    
    axis.line = element_line(color = "black"),
    legend.position = "bottom"                        
  ) +scale_colour_manual(name="",values=cols)
