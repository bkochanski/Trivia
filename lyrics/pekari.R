lyr <- "Sen to czy mara jest?
Sen czy mara to?
Sen to czy mara?
Czy flet to fujara jest?
Czy fujara to flet czy fujara?
Por to czy pora jest?
Por czy pora to?
Por to czy pora?
Czy lęk to czy zmora jest?
Lęk czy zmora to?
Lęk to czy zmora?

Leopardy kuguary
Mrówkojady i kalmary
Tarantule pajęczaki
Barakudy i szczupaki
Pekari pekari pekari pekari
Okapi okapi okapi okapi
I ślady i ślady i ślady i ślady
I znaki i znaki i znaki i znaki

Kur to czy kura jest?
Kur czy kura to?
Kur to czy kura?
Czy kurew to córa jest?
I czy kurew to Koryntu córa?
Na rękach czy ręcach mieć?
Czy na rękach czy ręcach?
Czy na mękach śmiać się chcieć?
Czy na mękach śmiać się czy kwękać?
Czy jękać czy jąkać się?
Jękać - jąkać się, jękać czy jąkać?
Czy na deser zmieszać coś?
Czy przypalić coś, cmokać czy wąchać?

Leopardy kuguary
Mrówkojady i kalmary
Tarantule pajęczaki
Barakudy i szczupaki
Pekari pekari pekari pekari
Okapi okapi okapi okapi
I ślady i ślady i ślady i ślady
I znaki i znaki i znaki i znaki
Pytania pytania pytania pytania
Wyzwania wyzwania wyzwania wyzwania
Szukania szukania szukania szukania
Zadania zadania zadania zadania

Wszystko się może zdarzyć"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]


mainwords <- c("i", "czy", "to", "jest", "okapi", "pekari", "ślady", "znaki", "pytania", "wyzwania", "szukania", "zadania")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
# words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
t1 <- 'Galago: Okapi'
t2 <- 'słowa składowe'
pie(table(words3), 
    clockwise=TRUE, 
    col=viridis::viridis(length(words_levels)), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main=paste0(t1, '\n', t2),  
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))



df2<-as.data.frame(table(words3))

library(ggplot2)
library(ggrepel)
library(dplyr)

df2 <- df2 %>%
  mutate(csum = rev(cumsum(rev(Freq))),
         pos = Freq / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), Freq / 2, pos))

ggplot(data = df2, aes(x = "", y = Freq, fill = words3)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y", direction=-1) +
  geom_text_repel(data = df2, aes(y = pos, label = paste0(words3, ' ', format(round(Freq/sum(Freq)*100,1), decimal.mark=','), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE, col='black') +
  theme_void() + theme(legend.position="none") +scale_fill_viridis_d() + 
  ggtitle(t1, t2)


