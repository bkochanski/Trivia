lyr <- "I nie mam po co iść, jeśli Ciebie nie spotkam
Ściska tłum, a ja cała samotna
Po co iść, jeśli Ciebie nie spotkam
Ściska tłum, a ja cała samotna

Na trzeźwo wiąże buty
Przez półtorej minuty
A ja wiem
Że to ten

Zje pół mojego dania
Nie zadając pytania
A ja wiem
Że to ten

Gra rano na gitarze
Gdy o śniadaniu marzę
A ja wiem
Że to ten

Ma spadające spodnie
Twierdzi, że mu wygodnie
A ja wiem
Że to ten

I nie mam po co iść, jeśli Ciebie nie spotkam
Ściska tłum, a ja cała samotna
Po co iść, jeśli Ciebie nie spotkam
Ściska tłum, a ja cała samotna

Gotuje mi makaron
Bo tak mi źle, tak szaro
A ja wiem
Że to ten

Pisze listy miłosne
Kiedy czekam na wiosnę
A ja wiem
Że to ten

Piękny nieprzyzwoicie
Przyspiesza serca bicie
A ja wiem
Że to ten

Już wchodzi na salony
Wzrok na mnie podniesiony
I ja wiem
Że to ten

I nie mam po co iść, jeśli Ciebie nie spotkam
Ściska tłum, a ja cała samotna
Po co iść, jeśli Ciebie nie spotkam
Ściska tłum, a ja cała samotna"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]


mainwords <- c("a", "ja", "wiem", "że", "to", "ten", "nie", "cała", "ciebie", "jeśli", "spotkam", "samotna", "ściska", "tłum")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
# words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
pie(table(words3), 
    clockwise=TRUE, 
    col=viridis::viridis(length(words_levels)), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main='Zuta: Elvis\nsłowa składowe', 
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
  theme_void() + theme(legend.position="none") + title('Test') +scale_fill_viridis_d() + 
  ggtitle("Zuta: Elvis", "słowa składowe")


