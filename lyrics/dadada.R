lyr <- "Aha, aha, aha
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie a ty mnie
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, a ty mnie
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, ty nie kochasz mnie (aha)
Ja nie kocham ciebie, ty nie kochasz mnie
Ja nie kocham ciebie, a ty mnie
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da (ja nie kocham ciebie, ty nie kochasz mnie)
Da, da, da (ja nie kocham ciebie, ty nie kochasz mnie)
Da, da, da (ja nie kocham ciebie, ty nie kochasz mnie)
Da, da, da (ja nie kocham ciebie, a ty mnie)
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Da, da, da
Aha, aha, aha"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]&&[^']]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>1]
#df[order(-df$Freq),]


mainwords <- df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>1]
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
#words_levels <-c(mainwords, "inne słowa")
words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
t1 <- 'Formacja Nieżywych Schabuff - Da, da, da'
t2 <- 'słowa składowe'
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
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


