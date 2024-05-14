lyr <- "To piosenka o kobietach będzie tak mi się wydaje
I chłopakach też co wytrzeszczają gały
Na dziewczyny długonogie które chodzą ulicami
Roztańczone rozbawione bujające w obłokach
Nie nie nie
Biodrami kolego robią to w biały dzień
A ja nie wiem dlaczego
Lód się w rękach topi serduszko mocno bije
Kiedy widzisz dziewczę rzucające się na szyję
Nie nie tobie
Twojemu sąsiadowi
Szlag cię wtedy trafia i nie wiesz co robić
Nie rób nic tylko pomyśl o gorących piaskach
O błękitnym oceanie balujących laskach razem z tobą
W klubie gdzie kolesi tłumy walą
Ogolonych ładnych jak z reklamy
A to wszystko by poskakać sobie trochę
Poprzytulać się do siebie
Już nie mogę
Daj spokój
Kobiety są aha aha gorące aha aha
Kobiety są hej
Kobiety są aha aha gorące aha aha
Kobiety są hej ho
To kobiety sprawiają że odjeżdżasz daleko
Nie pomoże Ci psychiatra nie pomoże żaden lekarz
Wiadro z lodem i pokrzywą możesz sobie wsadzić w d
Tak już jest na tym świecie
Że gdy słonko przygrzeje
To chłopaki jak idioci oglądają się za siebie
Na ulicach dzielnicach piaskownicach
Też też aha
A to wszystko przez kobiety
Bo zrzucają z siebie dużo kilogramów ciuchów
Eksponując delikatne prawie nagie ciała
Na facetów to działa
Kobiety są aha aha gorące aha aha
Kobiety są hej
Kobiety są acha aha gorące aha aha
Kobiety są hej ho
Nie rób nic tylko pomyśl o gorących piaskach
O błękitnym oceanie balujących laskach razem z tobą
W klubie gdzie kolesi tłumy walą ogolonych ładnie jak z reklamy
A to wszystko by poskakać sobie trochę
Poprzytulać się do siebie
Stop
Kobiety są kobiety
Kobiety są gorące ha
Kobiety są oh
Kobiety są aha aha gorące aha aha
Kobiety są he"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]


mainwords <- c("kobiety", "to", "nie", "są", "gorące", "aha")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
# words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
t1 <- 'Norbi: Kobiety są gorące'
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
  theme_void() + theme(legend.position="none") + title('Test') +scale_fill_viridis_d() + 
  ggtitle(t1, t2)


