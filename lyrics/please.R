lyr <- "Please, please, please, please me (You don't have to go)
Baby please, baby please, please me (You don't have to go)
Baby please, baby please don't go (You don't have to go)
Don't go, I said baby, don't baby
I love you so (You don't have to go)
Baby, you know you broke my heart when you went away (You don't have to go)
I said, I said, I said I'll see you some other day (You don't have to go)
I said, baby, baby, please, don't go (You don't have to go)
Don't go, no baby, no baby
I love you so (You don't have to go)
Take this pain from my heart
Baby, let me take you by the hand (You don't have to go)
Baby, baby let me, let me be your lover man (You don't have to go)
Baby please, baby please don't go (You don't have to go)
Don't go, I said baby, don't baby
I love you so (You don't have to go)
Please, please, please, please me (You don't have to go)
Baby please, baby please, please me (You don't have to go)
Baby please, baby please don't go (You don't have to go)
Don't go, I said baby, don't baby
I love you so (You don't have to go)
Please please me"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]&&[^']]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]


mainwords <- c("i", "said", "baby", "please", "you", "don't", "have", "to", "go")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
# words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
t1 <- 'Please, Please, Please - James Brown i The Famous Flame'
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


