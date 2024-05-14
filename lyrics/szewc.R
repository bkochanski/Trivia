lyr <- "O! O! O!

Oł! Oł! Oł!

Lalalalalalalalalalalala...
Lalalalalalalalalalalala...
Lalalalalalalalalalalala...
Lalalalalalalalalalalala...

Szewc zabija szewca, bum tarara bum tarara.
Szewc zabija szewca, bum tarara bum tarara.
Bum tarara, bum tarara, bum tarara, bum tarara.
Bum tarara, bum tarara, bum tarara, bum tarara.

Oł! Oł! Oł!

Lalalalalalalalalalalala...
Lalalalalalalalalalalala...
Lalalalalalalalalalalala...
Lalalalalalalalalalalala...

Szewc zabija szewca, bum tarara bum tarara.
Szewc zabija szewca, bum tarara bum tarara.
Bum tarara, bum tarara, bum tarara, bum tarara.
Bum tarara, bum tarara, bum tarara, bum tarara"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]

mainwords <- c("szewc", "zabija", "szewca", "bum", "tarara", "lalalalalalalalalalalala", "o", "oł")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
#words_levels <-c(mainwords, "inne słowa")
words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main='Siekiera: Misiowie puszyści\nsłowa składowe', 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


