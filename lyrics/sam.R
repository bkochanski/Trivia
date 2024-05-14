lyr <- "Tak, zawsze genialny
Idealny muszę być
I muszę chcieć super luz i już
Setki bzdur i już, to nie ja
I nawet kiedy będę sam
Nie zmienię się, to nie mój świat
Przede mną droga którą znam
Którą ja wybrałem sam
Wiesz, lubię wieczory
Lubię się schować na jakiś czas
I jakoś tak, nienaturalnie
Trochę przesadnie, pobyć sam
Wejść na drzewo i patrzeć w niebo
Tak zwyczajnie, tylko że
Tutaj też wiem kolejny raz
Nie mam szans
I nawet kiedy będę sam
Nie zmienię się, to nie mój świat
Przede mną droga którą znam
Którą ja wybrałem sam
Noc, a nocą gdy nie śpię
Wychodzę choć nie chcę spojrzeć na
Chemiczny świat, pachnący szarością
Z papieru miłością, gdzie ty i ja
I jeszcze ktoś, nie wiem kto
Chciałby tak przez kilka lat
Zbyt zachłannie i trochę przesadnie
Pobyć chwilę sam, chyba go znam
I nawet kiedy będę sam
Nie zmienię się, to nie mój świat
Przede mną droga którą znam
Którą ja wybrałem sam
I nawet kiedy będę sam
Nie zmienię się, to nie mój świat
Przede mną droga którą znam
Którą ja wybrałem sam"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]


mainwords <- c("sam", "i", "droga", "którą", "ja", "się", "nie", "znam", "to", "świat", "kiedy", "będę", "mną")
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
    main='A. Rojek: Długość dźwięku samotności\nsłowa składowe', 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


