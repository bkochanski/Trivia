lyr <- "Jestem słodka
Daje niebo
Jestem gorzka
Daje piekło
Do nieba (nieba), do piekła
Za tobą będę szła
Do nieba (nieba), do piekła
Pójdę tam, gdzie się da
Jestem dzika
Daję burze
Jestem cicha
Daję susze
Do nieba (nieba), do piekła
Za tobą będę szła
Do nieba (nieba), do piekła
Pójdę tam, gdzie się da
Daję niebo, co w zamian dasz?
Daję piekło, co ty mi dasz?
Daję niebo, co w zamian dasz?
Daję piekło, co ty mi dasz?
Daję niebo, co w zamian dasz?
Daję piekło, co ty mi dasz?
Daję niebo, co w zamian dasz?
Daję piekło, co ty mi dasz?
Do nieba (nieba), do piekła
Za tobą będę szła
Do nieba (nieba), do piekła
Pójdę tam, gdzie się da"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("do", "nieba", "piekła", "niebo", "piekło", "daję", "dasz", "co")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
#words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main="Blue Cafe, 'Do nieba, do piekła'\nsłowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


