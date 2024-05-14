lyr <- "Sto lat, sto lat,
Niech żyje, żyje nam.
Sto lat, sto lat,
Niech żyje, żyje nam,
Jeszcze raz, jeszcze raz, niech żyje, żyje nam,
Niech żyje nam"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("sto", "lat", "niech", "żyje", "nam", "jeszcze","raz")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
     main="Sto lat - słowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


