lyr <- "Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Pijani od słów
Pijani od słów
Pijani od słów
Pijani od słów
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Pijani od słów
Pijani od słów
Pijani od słów
Pijani od słów
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Pijani od słów
Pijani od słów
Pijani od słów
Pijani od słów
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Pijani od słów
Pijani od słów
Pijani od słów
Pijani od słów
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Pijani od słów
Pijani od słów
Pijani od słów
Pijani od słów
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las
Pijani od słów
Pijani od słów
Pijani od słów
Pijani od słów
Idziemy przez las
Idziemy przez las
Idziemy przez las
Idziemy przez las"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("idziemy", "przez", "las", "pijani", "od", "słów")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
# words_levels <-c(mainwords, "inne słowa")
words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
pie(table(words3), 
    clockwise=TRUE, 
    col=viridis::viridis(length(words_levels)), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main='Siekiera: Idziemy przez las\nsłowa składowe', 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


