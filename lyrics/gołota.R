lyr <- "Niepokonany w dwudziestu ośmiu walkach
A pokonany w dwóch ostatnich walkach
Niepokonany w dwudziestu ośmiu walkach
A oszukany w dwóch ostatnich walkach
Niepokonany w dwudziestu ośmiu walkach
A pokonany w dwóch ostatnich walkach
Niepokonany w dwudziestu ośmiu walkach
Uwolniony z miasta Włocławka

Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota

Myślę sobie czerwcowego poranka
Gdyby tu było przedszkole w przyszłości
Niepokonany w trzydziestu dziewięciu walkach
Greenpoint i Milwaukee chleją z radości
Niepokonany w czterdziestu ośmiu walkach
A pokonany w trzech jedynie walkach
Niepokonany w stu osiemnastu walkach
Kolejny oponent na kolankach

Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota

Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota

Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota
Andrzej Gołota, Gołota, Gołota, Gołota

Evander Holyfield, Mike Tyson,
Michael Moorer, Tim Witherspoon,
Ray Mercer, Lennox Lewis,
Joe Hipp, Everton Davis,
David Tua, Zeljko Mavrovic,
Vaughn Bean, Anton Josipovic,
Orlin Norris, Frans Botha,
Henry Akinwande, Andrzej Gołota"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("andrzej", "gołota", "w", "walkach", "niepokonany", "pokonany")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
#words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
levels(words3)[1] <- "Andrzej"
levels(words3)[2] <- "Gołota"
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main="Andrzej Gołota Kazika - słowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


