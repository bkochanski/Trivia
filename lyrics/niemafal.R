lyr <- "Fal, nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Fal, nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Pięknie
Rozmawia się całkiem nieźle
I podoba nam dotychczas każdy obejrzany film
We mnie
Problem jest na pewno we mnie
Nie doszukuj się go w sobie
W tobie wszytko jest okej
Może dasz się znać po tym
Weź mnie znajdź potem
Dziś nie chce popłynąć, bo
Chętnie
Mogę złapać cię za rękę
Jeśli chcesz wyrzucę każdy przeterminowany sos
A to śmieszne
Nie rozumiem ciebie jeszcze
Pani mama chciała żebym więcej niż liceum miał
Ale daj się znać po tym
Gdzieś mnie znajdź potem
Dziś nie chcę popłynąć bo
Fal, nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie
Fal, nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie
Pewnie
Chciałbym ciebie słuchać więcej
Tylko skoczę w jedno miejsce
Potem może znajdę czas
Wcześniej
Spotykałem cię codziennie
Ale teraz kiedy muszę
Chętniej udałbym się spać
Proszę daj się znać po tym
Gdzieś mnie znajdź potem
Dziś nie popłyniemy bo
Fal, nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie
Fal, nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie ma fal
Nie"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("nie", "ma", "fal")
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
    main='D. Podsiadło, Nie ma fal\nsłowa składowe', 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


