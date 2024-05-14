lyr <- "Mój dom murem podzielony
Podzielone murem schody
Po lewej stronie łazienka
Po prawej stronie kuchenka

Mój dom murem podzielony
Podzielone murem schody
Po lewej stronie łazienka
Po prawej…

Moje ciało murem podzielone
Dziesięć palców na lewą stronę
Drugie dziesięć na prawą stronę
Głowy równa część na każdą stronę

Moje ciało murem podzielone
Dziesięć palców na lewą stronę
Drugie dziesięć na prawą stronę
Głowy równa część na każdą stronę

Moja ulica murem podzielona
Świeci neonami prawa strona
Lewa strona cała wygaszona
Zza zasłony obserwuję obie strony

Moja ulica murem podzielona
Świeci neonami prawa strona
Lewa strona cała wygaszona
Zza zasłony obserwuję obie strony

Moja ulica murem podzielona
Świeci neonami prawa strona
Lewa strona cała wygaszona
Zza zasłony obserwuję obie strony

Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia
Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia
Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia
Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia
Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia
Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia
Lewa strona nigdy się nie budzi
Prawa strona nigdy nie zasypia"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
View(data.frame(table(words)))

mainwords <- c("prawa", "lewa", "strona", "nigdy", "się", "nie", "budzi", "zasypia")
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
    main="'Arahja'\nsłowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


