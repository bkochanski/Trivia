lyr <- "Chciałbym być sobą
Chciałbym być sobą wreszcie
Chciałbym być sobą
Chciałbym być sobą jeszcze
Chciałbym być sobą
Chciałbym być sobą wreszcie
Chciałbym być sobą
Chciałbym być sobą jeszcze
Jak co dzień rano bułkę maślaną
Popijam kawą nad gazety plamą
Nikt mi nie powie, wiem co mam robić
Szklanką o ścianę rzucam, chcę wychodzić
Na klatce stoi cieć, co się boi
Nawet odsłonić, miotłę ściska w dłoni
Ortalion szary, chwytam za bary
I przerażonej twarzy krzyczę prosto w nos
Chciałbym być sobą
Chciałbym być sobą wreszcie
Chciałbym być sobą
Chciałbym być sobą jeszcze
Chciałbym być sobą
Chciałbym być sobą wreszcie
Chciałbym być sobą
Chciałbym być sobą jeszcze
Trzymam się ściany, niczym pijany
Tłum wkoło tańczy tangiem opętany
Stopy zmęczone, depczą koronę
Król balu zwleka, oczy ma szalone
Magda w podzięce, chwyta me ręce
I nie ma sprawy, ślicznie jej w sukience
Po co sie śpieszysz, po co sie śpieszysz
Przecież do końca życia mamy na to czas
Aby być sobą
Aby być sobą jeszcze
Aby być sobą
Aby byc sobą wreszcie
Chciałbym być sobą
Chciałbym być sobą wreszcie
Chciałbym być sobą
Chciałbym być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą wreszcie
Chcemy być sobą
Chcemy być sobą jeszcze
Chcemy być sobą
Chcemy być sobą wreszcie"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
View(data.frame(table(words)))

mainwords <- c("chcemy", "chciałbym", "być", "sobą", "wreszcie", "jeszcze")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
#words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
levels(words3)<-c('k...a', 'ch.j', 'wóda', 'i', 'p...a', "inne słowa")
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main="Perfect - 'Chcemy być sobą'\nsłowa składowe", 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark=','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


