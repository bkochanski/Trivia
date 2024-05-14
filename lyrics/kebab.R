lyr <- "Wyszedłem dzisiaj z roboty
Gdyż nie miałem co robić
W robocie
Hej yeah
Poszedłem do turka na rogu
I mówię mu daj mi proszę
Kebab w cienkim cieście
Oł yeah
A on mi na to że niestety
Zabrakło baraniny
A on mi na to że niestety
Zabrakło baraniny
Kebab w cienkim cieście
Po nocach mi się śni
Po nocach mi się śni
Kebab w cienkim cieście
Och dajcież dzisiaj mi
Och dajcież mi dajcież mi
Kebab w cienkim cieście
Po nocach mi się śni
O ti di di di di
Kebab w cienkim cieście
Dajcież mi
Odszedłem niepocieszony
I do nocy włóczyłem się
Po moim mieście
Na zawsze zapadły w mej pamięci
Te słowa jakże okrutne
I złowieszcze
Bo to dzisiaj właśnie dzisiaj
Zabrakło baraniny
Bo to dzisiaj właśnie dzisiaj
Zabrakło baraniny
Kebab w cienkim cieście
Po nocach mi się śni
Po nocach mi się śni
Kebab w cienkim cieście
Oh dajcież dzisiaj mi
O dajcież mi dajcież
Kebab w cienkim cieście
Po nocach mi się śni
O ti di ti di ti di di di di
Kebab w cienkim cieście
Dajcież mi
Nie pójdę już więcej na kebab
I do śmierci będę żywił się jajecznicą
Lecz czasem serce moje zadrga
I zaskowyczę niczym hiena nad Pilicą
Bo to dzisiaj właśnie dzisiaj
Zabrakło baraniny
Bo to dzisiaj właśnie dzisiaj
Zabrakło baraniny
Kebab w cienkim cieście
Po nocach mi się śni
Po nocach mi się śni
Kebab w cienkim cieście
Och dajcież dzisiaj mi
Och dajcież mi dajcież mi
Kebab w cienkim cieście
Po nocach mi się śni
O ti di di di di
Kebab w cienkim cieście
Oł yeah yeah yeah yeah
Kebab
W cienkim cieście
Oł yeah
Oh kebab"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]


mainwords <- c("di", "dajcież", "mi", "dzisiaj", "kebab", "w", "cienkim", "cieście", "się", "po", "nocach", "śni", "yeah")
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
    main='Zacier: Kebab w cienkim cieście\nsłowa składowe', 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


