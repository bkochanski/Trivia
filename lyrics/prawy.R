lyr <- "W dużej sali duży stół
A przy nim gości tłum
Gospodarz zgięty wpół
Bije łychą w szklanę

Cisza chciałbym toast wznieść
Jak można to na cześć
Ojczyzny w której wieść
przyszło życie nasze hej

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego

A na stole śledzik był
Zobaczył go pan Zbych
I pojął dobrze w mig
Że śledzik lubi pływać

Wstał by nowy toast wniesć
Za rodzin świętą rzecz
No i teściowych rzecz
Rodzina to jest siła!

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego

Dzisiaj młodzież już nie ta
Użalał się pan Stach
Lecz ręką machnął tak
Że wylał barszcz na panią

Nic to jednak przecież bo
Sukienkę można zdjąć
A toast wznosi ktoś
Za dobre wychowanie

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego

Pana Kazia kolej to
Więc krawat ściągnął bo
Przecież postarza go
I choć był już na bani

Bez pomocy z gracją wstał
Jąkając się dał znak
By wypić teraz za
Balony pani Mani

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego

Racja brachu
Wypijmy za to
Kto z nami nie wypije
Tego we dwa kije
Prawy do lewego
Wypij kolego
Przecież wiemy nigdy nie ma tego złego"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>4]
#df[order(-df$Freq),]

words2<- words
mainwords <- df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>4]
words2[!(words %in% mainwords)]<-"inne słowa"
words3<-factor(words2, level = c(mainwords, "inne słowa"))
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    main="'Prawy do lewego'\nsłowa składowe", 
    radius = 1, cex = .8, 
    labels=
      paste(levels(words3),
            '-', 
            paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ","),'%')))
