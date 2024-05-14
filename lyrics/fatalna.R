lyr <- "Nie mogę w nocy zasnąć kręce sie i wiercę wypity drink rozsadza moje serce mysle o kobiecie ktorą dzisiaj widziałem kazdy by zachwycił się jej powabnym ciałem wracałem zmeczony po nocy dlugiej słonce swieciło jasno było chyba po drugiej gdy spojrzałem na zegarek mojego słocza bo o czas zapytała ta dziewczyna urocza widzac jej cialo cos we mnie zadrżało a w mojej białej głowie az zawirowało była w moim wieku miała krucze czarne włosy a po nich spłyneła kropelka rosy spytałem czy podaruje mi odrobine czasu swego o dziwo nie wzniosła sprzeciwu zadnego me troski znalazły sie na drugim planie odużony oczekiwałem co się stanie


Ja kobieta fatalna uwiodła go ja kobieta fatalna opętałam go ja kobieta fatalna męcze go we śnie ja kobieta fatalna...


Wpatrzeni w siebie siedzieliśmy we dwoje opowiadałem jej przygody moje tak to była miłość od pierwszego wejrzenia dostałem na jej punkcie juz uzależnienia kupiłem pare drinkow by przełamac pierwsze lody wypuscic nasza milosc na otwarte wole a kiedy do konca dobiegł ostatni trunek poczułem na ustach słodki pocałunek objołem ja czule muskałem włosy niech ta chwila trwa wiecznie nigdy nie będe miał dosyc aksamitne usta wędrowały po mym karku a ręka przebiła się do najskrytszych zakamarków zrzuciła swoje szaty po chwili zdarła moje nie rób samego co mozna we dwoje po wszystkim przeprosiła ze byla nachalna odeszla bo to była kobieta fatalna


Ja kobieta fatalna uwiodłam go ja kobieta fatalna opętałam go ja kobieta fatalna męcze go we snie ja kobieta fatalna...


Już wiem dałem uwieść sie a ta dziewczyna tylko wykorzystała mnie a wyglądała mi na kobiete z polotem na taka ktora mężczyzn potem nie zmiesza z błotem niestety to był błąd że jej zaufałem i w sidła miłości nie potrzebnie się wdałem wpadłem w zasacke kobiety pająka ile takich istot dzis po swiecie sie błąka wykorzystujac nas bezbronnych chłopaków by potem nas traktować jak dzielnych pędraków i może to dobrze że znowu jestem sam bo szczerze mówiąc coś teraz powiem wam dobrze że są na świecie takie panie ktorym w głowie jest tylko mężczyzno-branie gdyz chłopcy to lubią jak gadke banalną uda im sie zdobyć kobiete fatalną i wilk jest wtedy szyty i owca cała wiec cieszmy sie wolnoscią ktora teraz nastała bo czasem warto isc na te randkę feralną by poznać osobiście kobiete fatalną.

ja kobieta fatalna uwiodłam go ja kobieta fatalna opętałam go ja kobieta fatalna męcze go we snie ja kobieta fatalna"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
View(data.frame(table(words)))

mainwords <- c("ja", "kobieta", "fatalna")
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
    main="'Kobieta fatalna'\nsłowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


