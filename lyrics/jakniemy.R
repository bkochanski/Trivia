lyr <- "Bo jak nie my to kto
Bo jak nie my to kto
Bo jak nie my to kto oh oh oh
Bo jak nie my to kto
Bo jak nie my to nikt tego lepiej nie zrobi tu
Daj więcej beatu maestro
Buja się całe sąsiedztwo
Nie znamy granic i przez to
Potem czujemy się kiepsko
Niedouczeni na błędach
Dzisiaj się tworzy legenda
Za pomocą stopy i werbla
Dodaj do tego Amsterdam
Wiesz dokładnie o co chodzi nam
Bo Ty chcesz osiągać idealny stan
Kto
Bo jak nie my to kto
Bo jak nie my to kto oh oh
Bo jak nie my to kto
Bo jak nie my to nikt tego lepiej nie zrobi tu
To są nasze najlepsze dni
Zaskakuje sam siebie i
Potem z rana już nie wiem nic
Ciągle mało jest jeszcze mi
Moje życie bywa jak film
Imprezuje jak Charlie Sheen
Dynia pęka jak w Halloween
Drina goni kolejny drin
Wiesz dokładnie o co chodzi nam
Bo ja też nocami odbijam się od ścian
Kto
Bo jak nie my to kto
Bo jak nie my to kto oh oh oh
Bo jak nie my to kto
Bo jak nie my to nikt tego lepiej nie zrobi tu
My lubimy jazz i lubimy chillout
Jaki ma sens się dziś spinać
Buzuje w nas ta endorfina
Sukienka pin up tak ciebie opina
Oh oh Błędny wzrok kolejny shot łapię trop
Brzdęk brzdęk znika lęk
Dzisiaj w klubie będzie bang
Jak ja dobrze to znam
Grzeszne spojrzenia
Tych grzecznych dam
Ciało wyginasz
Tym łowisz mnie
Jaki będzie finał
Kto to wie
Co z nami będzie
Chcemy więcej
Każdy zuch łapie groove
Nie znajdziesz nigdzie takich trzech
Jak nas dwóch
Kto
Bo jak nie my to kto
Bo jak nie my to kto oh oh oh
Bo jak nie my to kto
Bo jak nie my to nikt tego lepiej nie zrobi tu"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
df<-data.frame(table(words))
#View(df)
df$words[df$Freq>8]
df[order(-df$Freq),]
#words[substring(words, 1, 15)=="małomiasteczkow"] <- 'małomiasteczkow*'

words2<- words
mainwords <- c("bo", "jak", "nie", "my", "to", "kto")
words2[!(words %in% mainwords)]<-"inne słowa"
words3<-factor(words2, level = c(mainwords, "inne słowa"))
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    main="'Jak nie my'\nsłowa składowe", 
    radius = 1, cex = .8, 
    labels=
      paste(levels(words3),
            '-', 
            paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ","),'%')))
