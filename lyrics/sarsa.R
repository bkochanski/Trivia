lyr <- "
Jutro znowu gonić, biec, latać ponad!
Ile sił mam, krzyczę: to ja!
Kiedy miga świat w twoich oczach,
Naucz mnie...

Na ciele rozmażę z mych łez tatuaże,
dwa metry ma strach, nie wolno się bać.
A szyfry, pułapki w słowach zostawię.
Pokonać chcesz nas - nie zostanę

Nie chcę ciebie na zawsze,
nie schowasz nas na dnie.
Tak mało mnie we mnie jest,
I nie zobaczysz, że patrzę na ciebie inaczej.
Nie chciałeś mnie takiej mieć..

Jutro znowu gonić, biec, latać ponad!
Ile sił mam, krzyczę: to ja!
Kiedy miga świat w twoich oczach,
Naucz mnie, naucz mnie od nowa! 
Naucz mnie, naucz mnie od nowa! 

Na ciele rozmażę z mych łez tatuaże,
Ostatnią już noc mi zabiera.
I męczy mnie ciągle, myśl, że już nie zdążę.
Oddalasz się znów z każdym słowem.

Nie zobaczysz, że patrzę na ciebie inaczej.
Nie chciałeś mnie takiej mieć.

Jutro znowu gonić, biec, latać ponad!
Ile sił mam, krzyczę: to ja!
Kiedy miga świat w twoich oczach,
Naucz mnie, naucz mnie od nowa!
Naucz mnie, naucz mnie od nowa!

Nie chciałeś mnie takiej, więc naucz mnie sobą być.
Nie chciałeś mnie takiej, więc naucz mnie sobą być.

Jutro znowu gonić, biec, latać ponad!
Ile sił mam, krzyczę: to ja!
Kiedy miga świat w twoich oczach,
Naucz mnie, naucz mnie od nowa!
Naucz mnie, naucz mnie od nowa!
"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("naucz", "mnie", "od", "nowa")
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
    main="Sarsa - 'Naucz mnie'\nsłowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


