lyr <- "Małomiasteczkowa twarz
Małomiasteczkowa głowa
Małomiasteczkowy styl
Małomiasteczkowo kocham
Z małego miasta wielkie sny
Atakują twoje ulice
Wyśniłem sobie ciebie, gdy
Śpiewałem głośno pod prysznicem
Ten mój małomiasteczkowy hit
I małomiasteczkowe słowa
Ten małomiasteczkowy rytm
Melodia małomiasteczkowa
Z małego miasta wielkie sny
Gromadzą się na twoich ulicach
Pamiętam, bardzo chciałem tu być
Na pewno dużo bardziej niż dzisiaj
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Przez chwilę czułem się jak Bóg
Przez chwilę byłem królem w mieście
Wybrałem na siłownię strój
I wtedy zrozumiałem wreszcie
Że z mojego miasta moje sny
Budują twoje ulice
Że ciebie nie zachwyca tu nic
Ale smuci mnie, że nadal nie krzyczę
Gdy wielkomiejski piękny świat
Na każdym kroku sypie kreski
Uściski i klepnięcia w bark
Płynące ze wzruszenia łezki
Dlaczego wszystko sztuczne aż tak
Że napromieniowane mi świeci?
Trzeba stąd wyjechać, bo strach
Że wszystko przejdzie na moje dzieci
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Znowu jadę do ciebie sam
Znowu jadę do ciebie
Sam"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
words[substring(words, 1, 15)=="małomiasteczkow"] <- 'małomiasteczkow*'

words2<- words
mainwords <- c("znowu", "jadę", "do", "ciebie", "sam", "małomiasteczkow*")
words2[!(words %in% mainwords)]<-"inne słowa"
words3<-factor(words2, level = c(mainwords, "inne słowa"))
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    main="'Małomiasteczkowy'\nsłowa składowe", 
    radius = 1, cex = .8, 
    labels=
      paste(levels(words3),
            '-', 
            paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ","),'%')))
