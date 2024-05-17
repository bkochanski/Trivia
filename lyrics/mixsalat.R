lyr <- "Jesteś tylko sumą swoich suplementów
Żyjesz żmudno od weekendu do weekendu
Jak pamiętać o poszukiwaniu piękna
Póki jest w portfelu cash, to nie widzę sensu
Chciałbyś przejąć na weselu cały dance floor
Chciałbyś wiedzieć, jak uwolnić się od stresu
Chciałbyś, żeby życie przejął jakiś mentor
Podaj numer karty kredytowej i nie dziękuj
Jesteś sumą swoich spraw do załatwienia
Robisz papier nie ma czasu na marzenia
Chyba zapomniałeś dzisiaj przyjąć błonnik
Twój żołądek robi stale zażalenia
Jesteś sumą wszystkich swoich wad i błędów
Liczba taka sama jak w przelewie z alimentów
Jesteś jedną z osób, którą ciągle pali w sercu
Bo przez całe swoje życie pozostała w miejscu
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz
Szlugi w poniedziałek rzuć, w środę idź do kiosku
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz, powtórz, powtórz
Jesteś sumą swych organów i hormonów
Każdej rany, która winna jest powodu
Czemu zawsze od komedii romantycznej
Jest tak blisko w twoim życiu do horroru
Chciałbyś więcej mieć diamentów niż popiołu
I dlatego znów uciekasz od mozołu
I dlatego znowu w klubie wziąłeś piksę
I wyglądasz jakbyś nie miał oczodołów
Jesteś sumą swoich wszystkich PLN'ów
Jesteś sumą wszystkich linijek w Excelu
Jesteś białym prochem na dnie neseserów
Jesteś mentalnością twoich starych z PRL'u
No bo masz życie, lecz mógłbyś żyć lepiej
No bo masz szefa, lecz mógłbyś być szefem
No bo masz żonę, lecz mogła być lepsza
Bo masz oponę a mógłbyś mieć dietę
powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz
Szlugi w poniedziałek rzuć, w środę idź do kiosku
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz
Kup mix sałat, wyrzuć mix sałat, powtórz, powtórz, powtórz
Jesteś sumą noworocznych postanowień
Które w lutym postanawiasz jakoś obejść
Chciałbyś kiedyś móc za kogoś skoczyć w ogień
(Wtedy może miałbyś o czym gadać z Panem Bogiem)
Chciałbyś umieć robić zdjęcia analogiem
Chciałbyś umieć robić w domu pannacottę
Chciałbyś, by bazylia Ci nie umierała
Zanim chociaż jeden pierdolony dzień postała w oknie
Jesteś sumą swoich traum i niepowodzeń
Które pogrzebujesz gdzieś głęboko w głowie
W głębi duszy to się zgadzasz z każdym hejtem
Gorsze rzeczy mówisz sobie już na co dzień
Nie pij wody, nie wiesz co jest dzisiaj w wodzie
Powiedz jebać modę, to jest dzisiaj w modzie
W radiu mówią, oświecenie jest za rogiem
1-800-OŚWIECENIE, dzwoń, chętnie Ci pomogę
Kup mix sałat, wyrzuć mix sałat, powtórz
Powtórz"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]&&[^']]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>5]
#df[order(-df$Freq),]

mainwords <- c("kup", "mix", "sałat", "powtórz", "wyrzuć", "jesteś", "sumą")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
#words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
levels(words3)[levels(words3)=='pojebane']<-'poj*bane'
levels(words3)[levels(words3)=='kurwa']<-'k***a'

t1 <- 'Mix sałat'
t2 <- 'słowa składowe'
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main=paste0(t1, '\n', t2),  
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))



df2<-as.data.frame(table(words3))

library(ggplot2)
library(ggrepel)
library(dplyr)

df2 <- df2 %>%
  mutate(csum = rev(cumsum(rev(Freq))),
         pos = Freq / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), Freq / 2, pos))

ggplot(data = df2, aes(x = "", y = Freq, fill = words3)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y", direction=-1) +
  geom_text_repel(data = df2, aes(y = pos, label = paste0(words3, ' ', format(round(Freq/sum(Freq)*100,1), decimal.mark=','), "%")),
                  size = 4.5, nudge_x = 1, show.legend = FALSE, col='black') +
  theme_void() + theme(legend.position="none") +scale_fill_viridis_d() + 
  ggtitle(t1, t2)