lyr <- "Dziękuję ci mamo za wszystko co dla mnie zrobiłaś
Z tobą przy tobie odnajdę swój sens
Nigdy nie upadne bo z całego serca kocham cię
Odpłacam się miłością bez żadnych granic

ŻYCIE ŻYCIE
ŻYCIE ŻYCIE
ŻYCIE ŻYCIE
ŻYCIE ŻYCIE
Wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie

Odstaw ten syf co wciskają nam
Tylko ty i ja muzyczna podróż nowy świat
Bez dwóch zdań bez żadnych granic
Wolności słowach największą przeszkoda
W drodze z marzeniami odsłoniętymi kulisami
Bo chce tu być nie uciekaj ode mnie
Pomożesz ty mi ją pomogę tobie więcej
SZAMAN MIŁOŚCIĄ CZĘSTUJE NA ZACHĘTĘ
Linie nie papilarne uciekaj ze mną stąd
Zchipują twój umysł twe życie będzie marne
Inie wenuo masoni jak was są
SZAMAN Z MICHAŁEM NIE BOJĄ SIĘ WAS
Podążać w nowy świat jest ciężko bro
Chcesz z nami walczyć odpadasz lub kozaczysz
W Polsce wielka siła która nigdy nie zginęła
W Polsce wielka siła która nigdy nie zginęła

ŻYCIE ŻYCIE
ŻYCIE ŻYCIE
ŻYCIE ŻYCIE
ŻYCIE ŻYCIE
Wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie, wciąż zaskakuje mnie

Jesteś ze mną tu w swej krainie snów
Pełnej marzeniami nie jesteśmy tu sami
To prezent od naszej matki Gai
Ziemią ja nazwali odwróć do góry nogami
Energia cię rozpali niekończąca muzyczna
Rzeka tam czeka wskoczymy w nią razem
Ich prąd drogowskazem spojrzeć na ląd
Twoimi oczami zataczającymi drzewami
Wyśmi jak na fali wspólnymi znakami
Od serca taktami
Oddaje swój głos
Zawsze razem z wami
Czy czujesz teraz mnie?
Rozplątuje swe serce utkane różami
Czy czujesz teraz mnie?
Rozplątuje swe serce utkane różami, utkane różami
Czy czujesz teraz mnie?
Rozplątuje swe serce
Od serca taktami
Od serca taktami

ŻYCIE ŻYCIE
Wciąż zaskakuje mnie, wciąż zaskakuje mnie
Od serca taktami
Od serca taktami
ŻYCIE ŻYCIE
Wciąż zaskakuje mnie, wciąż zaskakuje mnie
Od serca taktami
Od serca taktami
ŻYCIE ŻYCIE
Wciąż zaskakuje mnie

Zwrotka 4
Otwieram swe serce utkane różami
Ciernymi kolcami nie dotknie nie zrani
Swoją miłością może nas ocalić
Oddaje ci ją niech prowadzi mnie los"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]

words2<- words
mainwords <- c("życie", "wciąż", "zaskakuje", "mnie")
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
# words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
t1 <- 'Michał Archanioł: Szaman ty i ja'
t2 <- 'słowa składowe'
pie(table(words3), 
    clockwise=TRUE, 
    col=viridis::viridis(length(words_levels)), 
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

