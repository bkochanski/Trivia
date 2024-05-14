lyr <- "Dzwoni do mnie kuzyn
to jest Kaz bałagane
Mówi dawaj klip
to odwiedzimy Warszawę
odwiedzam studio, tylko przez to daje rade
a jeśli mnie tam nie ma
to traktuje tak jak karę

jebać suki tam, bo mi przeszkadzają
to mój cały świat
no i z ziomem solidarność
ulubiony numer
dawno zdarłem na nim gardło
ucieczka do nieba
płać odpowiedzialność karną

twoja dupa to nie dupa – to jest sadło
pisze do mnie kuzyn
nie wie co ma robić z matką
wyrzuciła z domu, teraz jest pod wycieraczką
ale nie prosi o żarcie
bo obraca prawko

ale dzwoni o poradę też
co ma robić kiedy sam tu pali jazz
porobimy w tylu ten cash
zależy od pogody też
ale raczej klipy, studio to wiesz jak jest

Dzwoni kuzyn to mój kuzyn
Dzwoni kuzyn to mój kuzyn
Dzwoni kuzyn to mój kuzyn
Dzwoni kuzyn to mój kuzyn

to mój kuzyn dzwoni i pyta o rade
czy znam dupy z zajebistym zadem
bo teraz ogląda klipy
no i w sumie złapał zajawę
tylko co powiedzieć dupie
nic czy coś?
ciężko powiedzieć mordo - rób jak uważasz

Dzwoni kuzyn to mój kuzyn
Dzwoni kuzyn to mój kuzyn
Dzwoni kuzyn to mój kuzyn
Dzwoni kuzyn to mój kuzyn"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))
#words[substring(words, 1, 5)=="wypij"] <- 'wypij*'

df<-data.frame(table(words))
df$words <- as.character(df$words)
#View(df)
df$words[order(-df$Freq)][df$Freq[order(-df$Freq)]>3]
#df[order(-df$Freq),]

mainwords <- c("dzwoni", "kuzyn", "to", "mój")
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
    main='Kaz Bałagane: Kuzyn\nsłowa składowe', 
    labels=paste(levels(words3),'-', paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ','),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


