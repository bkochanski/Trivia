lyr <- "Miało się jakoś tak ku wieczorowi
Kiedy posłańcy traktem przyjechali
Jeszczem nie całą ziemię swą obrobił
Ale posłańców oto wysłuchałem
Mówili Maria ma syna
Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Kiedy już zaczem niebo pociemniało
Całkiem posłańcy dalej pojechali
Raduje moje serce się i dusza
Na drogę nową od dzisiaj wyruszam
Mówili Maria ma syna
Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Kiedy dorośnie będzie tym
który odmieni nasze dni
Pomoże nam, pomoże wam
to czego nie możesz dostać sam
kiedy dorośnie będzie miał
u stopy swojej cały świat
który nadzieję ludziom da
panować będzie po wsze czas
kiedy dorośnie wtedy on
przyniesie wojnę pod Twój dom
i ojca synów, i bratu brat
stawi na przeciw pana świat
nie ma wiary bez niewoli
nie ma bólu, co nie boli
zostaw, postaw, tak pozostaw
to na pana w niebie rozkaz
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Jakem posłańców śladem pojechałem
Życie się moje odmieniło całkiem
Jakem się innym człowiekiem zostałem
Mówię każdemu, kto posłuchać pragnie
I mówię: Maria ma syna
Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Kiedy dorośnie będzie tym
który odmieni nasze dni
Położy nam, położy wam
to czego nie możesz dostać sam
kiedy dorośnie będzie miał
u stopy swojej cały świat
który nadzieję ludziom da
panować będzie po wsze czas
kiedy dorośnie wtedy on
przyniesie wojnę pod Twój dom
i ojca synów, i bratu brat
stawi na przeciw pana świat
nie ma wiary bez niewoli
nie ma bólu, co nie boli
zostaw, postaw, tak pozostaw
to na pana w niebie rozkaz
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna
Maria ma syna, Maria ma syna, Maria ma syna
Maria syna, Maria syna"

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
mainwords <- c("maria", "ma", "syna")
words2[!(words %in% mainwords)]<-"inne słowa"
words3<-factor(words2, level = c(mainwords, "inne słowa"))
levels(words3)<-c('Maria', 'ma', 'Syna', 'inne słowa')
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    main="'Maria ma Syna'\nsłowa składowe", 
    radius = 1, cex = .8, 
    labels=
      paste(levels(words3),
            '-', 
            paste0(format(round(table(words3)/sum(table(words3))*100,1), decimal.mark = ","),'%')))

