lyr <- "Chłosta
Chłosta
Chłosta
Chłosta
W ogóle centralnie kamieniem go bez kitu
W ogóle centralnie kamieniem go bez kitu
W ogóle centralnie kamieniem go bez kitu
W ogóle centralnie kamieniem go bez kitu
W ogóle centralnie kamieniem go bez kitu
W ogóle centralnie kamieniem go bez kitu
CHŁOSTA... chłosta, chłosta
SIANIE... sianie, sianie
VIAGRA... viagra, viagra
JARANIE... jaranie, jaranie
Chłosta, chłosta
Sianie, sianie
Viagra, viagra
Jaranie, jaranie
Chłosta, chłosta
Sianie, sianie
Viagra, viagra
Jaranie, jaranie
Kamieniem go bez kitu
Kamieniem go bez kitu
Kamieniem go bez kitu
Kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu
W ogóle, centralnie, kamieniem go bez kitu (CHŁOSTA... chłosta, chłosta)
W ogóle, centralnie, kamieniem go bez kitu (SIANIE... sianie, sianie)
W ogóle, centralnie, kamieniem go bez kitu (VIAGRA... viagra, viagra)
W ogóle, centralnie, kamieniem go bez kitu (JARANIE... jaranie, jaranie)
Chłosta"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- unique(words)
words2<- words
words2[!(words %in% mainwords)]<-"inne słowa"
words_levels <-c(mainwords, "inne słowa")
words_levels <-c(mainwords)
words3<-factor(words2, level = words_levels)
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    #    main="Lyrical composition\nof Let It Be", 
    # main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles" 
    main="Chłosta - słowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


