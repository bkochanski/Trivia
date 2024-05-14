lyr <- 'Du du du du po ledu
dozadu du i dopředu du
Du du du du po ledu
dozadu du i dopředu du

Du du du du po ledu
dozadu du i dopředu du
Du du du du po ledu
dozadu du i dopředu du'

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("du", "po", "ledu", "dozadu", "i", "dopředu")
words2<- words
#words2[!(words %in% mainwords)]<-"other"
words3<-factor(words2, level = c(mainwords))
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
    main="Lyrical composition\nof Pochod Eskymaku", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))
