lyr <- "When I find myself in times of trouble, Mother Mary comes to me
Speaking words of wisdom, let it be
And in my hour of darkness she is standing right in front of me
Speaking words of wisdom, let it be
Let it be, let it be, let it be, let it be
Whisper words of wisdom, let it be
And when the broken hearted people living in the world agree
There will be an answer, let it be
For though they may be parted, there is still a chance that they will see
There will be an answer, let it be
Let it be, let it be, let it be, let it be
There will be an answer, let it be
Let it be, let it be, let it be, let it be
Whisper words of wisdom, let it be
Let it be, let it be, let it be, let it be
Whisper words of wisdom, let it be, be
And when the night is cloudy there is still a light that shines on me
Shinin' until tomorrow, let it be
I wake up to the sound of music, Mother Mary comes to me
Speaking words of wisdom, let it be
And let it be, let it be, let it be, let it be
Whisper words of wisdom, let it be
And let it be, let it be, let it be, let it be
Whisper words of wisdom, let it be"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]]+")))
words
pie(table(words))

mainwords <- c("let", "it", "be", "words", "of", "wisdom")
words2<- words
words2[!(words %in% mainwords)]<-"other"
words3<-factor(words2, level = c(mainwords, "other"))
pie(table(words3), 
    clockwise=TRUE, 
    col= viridis::viridis(length(mainwords)+1), 
#    main="Lyrical composition\nof Let It Be", 
    main="Występowanie poszczególnych słów\n w Let It Be zespołu The Beatles", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)
        

