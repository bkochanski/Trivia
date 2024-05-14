lyr <- "You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch
You're out of touch
You're out of touch
You're out of touch
You're out of touch, so, baby, why can't you see?
When we're together, there's a feeling inside of me
You're like a star shining all through the night
When I'm with you, everything feels so right
I wanna make you mine, but you're out of time
Ooh, baby, you know I love you so much
But you're out of touch, ooh, baby
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
Well, nothing in life comes easy
I'm searching for love, and that I might not find
So, girl, won't you try to please me?
'Cause I can't get you out of my mind
I wanna make you mine, but you're out of time
Ooh, baby, you know I love you so much
But you're out of touch
Oh-oh-oh, oh-oh-oh
Out of touch, out of touch
Out of touch, out of touch
Out of touch, out of touch
Out of touch, out of touch
Out of touch, out of touch
Out of touch, out of touch
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around
You're out of touch, I'm out of time
But I'm out of my head when you're not around"

words <- tolower(unlist(stringr::str_split(lyr, "[[:space:][:punct:]&&[^']]+")))
words
pie(table(words))
View(data.frame(table(words)))

mainwords <- c("out", "of", "touch", "time", "you're", "i'm", "but", "when", "my", "head", "not", "around")
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
    main="Uniting Nations - 'Out of Touch'\nsłowa składowe", 
    labels=paste(levels(words3),'-', paste0(round(table(words3)/sum(table(words3))*100,1),'%')))


# library(ggplot2)
# ggplot(data = as.data.frame(table(words3)), aes(x="", y=Freq, fill=words3)) + 
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)


