library(readtext)
library(tidytext)
library(tidyr)

names<-c()
reswziac<-c()
reswziasc<-c()
naUkrainie<-c()
wUkrainie<-c()
naLitwie<-c()
wLitwie<-c()

urls<- c('https://wolnelektury.pl/media/book/txt/potop-tom-pierwszy.txt', 
         'https://wolnelektury.pl/media/book/txt/potop-tom-drugi.txt',
         'https://wolnelektury.pl/media/book/txt/potop-tom-trzeci.txt',
         'https://wolnelektury.pl/media/book/txt/ogniem-i-mieczem-tom-pierwszy.txt',
         'https://wolnelektury.pl/media/book/txt/ogniem-i-mieczem-tom-drugi.txt',
         'https://wolnelektury.pl/media/book/txt/pan-wolodyjowski.txt',
         'https://wolnelektury.pl/media/book/txt/nad-niemnem-tom-pierwszy.txt',
         'https://wolnelektury.pl/media/book/txt/nad-niemnem-tom-drugi.txt',
         'https://wolnelektury.pl/media/book/txt/nad-niemnem-tom-trzeci.txt',
         'https://wolnelektury.pl/media/book/txt/pan-tadeusz.txt')

for (i in 1:length(urls)) {
  print(i)
  print(urls[i])
  names[i]<-urls[i]
  tmp<-readtext(urls[i], encoding = "UTF-8")
  tmp<-unnest_tokens(tmp, word, text)
  reswziac[i]<-sum(tmp$word=='wziąć')
  reswziasc[i]<-sum(tmp$word=='wziąść')
  naUkrainie[i]<-sum(tmp$word[1:(length(tmp$word)-1)]=='na' & tmp$word[2:length(tmp$word)]=='ukrainie')
  wUkrainie[i]<-sum(tmp$word[1:(length(tmp$word)-1)]=='w' & tmp$word[2:length(tmp$word)]=='ukrainie')
  naLitwie[i]<-sum(tmp$word[1:(length(tmp$word)-1)]=='na' & tmp$word[2:length(tmp$word)]=='litwie')
  wLitwie[i]<-sum(tmp$word[1:(length(tmp$word)-1)]=='w' & tmp$word[2:length(tmp$word)]=='litwie')
}

View(data.frame(names, reswziac, reswziasc, naUkrainie, wUkrainie, naLitwie, wLitwie))

