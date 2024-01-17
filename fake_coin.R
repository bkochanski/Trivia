library(googlesheets4)
gs4_deauth()
a<-googlesheets4::read_sheet('1hpWWvPruSMitx1wQwP5Uk7sQsGga3G6tjsKTs_Z06gI')
a<-a[1:100,]
tmp<-as.vector(a[1]=='R')
max(rle(tmp)$lengths)
length(rle(tmp)$lengths)
sum(rle(tmp)$lengths>=4)

ileR <- sapply(a, function(x){sum(as.vector(x=='R'))})
maxS <- sapply(a, function(x){max(rle(as.vector(x=='R'))$lengths)})
ileS <- sapply(a, function(x){length(rle(as.vector(x=='R'))$lengths)})
ileSg4 <- sapply(a, function(x){sum(rle(as.vector(x=='R'))$lengths>=4)})

plot(ileS, jitter(maxS), pch=16, col=rgb(0,0,0,.7), ylim=c(1,10), xlim=c(20,80))

real_matrix<-replicate(5000, rbinom(100, 1, .5))
#View(real_matrix)
real_matrix<-as.data.frame(real_matrix)

ileR_2 <- sapply(real_matrix, function(x){sum(as.vector(x==1))})
maxS_2 <- sapply(real_matrix, function(x){max(rle(as.vector(x==1))$lengths)})
ileS_2 <- sapply(real_matrix, function(x){length(rle(as.vector(x==1))$lengths)})
ileSg4_2 <- sapply(real_matrix, function(x){sum(rle(as.vector(x==1))$lengths>=4)})

plot(ileS_2, jitter(maxS_2), col=rgb(1,0,0,.05), pch=16)
points(ileS, jitter(maxS), pch=1, col=rgb(0,0,0,.7))

plot(ileS_2, jitter(ileSg4_2), col=rgb(0,0,0,.05), pch=16)
points(ileS, jitter(ileSg4), pch=16, col=rgb(1,0,0,.7))

plot(ileS_2, jitter(maxS_2), col=rgb(0,0,0,.05), pch=16)
points(ileS, jitter(maxS), pch=16, col=rgb(1,0,0,.7))

