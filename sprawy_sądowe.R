#cases<-c(161328,	118435,	265533,	263151,	309985,	141561,	68070,	23978,	11973,	3911,	2305)

# sądy okręgowe
cases<- c(21444,	12361,	17393,	11585,	17171,	13485,	7430,	2632,	1816,	357,	109)


# sądy okręgowe - sprawy cywilne
#cases<-c(4536,	3022,	5405,	4236,	8341,	7715,	3990,	1493,	705,	198,	57)

cases_time<-c("do 15 dni", "powyżej 15 dni do 1 mies.",
"powyżej 1 do 2 mies.",	
"powyżej 2 do 3 mies.",	
"powyżej 3 do 6 miesięcy",
"powyżej 6 do 12 miesięcy",
"powyżej 12 miesięcy do 2 lat",
"powyżej 2 do 3 lat",
"powyżej 3 do 5 lat",
"powyżej 5 do 8 lat",
"ponad 8 lat")

breaks<-c(0, 0.5, 1, 2, 3, 
          6, 12, 12*2, 12*3, 12*5, 12*8, 12*20)              

histogram_data<-rep(breaks[1:(length(breaks)-1)]+diff(breaks), cases)

hist(histogram_data, breaks = breaks, xlim=c(0,12*2))  

barplot(cases, names.arg=cases_time, las=2, cex.name=.7)
