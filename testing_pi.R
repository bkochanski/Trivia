mypi = readLines(url('http://www.cs.princeton.edu/introcs/data/pi-10million.txt'), warn=FALSE)
piby1 = as.numeric(unlist(strsplit(mypi,"")))

chisq.test(table(piby1[20001:30000]), p=rep(0.1, 10))
chisq.test(table(piby1[(10000*2+1):(10000*(2+1))]), p=rep(0.1, 10))
i=2
chisq.test(table(piby1[(10000*i+1):(10000*(i+1))]), p=rep(0.1, 10))$p.value

chtest<-Vectorize(function(i){chisq.test(table(piby1[(10000*i+1):(10000*(i+1))]), p=rep(0.1, 10))$p.value})

i=1:1000-1
pvs<-chtest(i)
hist(pvs, breaks=20)
mean(pvs<.5)

chtest10e6<-Vectorize(function(i){chisq.test(table(piby1[(1e6*i+1):(1e6*(i+1))]), p=rep(0.1, 10))$p.value})
pvs<-chtest10e6(1:10-1)
pvs
l<-length(piby1)

chtest<-Vectorize(function(i, x){chisq.test(table(piby1[(x*i+1):(x*(i+1))]), p=rep(0.1, 10))$p.value})
pvs<-chtest(1:floor(l/5e5)-1, 5e5)
min(pvs)

chisq.test(table(piby1[8500001:9000000]), p=rep(0.1, 10))

test<-chisq.test(table(piby1[1:607]))
test
test$observed

x<-c(60, 62, 67, 68, 64, 56, 62, 44, 58, 67)
sum(x)
chisq.test(x=x)

?prop.test
prop.test(44, 608, p = .1, alternative = "less", correct = FALSE)
binom.test(44, 608, p = .1, alternative = "less")


m1 =762
m2 =193034
# F i r s t F e y n m a n P o i n t .
p1 =10*(1 - pbinom (0, size =m1 -6, prob =1/10^6) )
p1
# S e c o n d F e y n m a n P o i n t .
p2 =10*(1 - pbinom (1, size =m2 -6, prob =1/10^6) )
p2

#three consecutive nines in 5 digits

(1 - pbinom(0, size=5-3, prob=1/10^3))
272/100000

#two consecutive nines in 3 digits 
#19/1000
#x99, 99x, 999
1-pbinom(0, size=2, prob=1/100)
19/1000
(1-.9^2)/10^1

#three consecutive nines in 5 digits
#99999, 9999x, x9999, xd999, d999d, 999dx - 1+9+9+90+81+90
t<-expand.grid(0:9, 0:9, 0:9, 0:9, 0:9)
t<-apply(t, 1, paste, collapse="")
sum(grepl( "999", t, fixed = TRUE))/1e5
1-pbinom(0, size=3, prob=1/1000)
262/1e5
(1-.9^(5-3+1))/10^(3-1)

#three consecutive nines in 6 digits
#999999, 99999n, n99999, 9999nx, n9999n, xn9999, 999nxx, n999nx, xn999n, xxn999 
# 1+9+9+90+81+90+900+810+810+900
t<-expand.grid(0:9, 0:9, 0:9, 0:9, 0:9, 0:9)
t<-apply(t, 1, paste, collapse="")
sum(grepl( "999", t, fixed = TRUE))



(1-.9^757)/10^5
1e-5*100

