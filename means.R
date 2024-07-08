DescTools::Hmean(c(60,120))
mean(c(60,120))
weighted.mean(c(60,120), w=c(.6, .4))

1/weighted.mean(1/c(60,120), w=c(.6, .4))

w<-c(.6,.4)
x<-c(60,120)

sum(w*x)/sum(w)

1/(sum(w*(1/x))/sum(w))
1/sum(w*(1/x))

DescTools::Gmean(x)
1/DescTools::Gmean(1/x)



