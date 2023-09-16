f<-function(x){dt(x,5)}
curve(f,-10,10)
curve(f,-5,5, add=TRUE, col="red")

integrate(f, -60,60)
integrate(function(x){f(x)*x}, -60,60)
integrate(function(x){f(x)*x^2}, -60,60)
v1<-5/(5-2)

g<-function(x){sqrt(v1)*dt(x*sqrt(v1), 5)}
integrate(g, -60,60)
integrate(function(x){g(x)*x}, -60,60)
integrate(function(x){g(x)*x^2}, -60,60)

g(-10)*100000
f(-10)*100000

?dt
?dnorm
pnorm(2)
pt(1,5)

pt(-1.5, 5)
pt(-1.5, 4.7)
pt(-1.5, 4)
7*pt(-1.5, 5)/10+3*pt(-1.5, 4)/10

tempf<-Vectorize(function(x){pt(x, 4.7)})
myseq<-seq(-10, 10, 0.1)
#View(data.frame(tempf(myseq)))
tempf(-7)
pt(-7,5)
pt(-7,4)
pt(-7,4.7)
