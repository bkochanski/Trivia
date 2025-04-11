library(googlesheets4)
gs4_deauth()
dane <- read_sheet("https://docs.google.com/spreadsheets/d/1Bf6G3M66h1BpGoIyi1NiIdBw5n5uqDFox67wH7kZHqE/edit?gid=0#gid=0")
plot(dane$x, dane$y)
dane2<-unique(dane)
plot(dane2$x, dane2$y)
plot(dane2$x, cumsum(dane2$y))
f1 <- approxfun(dane2$x, dane2$y)
f2 <- approxfun(dane2$x, cumsum(dane2$y))
curve(f1,-6,6)
f1<-Vectorize(f1)

div1<-integrate(f1, -6,6, subdivisions = 1000L)$value
f3 <- approxfun(dane2$x, dane2$y)
f4<-function(x){f3(x)/div1}
curve(f4,-6,6, ylim=c(0,.6))
integrate(f4, -6,6, subdivisions = 1000L)

f5 <- function(x){f4(x*1.326861)*1.326861}
integrate(f5, -6/1.326861,6/1.326861, subdivisions = 1000L)
curve(f5, add=TRUE, col='green')
sqrt(integrate(function(x){f5(x)*x^2}, -6,6, subdivisions = 1000L)$value)
curve(dnorm, add=TRUE, col='blue')


curve(f5, -6,6, col='green')
curve(dnorm, add=TRUE, col='blue')

f3s <- approxfun(dane2$x, dane2$y, method = "constant")
f4s<-function(x){f3s(x)/div1}
f5s <- function(x){f4s(x*1.326861)*1.326861}
curve(f5s, -6,6, col='green')
curve(dnorm, add=TRUE, col='blue')

barplot()



sqrt(integrate(function(x){f5(x)*x^2}, -6/1.326861,6/1.326861, subdivisions = 1000L)$value)


integrate(function(x){f4(x)*x}, -6,6, subdivisions = 1000L)
integrate(function(x){f4(x)*x^2}, -6,6, subdivisions = 1000L)
sqrt(integrate(function(x){f4(x)*x^2}, -6,6, subdivisions = 1000L)$value)
integrate(function(x){f4(x)*x^4}, -6,6, subdivisions = 1000L)$value/
  (integrate(function(x){f4(x)*x^2}, -6,6, subdivisions = 1000L)$value)^2-3

sim1<-rt(15000, 4.5)
hist(sim1, breaks=100, probability = TRUE)
sd(sim1)
e1071::kurtosis(sim1)
curve(dnorm, col='blue', add=TRUE)


x0=sqrt(3)
1/12*(2*x0)^2

hist(runif(10000, -sqrt(3), sqrt(3)), breaks=40, xlim = c(-6,6), probability=TRUE)
curve(dnorm, add=TRUE, col='blue')

sim1<-rt(15000, 2)
hist(sim1, xlim = c(-6,6), breaks=2000)
sd(sim1)

#install.packages("extraDistr")
library(extraDistr)
set.seed(123)
lepto1<-rlaplace(40000)
lepto1<-(lepto1-mean(lepto1))/sd(lepto1)
hist(lepto1, breaks=100, probability=TRUE)
curve(dnorm, add=TRUE, col="blue")
