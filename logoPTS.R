x<-c(0.1758241758241758, 0.31648351648351647, 0.4043956043956044, 0.49230769230769234, 0.5714285714285714, 0.6593406593406593, 0.7384615384615385, 0.8263736263736263, 0.8967032967032967, 0.967032967032967, 1.0725274725274725, 1.1692307692307693, 1.2747252747252746, 1.3802197802197802, 1.4593406593406593, 1.5296703296703296, 1.6, 1.6967032967032967, 1.8197802197802198, 1.978021978021978, 2.241758241758242, 3.1032967032967034, 2.830769230769231, 2.5142857142857142, 3.3582417582417583)
y<-c(0.9899799599198396, 0.9659318637274549, 0.937875751503006, 0.8977955911823646, 0.8577154308617234, 0.8216432865731462, 0.7795591182364728, 0.7414829659318637, 0.7074148296593186, 0.6653306613226453, 0.6192384769539078, 0.565130260521042, 0.5110220440881763, 0.46492985971943884, 0.42685370741482964, 0.39078156312625245, 0.35270541082164325, 0.3046092184368737, 0.24849699398797592, 0.18637274549098196, 0.13226452905811623, 0.04008016032064128, 0.058116232464929855, 0.09418837675350701, 0.03006012024048096)
max(x)
plot(x,y)

x2<-c(rev(-x), 0, x)
y2<-c(rev(y), 1, y)
plot(x2, y2)

f<-approxfun(x2, y2)
curve(f, -max(x),max(x))

i1<-integrate(f, -max(x), max(x))$value

f2<-function(x){f(x)/i1}

curve(f2, -max(x), max(x))
integrate(f2, -max(x), max(x))

m1<-max(x)
E<-function(f){integrate(function(x){f(x)*x}, -m1, m1)$value}
E(f2)

E(f)

V<-function(f){integrate(function(x){(x-E(f))^2*f(x)}, -m1, m1)$value}
V(f2)
SD<-function(f){sqrt(V(f))}
SD(f2)

fg<-function(x){dnorm(x, 0, SD(f2))}
curve(fg, col='blue', add=TRUE, lty = 4)

Kurt<-function(f){integrate(function(x){(x-E(f))^4*f(x)}, -m1, m1)$value/(V(f))^4}
Kurt(f2)
Kurt(fg)

ft1<-function(x){dt(x, 20)}


curve(ft1, add=TRUE, col='red')
ftp<-function(x, nu, poziom){1/(dt(0/poziom,nu)/poziom)*dt(x/poziom,nu)/poziom}
curve(function(x){ftp(x, 4, 1)})

plot(ftp(-300:300/100, 4, 1))

ftp(0,3,4)-f(0)

funk <- function(param){integrate(function(x){(f(x)-ftp(x, param[1], param[2]))^2}, -m1, m1)$value}

funk(c(2,3))

optim(par=c(1,1), fn=funk) 
funk(c(1,1))

curve(f, -m1, m1)
fwyn<-function(x){ftp(x, 15, 1.09)}
curve(fwyn, add='TRUE', col='red', lty=4)
curve(f, add=TRUE)

#ftp(1, 15.3, 1.09)
