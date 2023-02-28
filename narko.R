n=400

d<-expand.grid(0:n, 0:n, 0:n)
d<-d[rowSums(d)==n,]

x<-d$Var1
y<-d$Var2
z<-d$Var3
prob<-z

for (i in 1:length(x)){prob[i]<-dmultinom(x=c(x[i],y[i],z[i]), prob=c(4/11,3/11,4/11))}

plot(prob)

m=0.4*n

#View(data.frame(x,y,z,prob)[y<=m&x+y>=m,])

p=0.3
l1<-ifelse(y>m|x+y<m,0,dbinom(m-y,x,p))
sum(l1*prob)

like<-Vectorize(function(p){sum(ifelse(y>m|x+y<m,0,dbinom(m-y,x,p))*prob)})
p<-0:1000/1000
likelihood<-like(p)
prior<-rep(1/length(p),length(p))
posterior<-prior*likelihood
posterior<-posterior/sum(posterior)
plot(p,posterior)
p[posterior==max(posterior)]
p[max(which(cumsum(posterior)<.025))]
p[min(which(cumsum(posterior)>1-.025))]
sum(posterior)

#4/11*.35+3/11


140*.35
