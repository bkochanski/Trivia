library(foreign)
Panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")
coplot(y ~ year|country, type="l", data=Panel) # Lines
coplot(y ~ year|country, type="b", data=Panel) # Points and lines
library(car)
scatterplot(y~year|country, smooth=TRUE, reg=FALSE, data=Panel)
library(gplots)
plotmeans(y ~ country, main="Heterogeineityacross countries", data=Panel)
plotmeans(y ~ year, main="Heterogeineityacross years", data=Panel)
detach("package:gplots")
ols<-lm(y ~ x1, data=Panel)
summary(ols)
yhat <-ols$fitted
plot(yhat, Panel$y)
plot(Panel$x1, Panel$y, pch=19, xlab="x1", ylab="y")
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")
fixed.dum <-lm(y ~ x1 + factor(country)-1, data=Panel)
summary(fixed.dum)

#anova(ols, fixed.dum)
yhat<-fixed.dum$fitted
scatterplot(yhat~Panel$x1|Panel$country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")

library(plm)
fixed <-plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
summary(fixed)
fixef(fixed)
pFtest(fixed, ols)

random <-plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")
summary(random)

Panel.set<-plm.data(Panel, index = c("country", "year"))
random.set<-plm(y ~ x1, data = Panel.set, model="random")
summary(random.set)
phtest(fixed, random)
