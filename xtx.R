o <- rep(1,12)
r <- c(1,1,1,1,2,2,2,2,3,3,3,3)
k1 <- c(1, 0,0,0,1,0,0,0,1,0,0,0)
k2 <- c(0, 1, 0,0,0,1,0,0,0,1,0,0)
k3 <- c(0, 0, 1, 0,0,0,1,0,0,0,1,0)
k4 <- c(0, 0, 0, 1, 0,0,0,1,0,0,0,1)

df <- data.frame(r, k1, k2, k3, k4)
y1 <- 2*r + k1*0.5 +k2*1 - k3*0.5 -k4*1 + rnorm(12)
y2 <- 3*r + k1*1 +k2*3 - k3*0.5 -k4*2 + rnorm(12)
y<-y1+y2
X <- as.matrix(df)
data <- df
data$y <-y
data1 <- df
data1$y1 <- y1
data2 <- df
data2$y2 <- y2

model <- lm(y~0+., data)
summary(model)
model1 <- lm(y1~0+., data1)
summary(model)
model2 <- lm(y2~0+., data2)
summary(model)

#predict(model)
model$fitted.values

model1$fitted.values+model2$fitted.values-model$fitted.values
y1+y2-y

library(MASS)
ginv(t(X)%*%X)%*%(t(X)%*%X)

X%*%ginv(t(X)%*%X)%*%t(X)%*%y
model$fitted.values
X%*%ginv(t(X)%*%X)%*%t(X)%*%y - model$fitted.values

ginv(t(X)%*%X)%*%t(X)%*%y
ginv(t(X)%*%X)%*%t(X)%*%matrix(y)
model$coefficients
ginv(t(X)%*%X)%*%t(X)%*%y - model$coefficients

ginv(t(X)%*%X)%*%t(X)%*%y1 - model1$coefficients

ginv(t(X)%*%X)%*%t(X)%*%y2 - model2$coefficients

ginv(t(X)%*%X)

(t(X)%*%X)


newdata<-data.frame(r = c(4,4,4,4), k1 = c(1,0,0,0), k2 = c(0,1,0,0), k3 = c(0,0,1,0), k4 = c(0,0,0,1))
predict(model, newdata = newdata)

sum(ginv(t(X)%*%X)%*%t(X)%*%y * c(4, 1, 0, 0, 0))
sum(ginv(t(X)%*%X)%*%t(X)%*%y * c(4, 0, 1, 0, 0))

sum(ginv(t(X)%*%X)%*%t(X)%*%y1 * c(4, 1, 0, 0, 0))
t((ginv(t(X)%*%X)%*%t(X)%*%y1))%*%c(4, 1, 0, 0, 0)

c(4, 1, 0, 0, 0)%*%ginv(t(X)%*%X)%*%t(X)%*%y1

sum(ginv(t(X)%*%X)%*%t(X)%*%y2 * c(4, 1, 0, 0, 0))

sum(ginv(t(X)%*%X)%*%t(X)%*%y1 * c(4, 1, 0, 0, 0)) + sum(ginv(t(X)%*%X)%*%t(X)%*%y2 * c(4, 1, 0, 0, 0))




t(X)%*%X

