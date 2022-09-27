library(MASS)
?fitdistr
misie<-c(0, 4, 3, 9, 3, 6, 5, 12, 6, 1, 23, 3, 0, 4, 5, 3, 3, 6, 1, 0, 7, 1, 5)

a<-fitdistr(misie, "negative binomial")
a
1-dnbinom(0, size=a$estimate[1], mu=a$estimate[2])

table(misie)
length(misie)

table(misie)/length(misie)
dnbinom(0:7, size=a$estimate[1], mu=a$estimate[2])

misie2<-c(0, 4, 3, 9, 1, 3, 5, 9, 6, 1, 3, 0, 4, 5, 2, 3, 3, 1, 0, 7, 1, 5)
a2<-fitdistr(misie2, "negative binomial")

table(misie2)/length(misie2)
dnbinom(0:9, size=a2$estimate[1], mu=a2$estimate[2])
