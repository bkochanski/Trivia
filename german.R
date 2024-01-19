german<-read.csv('german_credit.csv')
table(german$number_credits)
german$number_credits[german$number_credits=='3-Feb']<-'2 to 3'
german$number_credits[german$number_credits=='5-Apr']<-'4 to 5'
write.csv2(german, 'german_credit2.csv')

table(german$credit_risk)

german2<-german
german2$good_flag <- (german$credit_risk=='good')*1

linear<-lm(good_flag~age, data=german2)



library(ggplot2)
ggplot(german2, 
       aes(x = age, y = good_flag)) + geom_jitter(height=.01) + geom_abline(slope=linear$coefficients[2], intercept=linear$coefficients[1])


ggplot(german2, 
       aes(x = age, y = good_flag)) + 
  geom_jitter(height=.01) + 
  geom_abline(slope=linear$coefficients[2], intercept=linear$coefficients[1]) +
  geom_smooth(method="glm", se=FALSE, method.args=list(family=binomial))




  