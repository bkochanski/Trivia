library(ggplot2)
library(ggrepel)

size<-c(20, 22, 40, 55, 55, 70, 71, 80)
mass<-c(2.7, 3, 13, 28, 31, 50, 70, 90)
labels<-c("Chihuahua", "Yorkshire", "Terier", "Bearded collie", 
          "Chow Chow", "Akita", "Nowofundland", "Mastif")
df<-data.frame(size, mass, labels)

model<-lm(log(mass)~log(size))
summary(model)
prediction<-exp(predict.lm(model, newdata=data.frame(size=c(60))))
df2<-data.frame(size=60, mass=prediction, labels="Nasz Pies")

yticks<-c(2,3,4,5,6,7,8,9,10,15,20,25,(3:10)*10)
xticks<-c(20, 25, (3:10)*10)

ggplot(df, aes(x=size, y=mass)) + 
  geom_point(size=4) +
  xlab("Wielkość [cm]") +
  ylab("Masa [kg]") +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", 
              se=FALSE, 
              colour='red') +
  scale_y_continuous(trans='log10', breaks=yticks) +
  scale_x_continuous(trans='log10', breaks=xticks) +
  theme_bw() +
  geom_point(data=df2, 
             aes(x=size, y=mass), 
             colour='blue',
             size=4) + 
  geom_text_repel(data=df, aes(x=size, y=mass, label=labels))+
  geom_segment(aes(x=60, y=0, xend=60, yend=prediction), lty=2, col='blue')+
  geom_segment(aes(x=0, y=prediction, xend=60, yend=prediction), lty=2, col='blue') +
  annotate("text", x=min(xticks)-1, y=prediction+5, label=round(prediction,2), col='blue')



ggplot(df, aes(x=size, y=mass)) + 
  geom_point(size=4) +
  xlab("Wielkość [cm]") +
  ylab("Masa [kg]") +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", 
              se=FALSE, 
              colour='red',
              size=.8) +
  scale_y_continuous(trans='log10', breaks=yticks) +
  scale_x_continuous(trans='log10', breaks=xticks) +
  theme_bw() +
  geom_point(data=df2, 
             aes(x=size, y=mass), 
             colour='blue',
             size=4) + 
  geom_text_repel(data=df, aes(x=size, y=mass, label=labels))+
  geom_segment(aes(x=60, y=0, xend=60, yend=prediction), lty=2, col='blue')+
  geom_segment(aes(x=0, y=prediction, xend=60, yend=prediction), lty=2, col='blue') +
  annotate("text", x=min(xticks)-1, y=prediction+5, label=round(prediction,2), col='blue')




ggplot(df, aes(x=size, y=mass)) + 
  geom_point(size=4) +
  xlab("Wielkość [cm]") +
  ylab("Masa [kg]") +
  # geom_function(fun=function(x){exp(model$coefficients[1]+model$coefficients[2]*log(x))},
  #               colour='red') +
  geom_function(fun=function(x){exp(model$coefficients[1])*x^model$coefficients[2]},
                colour='red') +
  scale_x_continuous(limits=c(0,90), breaks=sort(c(0:3*25, 60)), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0,100), breaks=sort(c(0:4*25, as.numeric(round(prediction,2)))), expand = c(0, 0)) +
  theme_bw() +
  geom_point(data=df2, 
             aes(x=size, y=mass), 
             colour='blue',
             size=4) + 
  geom_text_repel(data=df, aes(x=size, y=mass, label=labels))+
  geom_segment(aes(x=60, y=0, xend=60, yend=prediction), lty=2, col='blue')+
  geom_segment(aes(x=0, y=prediction, xend=60, yend=prediction), lty=2, col='blue')

