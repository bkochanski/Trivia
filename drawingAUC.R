#Midnormal function
FuncMidnormal<-function(x,g){pnorm(qnorm((g+1)/2)*sqrt(2)+qnorm(x))}

library(ggplot2)

base <-
  ggplot(data.frame(x=0:1000/1000), aes(x=x)) +
  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1))+
  coord_fixed()

 
library(emojifont)  
drawAUC<-function(AUC1, AUC2, printAUC=TRUE){
f1<-function(x){FuncMidnormal(x, 2*AUC1-1)}
f2<-function(x){FuncMidnormal(x, 2*AUC2-1)}
label1<-paste0('AUC: ', 
               format(AUC1, nsmall=3, decimal.mark=","), 
               ' \U2197 ', 
               format(AUC2, nsmall=3, decimal.mark=","))
label2<-paste0('(Gini: ', 
               format(2*AUC1-1, nsmall=3, decimal.mark=","), 
               ' \U2197 ', 
               format(2*AUC2-1, nsmall=3, decimal.mark=","),
               '; ', format(round((2*AUC2-2*AUC1)*100,1), nsmall=1, decimal.mark=","), 'pp)')
base + 
  geom_ribbon(aes(ymin = f1(x), ymax = f2(x)), fill = "grey") +
  geom_ribbon(aes(ymin = 0, ymax = f1(x)), fill = "grey98") +
  geom_function(fun = f1) +
  geom_function(fun=f2) +
  annotate("text", x=.5, y=.2, label=paste0('',if(printAUC){label1}), size=30/.pt) +
  annotate("text", x=.5, y=.1, label=paste0('',if(printAUC){label2}), size=24/.pt)
}

drawAUC(.68, .728, FALSE)
drawAUC(.591, .675)
drawAUC(.736, .763)
drawAUC(.603, .775)
drawAUC(.899, .923)

drawAUC(round(.738/2+.5,3), round((.738+.036)/2+.5,3))
drawAUC(round(.738/2+.5,3), round((.738+.041)/2+.5,3))
drawAUC(round(.738/2+.5,3), round((.738+.103)/2+.5,3))



#win.metafile("tmp.wmf", width = 5, height = 5)
f3<-function(x){FuncMidnormal(x, .6)}
base + 
  geom_ribbon(aes(ymin = 0, ymax = f3(x)), fill = "dark grey") +
  geom_function(fun=f3) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
#dev.off()

base + 
  geom_ribbon(aes(ymin = x, ymax = f3(x)), fill = "grey", alpha = .6) +
  geom_ribbon(aes(ymin = x, ymax = 1), fill = "grey", alpha = .2) +
  geom_function(fun=function(x){x}) +
  geom_function(fun=f3)

ggplot(data.frame(x=0:1000/1000), aes(x=x)) +
#  margin(2,2,2,2)+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1), 
        panel.background = element_blank(), 
        axis.title=element_text(size=14,face="bold"))+
  coord_fixed()+
  geom_ribbon(aes(ymin = 0, ymax = f3(x)), fill = "dark grey") +
  geom_function(fun=f3) +
  xlab("Skumulowana frakcja dobrych klientów") +
  ylab("Skumulowana frakcja złych klientów")+ 
  annotate("text", x=.6, y=.4, label="AUC", size=12) +
  geom_textpath(aes(y=f3(x)), vjust=-.5, text_only = TRUE,  label="Krzywa charakterystyki operacyjnej")


ggplot(data.frame(x=0:1000/1000), aes(x=x)) +
  #  margin(2,2,2,2)+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1), 
        panel.background = element_blank(), 
        axis.title=element_text(size=14,face="bold"))+
  coord_fixed()+
  geom_ribbon(aes(ymin = x, ymax = f3(x)), fill = "dark grey") +
  geom_ribbon(aes(ymin = f3(x), ymax = 1), fill = "grey") +
  geom_function(fun=f3) +
  xlab("Skumulowana frakcja dobrych klientów") +
  ylab("Skumulowana frakcja złych klientów")+ 
  annotate("text", x=.68, y=.4, label="Gini=A/(A+B)", size=10)+
  annotate("text", x=.4, y=.6, label="A", size=10)+
  annotate("text", x=.1, y=.8, label="B", size=10)

