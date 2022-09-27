install.packages("msos")
library(msos)
data(Spam)
View(Spam)

george<-unname(Spam[,which(colnames(Spam)=="WFgeorge")])
hist(george)
hist(log(george+0.1))

Spam2<-Spam
for (i in 1:57) {Spam2[,i]<-log(Spam2[,i]+.1)}
Spam3<-data.frame(Spam2, )
m1<-glm(formula=spam~., family="binomial", data=Spam3)  

selected<-names(Spam3) %in% c("WFour", "WFover", "WFremove", 
                    "WFinternet", "WFfree", "WFbusiness", "WFhpl", 
                    "CFexclam", "CFdollar", "CRLlongest", "CRLtotal", 
                    "WFhp", "WFgeorge", "WF1999", "WFre", "WFedu", "spam")

Spam4<-data.frame(Spam2 [,selected])
m2<-glm(formula=spam~., family="binomial", data=Spam4)  
summary(m2)
bigstatsr::AUC(predict(m2), Spam4$spam)

m2_withoutgeorge<-glm(formula=spam~., family="binomial", data=Spam4[,names(Spam4)!="WFgeorge"])  
summary(m2_withoutgeorge)
bigstatsr::AUC(predict(m2_withoutgeorge), Spam4$spam)

0.02164643/0.02622361

