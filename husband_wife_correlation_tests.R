#Husband wife correlation

extract<-osoby[,c('numer_gd', 'numer_osoby', 'hc5', 'hc6', 'hc10', 'hp65', 'hp52', 'hp53', 'waga_2015_ind', 'wiek2015')]
extract$hp65[is.na(extract$hp65)]<-(-1)
extract<-na.omit(extract)
names(extract)<-c('household_id', 'person_id', 'family_no', 'family_role', 'sex', 'income', 'height', 'weight', 'survey_weight', 'age')
extract$income[extract$income==-1]<-NA
extract$family_id<-paste(extract$household_id, extract$family_no)
extract_husband<-extract[extract$family_role==1&extract$sex==1, c('family_id', 'income', 'height', 'weight', 'survey_weight', 'age')]
names(extract_husband)<-c('family_id', 'husband_income', 'husband_height', 'husband_weight', 'husband_survey_weight', 'husband_age')
extract_wife<-extract[extract$family_role==2&extract$sex==2, c('family_id', 'income', 'height', 'weight', 'survey_weight', 'age')]
names(extract_wife)<-c('family_id', 'wife_income', 'wife_height', 'wife_weight', 'wife_survey_weight', 'wife_age')
e<-merge(extract_husband, extract_wife)
e$avg_survey_weight<-(e$husband_survey_weight+e$wife_survey_weight)/2
#View(e)

#plot(log(e$husband_income), log(e$wife_income))
plot(jitter(e$husband_income,3), jitter(e$wife_income,3), col=rgb(0,0,0,0.2), pch=16, log="xy", xlab="dochód męża", ylab="dochód żony")
abline(a=0, b=1)


cor(e$husband_income, e$wife_income, use="pairwise.complete.obs")

cor(log(e$husband_income), log(e$wife_income), use="pairwise.complete.obs")

e2<-na.omit(e[,c('husband_income', 'wife_income', 'avg_survey_weight')])
cov.wt(data.frame(e2$husband_income, e2$wife_income), wt=e2$avg_survey_weight, cor=TRUE)$cor[2,1]
cov.wt(data.frame(log(e2$husband_income), log(e2$wife_income)), wt=e2$avg_survey_weight, cor=TRUE)$cor[2,1]


#weights::wtd.cors(e$husband_income, e$wife_income, weight=e$avg_survey_weight)
plot(jitter(e$husband_height,5), jitter(e$wife_height,5), col=rgb(0,0,0,0.2), pch=16, xlab="wzrost męża", ylab="wzrost żony")
cor(e$husband_height, e$wife_height)
cov.wt(data.frame(e$husband_height, e$wife_height), wt=e$avg_survey_weight, cor=TRUE)$cor[2,1]

abline(a=0, b=1)
plot(e$husband_weight, e$wife_weight)
cor(e$husband_weight, e$wife_weight)
cov.wt(data.frame(e$husband_weight, e$wife_weight), wt=e$avg_survey_weight, cor=TRUE)$cor[2,1]

cor(e$husband_age, e$wife_age)
plot(e$husband_age, e$wife_age)
abline(a=0, b=1)
summary(lm(e$wife_age~e$husband_age))
model1<-lm(e$wife_age~e$husband_age)
model1$coefficients[1]+model1$coefficients[2]*60

cor(e$husband_height, e$wife_height)
cor(e$husband_weight, e$wife_weight)
cor(e$husband_weight, e$husband_height)
cor(e$wife_weight, e$wife_height)

cor(e$husband_weight, e$wife_weight)
cor(residuals(lm(e$husband_weight~e$husband_height)),residuals(lm(e$wife_weight~e$wife_height)))
cor(residuals(lm(e$husband_weight~e$husband_height)),residuals(lm(e$wife_weight~e$husband_height)))

summary(lm(e$husband_weight~e$husband_height+e$wife_weight+e$husband_age))

e$wife_BMI<-e$wife_weight/(e$wife_height^2/10000)
e$husband_BMI<-e$husband_weight/(e$husband_height^2/10000)
cor(e$wife_BMI, e$husband_BMI)

summary(lm(e$husband_weight~e$husband_height+e$wife_BMI))


cor(e$husband_weight, e$husband_height)
cor(e$husband_weight, e$husband_height^2)
cor(e$wife_weight, e$wife_height)
cor(e$wife_weight, e$wife_height^2)

plot(e$husband_weight~e$husband_age)
summary(lm(e$husband_weight~e$husband_age))


mean(e2$wife_income>e2$husband_income)
mean((e2$wife_income>e2$husband_income)*e2$avg_survey_weight)
mean(e$wife_age>e$husband_age)
mean((e$wife_age>e$husband_age)*e$avg_survey_weight)
mean(e$wife_height>e$husband_height)
mean((e$wife_height>e$husband_height)*e$avg_survey_weight)
mean(e$wife_weight>e$husband_weight)
mean((e$wife_weight>e$husband_weight)*e$avg_survey_weight)

