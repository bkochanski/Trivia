#load(url("https://github.com/pbiecek/Diagnoza/raw/master/data/osoby.rda"))

extract<-osoby[,c('numer_gd', 'numer_osoby', 'hc5', 'hc6', 'hc10', 'hp65', 'hp52', 'hp53', 'waga_2015_ind', 'wiek2015')]

extract$hp65[is.na(extract$hp65)]<-(-1) #-1 instead of NA in case of personal income

extract<-na.omit(extract) #delete rows where there are NAs in the extracted columns

names(extract)<-c('household_id', 'person_id', 'family_no', 'family_role', 'sex', 'income', 'height', 'weight', 'survey_weight', 'age') 

extract$income[extract$income==-1]<-NA #back to NA for income

extract$family_id<-paste(extract$household_id, extract$family_no)

extract_husband<-extract[extract$family_role==1&extract$sex==1, c('family_id', 'income', 'height', 'weight', 'survey_weight', 'age')]
names(extract_husband)<-c('family_id', 'husband_income', 'husband_height', 'husband_weight', 'husband_survey_weight', 'husband_age')

extract_wife<-extract[extract$family_role==2&extract$sex==2, c('family_id', 'income', 'height', 'weight', 'survey_weight', 'age')]
names(extract_wife)<-c('family_id', 'wife_income', 'wife_height', 'wife_weight', 'wife_survey_weight', 'wife_age')

e<-merge(extract_husband, extract_wife)

# survey weights from Diagnoza - average for each couple
e$avg_survey_weight<-(e$husband_survey_weight+e$wife_survey_weight)/2
e$avg_survey_weight<-e$avg_survey_weight/mean(e$avg_survey_weight)

e2<-na.omit(e[,c('husband_income', 'wife_income', 'avg_survey_weight')]) #e2 - extract with rows where both spouses have income

old.par<-par(mar=rep(4,4), mfrow=c(2,2))

#### INCOME

# correlation without survey weights
cor(e2$husband_income, e2$wife_income)

# correlation with survey weights
cor_income<-cov.wt(data.frame(e2$husband_income, e2$wife_income), wt=e2$avg_survey_weight, cor=TRUE)$cor[2,1]

plot(jitter(e2$husband_income,3), jitter(e2$wife_income,3), col=rgb(0,0,0,0.2), pch=16, log="xy", xlab="husband's monthly income (PLN)", ylab="wife's monthly income (PLN)", main=paste("INCOME. Correlation =", round(cor_income,3)))


#### HEIGHT

# correlation without survey weights
cor(e$husband_height, e$wife_height)

# correlation with survey weights
cor_height<-cov.wt(data.frame(e$husband_height, e$wife_height), wt=e$avg_survey_weight, cor=TRUE)$cor[2,1]

plot(jitter(e$husband_height,3), jitter(e$wife_height,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's height", ylab="wife's height", main=paste("HEIGHT. Correlation =", round(cor_height,3)))

#### WEIGHT

# correlation without survey weights
cor(e$husband_weight, e$wife_weight)

# correlation with survey weights
cor_weight<-cov.wt(data.frame(e$husband_weight, e$wife_weight), wt=e$avg_survey_weight, cor=TRUE)$cor[2,1]

plot(jitter(e$husband_weight,3), jitter(e$wife_weight,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's weight", ylab="wife's weight", main=paste("WEIGHT. Correlation =", round(cor_weight,3)))


#### AGE

# correlation without survey weights
cor(e$husband_age, e$wife_age)

# correlation with survey weights
cor_age<-cov.wt(data.frame(e$husband_age, e$wife_age), wt=e$avg_survey_weight, cor=TRUE)$cor[2,1]

plot(jitter(e$husband_age,3), jitter(e$wife_age,3), col=rgb(0,0,0,0.2), pch=16, xlab="husban's age", ylab="wife's age", main=paste("AGE. Correlation =", round(cor_age,3)))

par(old.par)




#THE SAME SCALE ON X AND Y

old.par<-par(mar=rep(4,4), mfrow=c(2,2))

#### INCOME

plot(jitter(e2$husband_income,3), jitter(e2$wife_income,3), col=rgb(0,0,0,0.2), pch=16, log="xy", xlab="husband's monthly income (PLN)", ylab="wife's monthly income (PLN)", xlim=c(150,30000), ylim=c(150,30000), main=paste("INCOME. Correlation =", round(cor_income,3)))
abline(0,1,lty=3, col="grey")


#### HEIGHT

plot(jitter(e$husband_height,3), jitter(e$wife_height,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's height (cm)", ylab="wife's height (cm)", main=paste("HEIGHT. Correlation =", round(cor_height,3)), xlim=c(140,205), ylim=c(140,205))
abline(0,1,lty=3, col="grey")

#### WEIGHT

plot(jitter(e$husband_weight,3), jitter(e$wife_weight,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's weight (kg)", ylab="wife's weight (kg)", main=paste("WEIGHT. Correlation =", round(cor_weight,3)), xlim=c(35,165), ylim=c(35,165))
abline(0,1,lty=3, col="grey")


#### AGE

plot(jitter(e$husband_age,3), jitter(e$wife_age,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's age (years)", ylab="wife's age (years)", main=paste("Age. Correlation =", round(cor_age,3)), xlim=c(18,100), ylim=c(18,100))
abline(0,1,lty=3, col="grey")
par(old.par)



e$wife_BMI<-e$wife_weight/(e$wife_height^2/10000)
e$husband_BMI<-e$husband_weight/(e$husband_height^2/10000)

# correlation without survey weights
cor(e$husband_BMI, e$wife_BMI)

# correlation with survey weights
cor_bmi<-cov.wt(data.frame(e$husband_BMI, e$wife_BMI), wt=e$avg_survey_weight, cor=TRUE)$cor[2,1]

plot(jitter(e$husband_BMI,3), jitter(e$wife_BMI,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's BMI", ylab="wife's BMI", main=paste("BMI. Correlation =", round(cor_bmi,3)), xlim=c(14,55), ylim=c(14,55))
abline(0,1,lty=3, col="grey")




#### INCOME WITHOUT LOGARITHMIC SCALE

plot(jitter(e2$husband_income,3), jitter(e2$wife_income,3), col=rgb(0,0,0,0.2), pch=16, xlab="husband's income", ylab="wife's income", main=paste("INCOME. Correlation =", round(cor_income,3)))
abline(0,1,lty=3, col="grey")
