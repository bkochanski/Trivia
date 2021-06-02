jpeg('husbandincomecorrelations.jpg')

old.par<-par(mar=rep(4,4), mfrow=c(2,2))

cor_hi_ww<-cov.wt(data.frame(e$husband_income,e$wife_weight)[!is.na(e$husband_income),], wt=e$avg_survey_weight[!is.na(e$husband_income)], cor=TRUE)$cor[2,1]

plot(jitter(e$husband_income,3), jitter(e$wife_weight,3), col=rgb(0,0,0,0.2), pch=16, log="x", xlab="husband's monthly income (PLN)", ylab="wife's weight (kg)", main=paste("Correlation =", round(cor_hi_ww,3)))

cor_hi_hw<-cov.wt(data.frame(e$husband_income,e$husband_weight)[!is.na(e$husband_income),], wt=e$avg_survey_weight[!is.na(e$husband_income)], cor=TRUE)$cor[2,1]

plot(jitter(e$husband_income,3), jitter(e$husband_weight,3), col=rgb(0,0,0,0.2), pch=16, log="x", xlab="husband's monthly income (PLN)", ylab="husband's weight (kg)", main=paste("Correlation =", round(cor_hi_hw,3)))

cor_hi_wh<-cov.wt(data.frame(e$husband_income,e$wife_height)[!is.na(e$husband_income),], wt=e$avg_survey_weight[!is.na(e$husband_income)], cor=TRUE)$cor[2,1]

plot(jitter(e$husband_income,3), jitter(e$wife_height,3), col=rgb(0,0,0,0.2), pch=16, log="x", xlab="husband's monthly income (PLN)", ylab="wife's height (cm)", main=paste("Correlation =", round(cor_hi_wh,3)))

cor_hi_hh<-cov.wt(data.frame(e$husband_income,e$husband_height)[!is.na(e$husband_income),], wt=e$avg_survey_weight[!is.na(e$husband_income)], cor=TRUE)$cor[2,1]

plot(jitter(e$husband_income,3), jitter(e$husband_height,3), col=rgb(0,0,0,0.2), pch=16, log="x", xlab="husband's monthly income (PLN)", ylab="husband's height (cm)", main=paste("Correlation =", round(cor_hi_hh,3)))

par(old.par)

dev.off()
