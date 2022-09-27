a<-read.csv(url("https://raw.githubusercontent.com/eddwebster/football_analytics/master/data/capology/engineered/capology_big5_latest.csv"))
a2<-a[a$season=="2020-2021",]
a3<-a2[a2$annual_gross_base_salary_gbp>0,]
hist(a3$annual_gross_base_salary_gbp, breaks=10000, xlim=c(0,1e05))

hist(as.numeric(substr(as.character(a3$annual_gross_base_salary_gbp),1,1))
     , breaks=0:9+.5)
hist(as.numeric(substr(as.character(a3$weekly_gross_base_salary_gbp),1,1))
     , breaks=0:9+.5)
hist(as.numeric(substr(as.character(a3$annual_gross_base_salary_gbp*5),1,1))
     , breaks=0:9+.5)
hist(as.numeric(substr(as.character(a3$annual_gross_base_salary_gbp/365),1,1))
     , breaks=0:9+.5)

m<-mean(a3$annual_gross_base_salary_gbp)
m
median(a3$annual_gross_base_salary_gbp)
max(a3$annual_gross_base_salary_gbp)
min(a3$annual_gross_base_salary_gbp)
s<-sd(a3$annual_gross_base_salary_gbp)
s
mean(a3$annual_gross_base_salary_gbp>=m-2*s &
       a3$annual_gross_base_salary_gbp<= m+2*s)
mean(a3$annual_gross_base_salary_gbp>=m-s &
       a3$annual_gross_base_salary_gbp<= m+s)
mean(a3$annual_gross_base_salary_gbp>=m-3*s &
       a3$annual_gross_base_salary_gbp<= m+3*s)

e1071::kurtosis(a3$annual_gross_base_salary_gbp)
e1071::skewness(a3$annual_gross_base_salary_gbp)


write.csv(a, 'footballplayers.csv')
