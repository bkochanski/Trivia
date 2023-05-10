#cena1 <- bdl::get_data_by_variable("633702",unitLevel = "5", year = 2013:2021)
Y<-cena1[, 1:4]
names(Y)<-c("id", "name", "year", "Y")
Y[[4]][cena1$attrId!=1]<-NA


#powierzchnia <- bdl::get_data_by_variable("2018",unitLevel = "5", year = 2013:2021)
x1<-powierzchnia[,1:4]
names(x1)<-c("id", "name", "year", "x1")



tablica<-merge(Y, x1)

tablica$indeks<-as.numeric(factor(tablica$name))
tablica<-tablica[, c(2, ncol(tablica), 3:(ncol(tablica)-1))] 
tablica <- tablica[order(tablica$indeks, tablica$year),]





#------ Jak się pozbyć NULLI

a<-sqldf::sqldf("select id, name, count(1) count_ from Y where Y is null group by id, name order by 3 ")
averages<-sqldf::sqldf("select year, avg(Y) avgY from Y where id not in (select id from a) group by year")

complete(mice(merge(Y[Y$id==a[14,1],3:4], averages)[,2:3], method = "norm.predict", m = 1))

Y_imp<-complete(mice(data.frame(id=as.factor(Y$id), year=Y$year, Y_imp=Y$Y), method = "norm.predict", m = 1))

i=2
merge(merge(Y[Y$id==a[i,1],3:4], Y_imp[Y$id==a[i,1],2:3]), averages)

tmp<-merge(Y[Y$id==a[15,1],3:4], averages)

tmp2<-complete(mice(tmp, method = "norm.predict", m = 1))


Y_wide<-tidyr::spread(Y[, c(1,3,4)], id, Y)
write.csv2(Y_wide, 'temp.csv')
names(Y_wide)<-paste0('x', names(Y_wide))
Y_wide_imp<-mice(Y_wide, method = "norm.predict", m = 1) 
write.csv2(Y_wide, 'temp2.csv')
?mice

library(mice)




