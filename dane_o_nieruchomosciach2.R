cena1 <- bdl::get_data_by_variable("633702",unitLevel = "5", year = 2013:2021)
Y<-cena1[, 1:4]
names(Y)<-c("id", "name", "year", "Y")
Y[[4]][cena1$attrId!=1]<-NA

a<-sqldf::sqldf("select id, name, count(1) count_ from Y where Y is null group by id, name order by 3 ")
averages<-sqldf::sqldf("select year, avg(Y) avgY from Y where id not in (select id from a) group by year")

# tabela "szeroka" z powiatami jako kolumnami
Y_wide<-tidyr::spread(Y[, c(1,3,4)], id, Y)
write.csv2(Y_wide, 'temp.csv')
write.csv2(averages, 'temp2.csv')

library(mice)

tmp<-merge(Y[Y$id==a[15,1],3:4], averages)

complete(mice(tmp, method = "norm.predict", m = 1))

imputedY<-list()
library(mice)
for(i in 1:14) {
  print(i)
  tmp<-merge(Y[Y$id==a[i,1],3:4], averages)
  tmp2<-(complete(mice(tmp, method = "norm.predict", m = 1)))
  imputedY[[i]]<-data.frame(id=a[i,1], name=a[i,2], year=tmp2$year, Y=tmp2$Y)
  }

#ostrołęka
i=15
tmp<-merge(Y[Y$id==a[i,1],3:4], averages)
mean(tmp$Y, na.rm=TRUE)
mean(tmp$avgY, na.rm=TRUE)
tmp3<-tmp$avgY*mean(tmp$Y, na.rm=TRUE)/mean(tmp$avgY, na.rm=TRUE)
imputedY[[i]]<-data.frame(id=a[i,1], name=a[i,2], year=tmp2$year, Y=tmp$Y)
imputedY[[i]]$Y[is.na(tmp$Y)]<-tmp3[is.na(tmp$Y)]



imputedY<-rbind(dplyr::bind_rows(imputedY), Y[!(Y$id %in% a$id),])


# Uśrednianie pojedynczej kolumny (możliwa imputacja medianą: mean -> median)
# panel$V1<-ave(panel$V1,panel$index,FUN=function(x) ifelse(is.na(x), median(x,na.rm=TRUE), x))