cena <- bdl::get_data_by_variable("633704",unitLevel = "5", year = 2011:2021)
library(sf)
powiaty<-read_sf("mapy/powiaty.shp")
c2015<-cena[cena$year==2015,c("id", "name", "val")]
names(c2015)[3]<-"val2015"
c2021<-cena[cena$year==2021,c("id", "name", "val")]
names(c2021)[3]<-"val2021"

c1<-merge(c2015, c2021)
c1<-c1[c1$val2015>0,]
c1<-c1[c1$val2021>0,]
c1$increase<-c1$val2021/c1$val2015-1
c1$increase<-c1$increase*100
dane<-c1
dane$jpt_kod_je<-paste0(substr(dane$id, 3, 4), substr(dane$id,8,9))
dane2<-merge(dane, powiaty)

library(ggplot2)
ggplot(dane2)+geom_sf(aes(fill=increase, geometry=geometry))

ggplot(dane2)+geom_sf(aes(fill=increase, geometry=geometry))+scale_fill_gradient(low="green", high="red")+theme_void()
#!(dane2$jpt_kod_je %in% c("1215", "2203"))

ggplot(dane2[dane2$increase<100,])+geom_sf(aes(fill=increase, geometry=geometry))+scale_fill_gradient(low="green", high="red")+theme_void()
