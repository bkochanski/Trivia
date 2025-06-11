t1o <- read.csv2("protokoly_po_obwodach_utf8.csv")
t2o <- read.csv2("protokoly_po_obwodach_w_drugiej_turze_utf8.csv")

#View(t1o)
#View(t2o)
t1ob <- t1o[!is.na(t1o$Teryt.Gminy),c(1,3,5,26, (43-13):43)]
names(t1ob)[5]<-"glosy1"

t2ob <- t2o[!is.na(t2o$Teryt.Gminy),c(1,3,5,25:26,30:31)]
names(t2ob) <- c(names(t1ob)[1:3], "glosy", "niewa", "KN", "RT")

wo <- merge(t1ob, t2ob)
names(wo)
wo$koal <- wo$HOŁOWNIA.Szymon.Franciszek+wo$BIEJAT.Magdalena.Agnieszka+wo$SENYSZYN.Joanna+wo$ZANDBERG.Adrian.Tadeusz
wo$opoz <- wo$MENTZEN.Sławomir.Jerzy+wo$BRAUN.Grzegorz.Michał+wo$JAKUBIAK.Marek
wo$koalp <- wo$koal/wo$glosy1*100
wo$opozp <- wo$opoz/wo$glosy1*100

y_var <- "I(RT/glosy*100-TRZASKOWSKI.Rafał.Kazimierz/glosy1*100)"
#y_var <- "I(KN-NAWROCKI.Karol.Tadeusz)"
#y_var <- "I(RT-TRZASKOWSKI.Rafał.Kazimierz)"
x_vars <- names(t1ob)[4:17]
x_vars <- c("koal", "opoz")
x_vars <- c("koalp", "opozp")


formula <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
df <- wo[wo$glosy1>1000,]
model1 <- lm(formula, data=df)
summary(model1)
formula

roznpred<-as.numeric(predict(model1, newdata=df)-df$RT/df$glosy1*100+df$TRZASKOWSKI.Rafał.Kazimierz/df$glosy*100)
roznpred2<-roznpred/df$RT*df$glosy/100
hist(rozpred2, breaks=100)
df$RT1<-df$TRZASKOWSKI.Rafał.Kazimierz
df$KN1<-df$NAWROCKI.Karol.Tadeusz
df$roznpred<-roznpred
df$roznpred2<-roznpred2
df$zmglos<-df$glosy/df$glosy1
View(merge(df[df$roznpred2< (-.3),], t1o[,1:5]))
View(merge(df[df$roznpred2> (.3),], t1o[,1:5]))
df[df$roznpred2> (.3),]

#View(df)


df[which(rozpred2>.5,),1]
df[rozpred2>.5,1]
df[which(is.na(rozpred2)),]
