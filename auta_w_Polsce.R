polska_auta<-bdl::get_data_by_variable("32561", unitLevel=0, lang='en')
library(ggplot2)
ggplot(data=polska_auta, aes(x=year, y=val))+
  geom_col(fill="dark blue")+ scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))+
  geom_text(aes(label = format(val, big.mark=" ", small.mark=" ")), angle=90, hjust=1.05, col="white")+xlab("rok")+ylab("liczba samochodów")+ggtitle("Liczba zarejestrowanych samochodów osobowych w Polsce")
((polska_auta$val[length(polska_auta$val)]/polska_auta$val[1])^
(1/(length(polska_auta$val)-1))-1)*100
