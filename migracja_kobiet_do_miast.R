library(bdl)

#60559 - gęstość zaludnienia (ludność na km3)
gestosc<-get_data_by_variable("60559", unitLevel = "6", year = 2022)
# 72301 - mężczyźni 0-4 
m04<-get_data_by_variable("72301", unitLevel = "6", year = 2022)
f04<-get_data_by_variable("72296", unitLevel = "6", year = 2022)
m59<-get_data_by_variable("72302", unitLevel = "6", year = 2022)
f59<-get_data_by_variable("72297", unitLevel = "6", year = 2022)
# 
m2529<-get_data_by_variable("47736", unitLevel = "6", year = 2022)
f2529<-get_data_by_variable("47696", unitLevel = "6", year = 2022)
m3034<-get_data_by_variable("47724", unitLevel = "6", year = 2022)
f3034<-get_data_by_variable("47695", unitLevel = "6", year = 2022)

library(dplyr)
library(ggplot2)
gestosc %>%
  select(c(id, name, gestosc=val)) %>%
  merge(select(m04, c(id, m04=val)))%>%
  merge(select(f04, c(id, f04=val)))%>%
  merge(select(m59, c(id, m59=val)))%>%
  merge(select(f59, c(id, f59=val)))%>%
  merge(select(m2529, c(id, m2529=val)))%>%
  merge(select(f2529, c(id, f2529=val)))%>%
  merge(select(m3034, c(id, m3034=val)))%>%
  merge(select(f3034, c(id, f3034=val))) -> migracje

#saveRDS(migracje, 'migracje.rds')
#migracje<-readRDS('migracje.rds')

library(tidyr)
migracje%>%
  mutate(m09=m04+m59, f09=f04+f59, m2534=m2529+m3034, f2534=f2529+f3034)%>%
  mutate(sexratio09=m09/f09, sexratio2534=m2534/f2534)%>%
#  filter(sexratio2534>8)%>%head()
  filter(substring(id,12,12)<"4")%>%
  select(id, name, gestosc, sexratio09, sexratio2534) -> df
  
  df %>% pivot_longer(4:5, names_to='group', values_to='m2f_ratio') %>% 
  mutate(group = ifelse(group=='sexratio09', '0-9 lat', '25-34 lat')) %>%
  ggplot(aes(x=gestosc, y=m2f_ratio, col=group, label=name)) + 
  scale_x_continuous(trans='log10') + 
  geom_point(alpha=.4) + 
  stat_smooth(method='lm', se=FALSE) +
  scale_color_manual(name='', values=c('blue', 'red'))+
  xlab("gęstość zaludnienia (liczba ludności przypadająca na km^2)") +
  ylab("stosunek liczby mężczyzn do liczby kobiet \nw grupie wiekowej") -> GG1
  GG1


  library(plotly)
  ggplotly(GG1)

  ggplot(df) +
  scale_x_continuous(trans='log10') +
  geom_point(aes(x=gestosc, y=sexratio2534), col='red', alpha=.2) +
  stat_smooth(aes(x=gestosc, y=sexratio2534), method='lm', se=FALSE, alpha=.2, col='red') +
  geom_point(aes(x=gestosc, y=sexratio09), col='blue', alpha=.2) +
  stat_smooth(aes(x=gestosc, y=sexratio09), method='lm', se=FALSE, alpha=.2, col='blue') +
  xlab("gęstość zaludnienia w osobach na km^2") +
  ylab("stosunek liczby mężczyzn do liczby kobiet")

  cor(df$gestosc, df$sexratio09)  
  cor(df$gestosc, df$sexratio2534)  
  



