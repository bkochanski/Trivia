library(tidyverse)
options(scipen = 20)
dane = readxl::read_excel("danePL.xlsx") %>%
  mutate_if(is.character, as.factor) %>%
  select(-id)

# Oczyszczenie danych
sapply(dane, function(x) length(unique(x))) %>%
  sort(decreasing = TRUE)

# pomińmy "kierunek ukończonych studiów"
dane = dane %>%
  select(-`kierunek ukończonych studiów`)

# Analiza korelacji
# Wektor, który będzie przechowywać nazwy zmiennych skorelowanych
correlated_vars <- c()
results<-list()
results_colname<-c()
results_p<-c()
results_mine<-c()

# Przechodzimy przez wszystkie kolumny w naszych danych
for(colname in colnames(dane)) {
  # Pomijamy kolumnę "Praca nieformalna", ponieważ to nasza zmienna celu
  if(colname != "Praca nieformalna") {
    # Tworzymy tabelę krzyżową dla kolumny i zmiennej celu
    contingency_table <- table(dane[[colname]], dane[["Praca nieformalna"]])
    
    # Sprawdzamy, czy wszystkie oczekiwane wartości są większe niż 5 (jest to jedno z założeń testu chi-kwadrat)
    expected_values <- chisq.test(contingency_table)$expected

    results[[length(results)+1]]<-
    list(colname, 
    contingency_table,
    chisq.test(contingency_table)$expected,
    min(expected_values),
    chisq.test(contingency_table)$p.value)
    
    results_colname[length(results)+1]<-colname
    results_p[length(results)+1]<-chisq.test(contingency_table)$p.value
    results_mine[length(results)+1]<-min(expected_values)
    
        
    if (min(expected_values) < 5) next
    
    # Wykonujemy test chi-kwadrat
    chi2_test <- chisq.test(contingency_table)

    
    # Jeśli p-wartość jest mniejsza niż 0.05, uważamy, że zmienna jest skorelowana
    if(chi2_test$p.value < 0.05) {
      correlated_vars <- c(correlated_vars, colname)
    }
  }
}

correlated_vars
resdf<-data.frame(colname=results_colname, p=results_p, mine=results_mine)
View(resdf)
View(resdf[resdf$mine<5 & resdf$p<0.01 & resdf$p>0,])
listfound<-as.numeric(labels(resdf[resdf$mine<5 & resdf$p<0.01 & resdf$p>0,])[[1]])[2:length(labels(resdf[resdf$mine<5 & resdf$p<0.01  & resdf$p>0,])[[1]])]

results[listfound]

# Model regresji logistycznej z wykorzystaniem wyniku powyżej,
# użyte zostaną tylko zmienne, które są istotnie skorelowane ze zmienną zależną.
formula <- paste0('`Praca nieformalna` ~ `', paste(correlated_vars, collapse = '` + `'), '`')
model <- glm(formula, data = dane, family = binomial)

summary(model)

# Model nie jest zbieżny, ponieważ występują zmienne, które idealnie określają czy ktoś podejmował pracę nieformalną
# Lista kolumn do usunięcia
columns_to_remove <- c('Wykonywanie pracy nieformalnej dla firmy lub przedsiębiorstwa',
                       'Wykonywanie pracy nieformalnej dla rodziny, znajomych lub sąsiadów',
                       'Wykonywanie pracy nieformalnej dla prywatnego zleceniodawcy',
                       'powód: była to praca dla rodziny lub znajomych',
                       'powód: była to tylko drobna, dorywcza praca')

# Usunięcie kolumn z wektora correlated_vars
correlated_vars <- setdiff(correlated_vars, columns_to_remove)

formula <- paste0('`Praca nieformalna` ~ `', paste(correlated_vars, collapse = '` + `'), '`')
model <- glm(formula, data = dane, family = binomial)

summary(model)
