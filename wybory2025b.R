library(jsonlite)
library(httr)
url <- "https://gist.githubusercontent.com/BERENZ/76d82d6aaf55cec6c9ff0ee42febe736/raw/4146cbfe836a2851ab244ea99ebfe0f5e29abfd1/ELECTIONS-LOCAL-DEMOGRAPHIC-FREQUENCY.json"
response <- GET(url)
json_data <- fromJSON(content(response, "text"))

a<-json_data$data$items$data$data

names<-json_data$data$items$committee_items[[4]][,1:2]
g1 <- json_data$data$items$category_items[[1]]
g2 <- json_data$data$items$category_items[[2]]
g2 <- gsub("tyś", "tys", g2)
g3 <- json_data$data$items$category_items[[3]]
g4 <- json_data$data$items$category_items[[4]]


a1 <- data.frame(t(sapply(a, function(x) x[[1]])))
colnames(a1) <- g1
a1$`_id` <- rownames(a1)

a2 <- data.frame(t(sapply(a, function(x) x[[2]])))
colnames(a2) <- g2
a2$`_id` <- rownames(a2)

a3 <- data.frame(t(sapply(a, function(x) x[[3]])))
colnames(a3) <- g3
a3$`_id` <- rownames(a3)

a4 <- data.frame(t(sapply(a, function(x) x[[4]])))
colnames(a4) <- g4
a4$`_id` <- rownames(a4)

merge(names, a1)
merge(names, a2)
merge(names, a3)
merge(names, a4)

library(ggplot2)
library(dplyr)
library(tidyr)

merge(names, a3) %>%
  pivot_longer(
    cols = 3:7,
    names_to = "Age_Group",
    values_to = "Percentage"
  ) %>%
  ggplot(aes(x = reorder(Age_Group, -Percentage), y = Percentage, fill = title)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Wiek wyborców kandydatów",
       x = "Grupa wiekowa",
       y = "Procent poparcia",
       fill = "Kandydat") +
  theme_minimal()


merge(names, a3) %>%
  pivot_longer(
    cols = 3:7,
    names_to = "Age_Group",
    values_to = "Percentage"
  ) %>%
  filter(title!="Maciej Maciak"
         & title!="Marek Jakubiak"
         & title!="Artur Bartoszewicz"
         & title!="Marek Woch"
         # filter(title=="Maciej Maciak" 
         #            | title=="Marek Jakubiak" 
         #            | title=="Artur Bartoszewicz" 
         #            | title=="Marek Woch" 
  ) -> df

df$title<-factor(df$title, levels=c("Rafał Trzaskowski", "Karol Nawrocki", 
                                    "Sławomir Mentzen", "Grzegorz Braun",
                                    "Adrian Zandberg", "Magdalena Biejat", "Szymon Hołownia", 
                                    "Joanna Senyszyn", "Krzysztof Stanowski", 
                                    "Marek Jakubiak", "Artur Bartoszewicz",
                                    "Marek Woch"
))



ggplot(df, aes(x = Age_Group, y = Percentage)) +
  geom_bar(stat = "identity", fill="blue") +
  geom_text(aes(label = Percentage),
            vjust = -0.3, size = 3) + 
  facet_wrap(~ title, strip.position = "top", scales = "free_x") +
  labs(title = "Poparcie kandydatów w grupach wiekowych",
       x = "Grupa wiekowa",
       y = "Procent poparcia") +
#  coord_cartesian(ylim = c(0, 48)) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(caption = "Źródło danych: https://www.tvp.info/wybory-prezydenckie-2025 via https://gist.github.com/BERENZ")




merge(names, a4) %>%
  pivot_longer(
    cols = 3:4,
    names_to = "Group",
    values_to = "Percentage"
  ) %>%
  filter(title!="Maciej Maciak"
         & title!="Marek Jakubiak"
         & title!="Artur Bartoszewicz"
         & title!="Marek Woch"
         # filter(title=="Maciej Maciak" 
         #            | title=="Marek Jakubiak" 
         #            | title=="Artur Bartoszewicz" 
         #            | title=="Marek Woch" 
  ) -> df
df$title<-factor(df$title, levels=c("Rafał Trzaskowski", "Karol Nawrocki", 
                                    "Sławomir Mentzen", "Grzegorz Braun",
                                    "Adrian Zandberg", "Magdalena Biejat", "Szymon Hołownia", 
                                    "Joanna Senyszyn", "Krzysztof Stanowski", 
                                    "Marek Jakubiak", "Artur Bartoszewicz",
                                    "Marek Woch"
))
ggplot(df, aes(x = Group, y = Percentage, fill=Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percentage),
            vjust = -0.3, size = 3) + 
  facet_wrap(~ title, strip.position = "top", scales = "free_x") +
  labs(title = "Poparcie kandydatów według płci",
       x = "Płeć",
       y = "Procent poparcia") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Kobiety" = "orange", "Mężczyźni" = "blue")) +  # set your category names here
#  coord_cartesian(ylim = c(0, 35)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(caption = "Źródło danych: https://www.tvp.info/wybory-prezydenckie-2025 via https://gist.github.com/BERENZ")





merge(names, a1) %>%
  pivot_longer(
    cols = 3:6,
    names_to = "Group",
    values_to = "Percentage"
  ) %>%
  filter(title!="Maciej Maciak"
         & title!="Marek Jakubiak"
         & title!="Artur Bartoszewicz"
         & title!="Marek Woch"
         # filter(title=="Maciej Maciak" 
         #            | title=="Marek Jakubiak" 
         #            | title=="Artur Bartoszewicz" 
         #            | title=="Marek Woch" 
  ) -> df
df$title<-factor(df$title, levels=c("Rafał Trzaskowski", "Karol Nawrocki", 
                                    "Sławomir Mentzen", "Grzegorz Braun",
                                    "Adrian Zandberg", "Magdalena Biejat", "Szymon Hołownia", 
                                    "Joanna Senyszyn", "Krzysztof Stanowski", 
                                    "Marek Jakubiak", "Artur Bartoszewicz",
                                    "Marek Woch"
))

df$Group<-factor(df$Group, levels = c("Podstawowe", "Zasadnicze zawodowe", "Średnie i pomaturalne", "Licencjat i wyższe"))

ggplot(df, aes(x = Group, y = Percentage, fill=Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percentage),
            vjust = -0.3, size = 3) + 
  facet_wrap(~ title, strip.position = "top", scales = "free_x") +
  labs(title = "Poparcie kandydatów według wykształcenia",
       x = "",
       y = "Procent poparcia") +
  theme_minimal() +
  theme(legend.position = "none") +
#  coord_cartesian(ylim = c(0, 55)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(caption = "Źródło danych: https://www.tvp.info/wybory-prezydenckie-2025 via https://gist.github.com/BERENZ")





merge(names, a2) %>%
  pivot_longer(
    cols = 3:7,
    names_to = "Group",
    values_to = "Percentage"
  ) %>%
  filter(title!="Maciej Maciak"
         & title!="Marek Jakubiak"
         & title!="Artur Bartoszewicz"
         & title!="Marek Woch"
         # filter(title=="Maciej Maciak" 
         #            | title=="Marek Jakubiak" 
         #            | title=="Artur Bartoszewicz" 
         #            | title=="Marek Woch" 
  ) -> df
df$title<-factor(df$title, levels=c("Rafał Trzaskowski", "Karol Nawrocki", 
                                    "Sławomir Mentzen", "Grzegorz Braun",
                                    "Adrian Zandberg", "Magdalena Biejat", "Szymon Hołownia", 
                                    "Joanna Senyszyn", "Krzysztof Stanowski", 
                                    "Marek Jakubiak", "Artur Bartoszewicz",
                                    "Marek Woch"
))

df$Group<-factor(df$Group, levels = g2)
ggplot(df, aes(x = Group, y = Percentage)) +
  geom_bar(stat = "identity", fill="orange") +
  geom_text(aes(label = Percentage),
            vjust = -0.3, size = 3) + 
  facet_wrap(~ title, strip.position = "top", scales = "free_x") +
  labs(title = "Poparcie kandydatów według wielkości miejscowości",
       x = "",
       y = "Procent poparcia") +
  theme_minimal() +
  theme(legend.position = "none") +
#  coord_cartesian(ylim = c(0, 45)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(caption = "Źródło danych: https://www.tvp.info/wybory-prezydenckie-2025 via https://gist.github.com/BERENZ")


