library(jsonlite)
jd1 <- fromJSON("Ipsos_tvn_demografia_2tura.json")
jd1s <- jd1$SEJM_2023_row
#View(t(as.data.frame(jd1s)))

names(jd1$SEJM_2023_row)
names(jd1s)

# Recursive function to flatten JSON to desired format
flatten_json <- function(data, group1 = NA, group2 = NA) {
  if (is.list(data) && all(sapply(data, is.numeric))) {
    return(tibble(
      Group1 = group1,
      Group2 = group2,
      Candidate = names(data),
      Value = unlist(data)
    ))
  } else if (is.list(data)) {
    return(map_dfr(names(data), function(name) {
      flatten_json(data[[name]], group1 = group1, group2 = name)
    }))
  } else {
    return(tibble(
      Group1 = group1,
      Group2 = group2,
      Candidate = names(data),
      Value = data
    ))
  }
}

# Apply across all top-level JSON keys
library(purrr)
library(tibble)
result_df <- map_dfr(names(jd1), function(key) {
  flatten_json(jd1[[key]], group1 = key)
})

#View(result_df)

#unique(result_df$Candidate)

lvls<-c("TRZASKOWSKI", "NAWROCKI", "MENTZEN", "BRAUN", "HOŁOWNIA", "ZANDBERG", "BIEJAT", "STANOWSKI", "SENYSZYN", "JAKUBIAK", "BARTOSZEWICZ", "MACIAK", "WOCH")

result_df$Candidate<-factor(result_df$Candidate, level=lvls)

library(dplyr)
library(tidyr)

# Summarise

pivot <- result_df %>%
  group_by(Group1, Group2, Candidate) %>%
  summarise(Perc = sum(Value), .groups = "drop") %>%
  pivot_wider(names_from = Candidate, values_from = Perc)

print(pivot)
write.csv(pivot[grepl("_col", pivot$Group1),], "pivot2.csv")
write.csv(pivot[grepl("_row", pivot$Group1),], "pivot3.csv")
