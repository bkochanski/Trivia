powiaty <- read.csv2('powiaty.csv')
symbol<-powiaty[powiaty$symbol_ty==4,]$symbol
symbolec <- expand.grid(x = symbol, y = symbol)

g1 <- symbolec[substring(symbolec$x, 2, 3)==substring(symbolec$y, 1, 2),]

# library(dplyr)
# g1 %>% group_by(x) %>% summarize(n=n()) -> 

#install.packages("influential")
library(influential)
library(igraph)
g<-graph_from_data_frame(g1, directed = T)
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray',
     vertex.size = 0,
     edge.arrow.size = 1,
     layout = layout_nicely(g))

ciag <- data.frame(symbol=c('PGN', 'GND', 'NDZ', 'DZL', 'ZLO', 'LOP', 'OPO', 'POS', 'OST', 'STA'))


ciag$lp <- 1: dim(ciag)[1]
ciag2 <- merge(ciag, powiaty)
paste(ciag2[order(ciag2$lp),c('Dane')], ' -> ', collapse = "")

library(dplyr)
data.frame(x01 = g1$x, x02=g1$y) %>%
  merge(data.frame(x02 = g1$x, x03=g1$y)) %>%
  merge(data.frame(x03 = g1$x, x04=g1$y)) %>%
  merge(data.frame(x04 = g1$x, x05=g1$y)) %>%
  merge(data.frame(x05 = g1$x, x06=g1$y)) %>%
  merge(data.frame(x06 = g1$x, x07=g1$y)) %>%
  merge(data.frame(x07 = g1$x, x08=g1$y)) %>%
  merge(data.frame(x08 = g1$x, x09=g1$y)) %>%
  merge(data.frame(x09 = g1$x, x10=g1$y)) %>%
  merge(data.frame(x10 = g1$x, x11=g1$y))

# #https://lists.nongnu.org/archive/html/igraph-help/2009-04/msg00125.html
# find.cycles <- function(graph, k) {
#   ring <- graph.ring(k, TRUE)
#   subgraph_isomorphisms(ring, graph)
# }
# 
# #find all cycles
# N <- length(unique(unlist(g1)))
# l <- unlist(lapply(1L:N, find.cycles, graph=g), recursive=FALSE)
# 
# #extract the vertices in each cycle
# Filter(Negate(is.null), lapply(l, function(e) {
#   if (length(e) > 1L) {
#     nm <- names(e)
#     c(nm, nm[1L])
#   }
# }))
