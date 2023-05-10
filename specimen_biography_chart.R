#install.packages("ggplot2")

library("ggplot2")

dane_WIG20<-readxl::read_excel("C:/Users/Błażej/Downloads/dane_WIG20.xlsx")

ggplot(dane_WIG20) +
  geom_segment(aes(y = row, yend = row, x = date_start, xend = date_end, col=factor(row)),
               alpha = 1, size = 1.5, show.legend = FALSE) +
  theme_minimal() +
  theme( 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "white", colour = NA),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(face = "bold"))+

    geom_text(aes(x = date_start+difftime(date_end, date_start)/2, y = row, label = spolka),
            size = 3, vjust = "bottom", nudge_y = 0.25, color = "gray50") +
  scale_y_continuous("", expand = c(0.1, 0.1)) +
  xlab("")+
  facet_wrap(~ index, ncol = 1)
