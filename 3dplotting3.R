library(plotly)
library(MASS)

set.seed(123)

#Symulowane dane z rozkładu normalnego trójwymiarowego
sim <- MASS::mvrnorm(50, mu = c(0, 0, 0), 
                     Sigma = matrix(c(1, 0.7, .6, 0.7, 1, .6, .6, .6, 1),
                      nrow = 3)) %>% round(., 2)
df <- data.frame(x1 = sim[, 1], x2 = sim[, 2], y = sim[, 3])

fig <- plot_ly() %>%
  add_trace(
    data = df, x = ~ x1, y = ~ x2, z = ~ y,
    type = "scatter3d", mode = "markers", hoverinfo = 'text',
    text = ~ paste0("x1:  ", x1, "<br>x2:  ", x2, "<br>  y:  ", y)
  ) %>%
  layout(scene = list(
    xaxis = list(range = c(-2.5, 2.5)),
    yaxis = list(range = c(-2.5, 2.5)),
    zaxis = list(range = c(-2.5, 2.5))
  ))
fig

# model regresji
lm_model <- lm(y ~ x1 + x2, data = df)
# wartości dopasowane
df$yhat <- lm_model$fitted.values

# siatka punktów - płaszczyzna regresji
x1_grid <- seq(-2.5, 2.5, length.out = 6)
x2_grid <- seq(-2.5, 2.5, length.out = 6)
grid_df <- expand.grid(x1 = x1_grid, x2 = x2_grid)
grid_df$predicted_y <- predict(lm_model, newdata = grid_df)

for (i in 1:length(x1_grid)) {
  fig <- fig %>% add_trace(
    x = c(x1_grid[i], x1_grid[i]),
    y = c(-3, 3),
    z = c(
      predict(lm_model, newdata = data.frame(x1 = x1_grid[i], x2 = -3)),
      predict(lm_model, newdata = data.frame(x1 = x1_grid[i], x2 = 3))
    ),
    type = "scatter3d", mode = "lines", hoverinfo = 'none',
    line = list(
      width = 1, color = "black", opacity = 0.3
    )
  )
}

for (i in 1:length(x2_grid)) {
  fig <- fig %>% add_trace(
    x = c(-3, 3),
    y = c(x2_grid[i], x2_grid[i]),
    z = c(
      predict(lm_model, newdata = data.frame(x1 = -3, x2 = x2_grid[i])),
      predict(lm_model, newdata = data.frame(x1 = 3, x2 = x2_grid[i]))
    ),
    type = "scatter3d", mode = "lines", hoverinfo = 'none',
    line = list(
      width = 1, color = "black", opacity = 0.3
    )
  )
}

# płaszczyzna regresji - powierzchnia
fig <- fig %>% layout(showlegend = FALSE) %>%
  add_trace(
    data = grid_df, x = ~ x1, y = ~ x2, z = ~ predicted_y,
    type = "mesh3d", opacity = 0.5, hoverinfo = 'none'
  )

# wartości dopasowane
fig <- fig %>%
  add_trace(
    data = df,
    x = ~ x1, y = ~ x2, z = ~ yhat,
    type = "scatter3d",
    mode = "markers",
    opacity = .6,
    marker = list(color = 'red', size = 5),
    hoverinfo = 'text',
    text = ~ paste0("x1:  ", x1, "<br>x2:  ", x2, "<br>  yhat:  ", round(yhat, 4))
  )

# reszty
for (i in 1:length(df$yhat)) {
  fig <- fig %>% add_trace(
    x = c(df$x1[i], df$x1[i]),
    y = c(df$x2[i], df$x2[i]),
    z = c(df$y[i], df$yhat[i]),
    type = "scatter3d",
    mode = "lines",
    hoverinfo = 'none',
    line = list(width = 5, color = "red")
  )
}
fig
