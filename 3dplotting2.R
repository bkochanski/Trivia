set.seed(123)
library(plotly)
library(MASS)
sim<-MASS::mvrnorm(50, 
              mu = c(0,0,0), 
              Sigma = matrix(c(1, 0.7, .6,
                            0.7, 1, .6,
                            .6, .6, 1), nrow=3)) %>% round(., 2)

df<-data.frame(x1 = sim[,1], x2=sim[,2], y = sim[,3])

fig <- plot_ly() %>% 
  add_trace(data = df, x = ~x1, y = ~x2, z = ~y, 
            type = "scatter3d", 
            mode = "markers", 
            hoverinfo = 'text', 
            text = ~paste0("x1:  ", x1, "<br>x2:  ", x2, "<br>  y:  ", y)) %>%
  layout(scene = list(
    xaxis = list(range = c(-2.5, 2.5)),  # Set x-axis limits
    yaxis = list(range = c(-2.5, 2.5)),  # Set y-axis limits
    zaxis = list(range = c(-2.5, 2.5))     # Set z-axis limits
  ))
fig



# Fit the linear regression model
lm_model <- lm(y ~ x1 + x2, data = df)
df$yhat <- lm_model$fitted.values

# Generate a grid of points for the surface
x_grid <- seq(-2.5, 2.5, length.out = 6)
y_grid <- seq(-2.5, 2.5, length.out = 6)
grid_df <- expand.grid(x1 = x_grid, x2 = y_grid)
grid_df$predicted_y <- predict(lm_model, newdata = grid_df)

fig2 <- fig

for (i in 1:length(x_grid)) {
  fig2 <- fig2 %>% add_trace(
    x = c(x_grid[i], x_grid[i]),
    y = c(-3, 3),
    z = c(predict(lm_model, newdata=data.frame(x1=x_grid[i], x2=-3)), predict(lm_model, newdata=data.frame(x1=x_grid[i], x2=3))),
    type = "scatter3d",
    mode = "lines",
    hoverinfo = 'none',
    line = list(width = 1, color = "black", opacity=0.3)
  )
}

for (i in 1:length(y_grid)) {
  fig2 <- fig2 %>% add_trace(
    x = c(-3, 3),
    y = c(y_grid[i], y_grid[i]),
    z = c(predict(lm_model, newdata=data.frame(x1=-3, x2=y_grid[i])), predict(lm_model, newdata=data.frame(x1 = 3, x2=y_grid[i]))),
    type = "scatter3d",
    mode = "lines",
    hoverinfo = 'none', 
    line = list(width = 1, color = "black", opacity=0.3)
  )
}

fig2 <- fig2 %>% layout(showlegend = FALSE) %>%
  add_trace(data = grid_df, x = ~x1, y = ~x2, z = ~predicted_y, 
            type="mesh3d", opacity = 0.5, hoverinfo = 'none')

fig2

fig3 <- fig2

fig3 <- fig3 %>% layout(showlegend = FALSE) %>%
  add_trace(data = df, x = ~x1, y = ~x2, z = ~yhat, 
            type="scatter3d", mode="markers", 
            opacity = .6, marker = list(color = 'red', size=5), 
            hoverinfo = 'text', 
            text = ~paste0("x1:  ", x1, "<br>x2:  ", x2, "<br>  yhat:  ", round(yhat,4)))

fig3

fig4<-fig3 %>% layout(showlegend = FALSE)

for (i in 1:length(df$yhat)) {
  fig4 <- fig4 %>% add_trace(
    x = c(df$x1[i], df$x1[i]),
    y = c(df$x2[i], df$x2[i]),
    z = c(df$y[i], df$yhat[i]),
    type = "scatter3d",
    mode = "lines",
    hoverinfo = 'none', 
    line = list(width = 5, color = "red")
  )
}
fig4