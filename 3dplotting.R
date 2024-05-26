set.seed(123)
library(plotly)
library(MASS)
sim<-MASS::mvrnorm(50, 
              mu = c(0,0,0), 
              Sigma = matrix(c(1, 0.7, .6,
                            0.7, 1, .6,
                            .6, .6, 1), nrow=3)) %>% round(., 2)

df<-data.frame(x1 = sim[,1], x2=sim[,2], y = sim[,3])


fig <- plot_ly(df, x = ~x1, y = ~x2, z = ~y)
fig <- fig %>% add_markers()

fig <- plot_ly() %>% 
  add_trace(data = df, x = ~x1, y = ~x2, z = ~y, type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(range = c(-3, 3)),  # Set x-axis limits
    yaxis = list(range = c(-3, 3)),  # Set y-axis limits
    zaxis = list(range = c(-3, 3))     # Set z-axis limits
  ))

# Fit the linear regression model
lm_model <- lm(y ~ x1 + x2, data = df)
df$yhat <- lm_model$fitted.values

# Generate a grid of points for the surface
x_grid <- seq(-3, 3, length.out = 13)
y_grid <- seq(-3, 3, length.out = 13)
grid_df <- expand.grid(x1 = x_grid, x2 = y_grid)
grid_df$predicted_y <- predict(lm_model, newdata = grid_df)

# Create the surface trace
#surface_trace <- plot_ly(data = grid_df, x = ~x1, y = ~x2, z = ~predicted_y, type = "surface3d", opacity = 0.5)

# Combine the scatter plot and surface trace
# fig2a <- fig %>%
#   add_trace(data = grid_df, x = ~x1, y = ~x2, z = ~predicted_y, type="scatter3d", mode="lines")



fig2 <- fig %>% layout(showlegend = FALSE) %>%
  add_trace(data = grid_df, x = ~x1, y = ~x2, z = ~predicted_y, type="mesh3d", opacity = 0.5)

fig2 <- fig2 %>% layout(showlegend = FALSE) %>%
  add_trace(data = df, x = ~x1, y = ~x2, z = ~yhat, type="scatter3d", mode="markers", opacity = .6, marker = list(color = 'red', size=5))

fig2a<-fig2

i=1

for (i in 1:length(x_grid)) {
  fig2a <- fig2a %>% add_trace(
    x = c(x_grid[i], x_grid[i]),
    y = c(-3, 3),
    z = c(predict(lm_model, newdata=data.frame(x1=x_grid[i], x2=-3)), predict(lm_model, newdata=data.frame(x1=x_grid[i], x2=3))),
    type = "scatter3d",
    mode = "lines",
    line = list(width = 1, color = "black", opacity=0.3)
  )
}

for (i in 1:length(y_grid)) {
  fig2a <- fig2a %>% add_trace(
    x = c(-3, 3),
    y = c(y_grid[i], y_grid[i]),
    z = c(predict(lm_model, newdata=data.frame(x1=-3, x2=y_grid[i])), predict(lm_model, newdata=data.frame(x1 = 3, x2=y_grid[i]))),
    type = "scatter3d",
    mode = "lines",
    line = list(width = 1, color = "black", opacity=0.3)
  )
}

fig3<-fig2a %>% layout(showlegend = FALSE)

for (i in 1:length(df$yhat)) {
  fig3 <- fig3 %>% add_trace(
    x = c(df$x1[i], df$x1[i]),
    y = c(df$x2[i], df$x2[i]),
    z = c(df$y[i], df$yhat[i]),
    type = "scatter3d",
    mode = "lines",
    line = list(width = 5, color = "red")
  )
}
fig3
