library(plotly)
library(MASS)
sim<-MASS::mvrnorm(300, 
              mu = c(0,0,0), 
              Sigma = matrix(c(1, 0.6, .6,
                            0.6, 1, .6,
                            .6, .6, 1), nrow=3)
)
df<-data.frame(x1 = sim[,1], x2=sim[,2], y = sim[,3])


fig <- plot_ly(df, x = ~x1, y = ~x2, z = ~y)
fig <- fig %>% add_markers()
fig

plot_ly(df, x=~x, y=~y)

model1<-lm(y~x1+x2, df)
df2 <- 

fig <- plot_ly() %>% 
  add_trace(data=df, x = ~x1, y = ~x2, z= ~y, type="scatter3d", mode="markers") 

fig <- plot_ly() %>% 
  add_trace(data = df, x = ~x1, y = ~x2, z = ~y, type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(range = c(-4, 4)),  # Set x-axis limits
    yaxis = list(range = c(-4, 4)),  # Set y-axis limits
    zaxis = list(range = c(-4, 4))     # Set z-axis limits
  ))
fig

# Fit the linear regression model
lm_model <- lm(y ~ x1 + x2, data = df)

# Generate a grid of points for the surface
x_grid <- seq(-4, 4, length.out = 2^6+1)
y_grid <- seq(-4, 4, length.out = 2^6+1)
grid_df <- expand.grid(x1 = x_grid, x2 = y_grid)
grid_df$predicted_y <- predict(lm_model, newdata = grid_df)

# Create the surface trace
#surface_trace <- plot_ly(data = grid_df, x = ~x1, y = ~x2, z = ~predicted_y, type = "surface3d", opacity = 0.5)

# Combine the scatter plot and surface trace
fig2 <- fig %>%
  add_trace(data = grid_df, x = ~x1, y = ~x2, z = ~predicted_y, type="scatter3d", mode="lines")

# Show the updated plot
fig2
