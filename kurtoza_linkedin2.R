library(extraDistr)
set.seed(123)
lepto1 <- rlaplace(10000)
lepto1 <- (lepto1-mean(lepto1))/sd(lepto1)

# Create histogram
hist(lepto1, breaks=200, probability=TRUE, col="gray", border="white", main="", xlim=c(-7,7))

# Generate normal density values
x_vals <- seq(min(lepto1), max(lepto1), length.out=1000)
y_vals <- dnorm(x_vals, mean=mean(lepto1), sd=sd(lepto1))

# Fill normal distribution area with blue before plotting the histogram
polygon(c(x_vals, rev(x_vals)), c(y_vals, rep(0, length(y_vals))), col=rgb(0, 0, 1, 0.3), border=NA)

# Add normal density curve
lines(x_vals, y_vals, col="blue", lwd=2)

# Redraw histogram on top
hist(lepto1, breaks=200, probability=TRUE, col=rgb(0.5, 0.5, 0.5, 0.7), border="white", add=TRUE)

e1071::kurtosis(lepto1)

lepto2 <- c(rt(10000, 3.5), runif(6000, -4, 4), rnorm(10,0,1.5))
lepto2 <- (lepto2-mean(lepto2))/sd(lepto2)
hist(lepto2, breaks=200, probability=TRUE, col="gray", border="white", main="", xlim=c(-7,7))

# Fill normal distribution area with blue before plotting the histogram
polygon(c(x_vals, rev(x_vals)), c(y_vals, rep(0, length(y_vals))), col=rgb(0, 0, 1, 0.3), border=NA)

# Add normal density curve
lines(x_vals, y_vals, col="blue", lwd=2)

# Redraw histogram on top
hist(lepto2, breaks=200, probability=TRUE, col=rgb(0.5, 0.5, 0.5, 0.7), border="white", add=TRUE)

e1071::kurtosis(lepto2)
