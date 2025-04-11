#t-coopula test
# Set seed for reproducibility
library(copula)
set.seed(123)

# Define correlation matrix for 3 variables
rho <- 0.6  # Common correlation
Sigma <- matrix(c(1, rho, rho, 
                  rho, 1, rho, 
                  rho, rho, 1), 
                nrow = 3, byrow = TRUE)

# Define degrees of freedom for t-distribution
df <- 5  # Adjust as needed

# Create t-copula object
###### NIE DZIAŁA ####
t_cop <- tCopula(param = rho, dim = 3, df = df)
#?tCopula
# Generate 1000 samples from the copula
samples <- rCopula(10000, t_cop)

cor(samples)

# Convert copula samples to marginal distributions (e.g., Normal)
X1 <- qnorm(samples[,1], mean = 0, sd = 1)
X2 <- qnorm(samples[,2], mean = 5, sd = 2)
X3 <- qnorm(samples[,3], mean = 10, sd = 3)

# Combine into a data frame
data <- data.frame(X1, X2, X3)

# Preview the first few rows
head(data)
