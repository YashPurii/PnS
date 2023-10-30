# a)
prob_a <- 1 - punif(45, min = 0, max = 60)
cat("a) Probability that waiting time is more than 45 minutes:", prob_a, "\n")

# b)
prob_b <- punif(30, min = 0, max = 60) - punif(20, \min = 0, max = 60)
cat("b) Probability that waiting time lies between 20 and 30 minutes:", prob_b, "\n")

# For the Exponential Distribution
lambda <- 1/2 

# a)at x = 3
density_a <- dexp(3, rate = lambda)
cat("a) Value of the density function at x = 3:", density_a, "\n")

# b)for 0 < x < 5
x_values <- seq(0, 5, by = 0.1)
plot(x_values, dexp(x_values, rate = lambda), type = "l", ylab = "Probability Density", xlab = "x")
title("Exponential Probability Distribution")

# c)Probability that repair time takes at most 3 hours
prob_c <- pexp(3, rate = lambda)
cat("c) Probability that repair time takes at most 3 hours:", prob_c, "\n")

# d)for 0 < x < 5
plot(x_values, pexp(x_values, rate = lambda), type = "l", ylab = "Cumulative Probability", xlab = "x")
title("Cumulative Exponential Probabilities")

# e) Simulate 1000 exponential distributed random numbers with lambda = 1/2
set.seed(123)  # For reproducibility
simulated_data <- rexp(1000, rate = lambda)
hist(simulated_data, breaks = 30, main = "Simulated Exponential Data", xlab = "x")

# For the Gamma Distribution
a <- 2  # Shape parameter
beta <- 1/3  # Scale parameter

# a) Probability that the lifetime of equipment is at least 1 unit of time
prob_a_gamma <- 1 - pgamma(1, shape = a, scale = beta)
cat("a) Probability that the lifetime of equipment is at least 1 unit of time:", prob_a_gamma, "\n")

# b) Find the value of c such that P(X < c) > 0.70
c <- qgamma(0.70, shape = a, scale = beta)
cat("b) Value of c such that P(X < c) > 0.70:", c, "\n")