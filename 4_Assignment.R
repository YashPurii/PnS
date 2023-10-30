#Ques 1
x = c(0,1,2,3,4)
f_x =c(0.41,0.37,0.16,0.05,0.01)

xm =weighted.mean(x,f_x)
xm

#Ques 2
# Define the probability density function f(t)
f <- function(t) 0.1 * exp(-0.1 * t)

# Calculate the expected value using the integrate() function
expected_value <- integrate(function(t) t * f(t), lower = 0, upper = Inf)$value
expected_value

#Ques 3
# Define the probability mass function p(x)
x <- c(0, 1, 2, 3)
p_x <- c(0.1, 0.2, 0.2, 0.5)

# Calculate the expected value of Y
expected_value_Y <- sum((12 * x - 2 * (3 - x)) * p_x)
expected_value_Y

#Ques 4
# Define the probability density function f(x)
f_x <- function(x) 0.5 * exp(-abs(x))

# Calculate the first moment (mean)
mean_X <- integrate(function(x) x * f_x(x), lower = 1, upper = 10)$value

# Calculate the second moment
second_moment_X <- integrate(function(x) x^2 * f_x(x), lower = 1, upper = 10)$value

# Calculate variance
variance_X <- second_moment_X - mean_X^2

mean_X
variance_X

#Ques 5
# Define the probability distribution of X
x_values <- 1:5
p_X <- (3/4) * (1/4)^(x_values - 1)

# Calculate the probability distribution of Y
y_values <- x_values^2
p_Y <- p_X

# Calculate the expected value of Y for X = 1,2,3,4,5
expected_values_Y <- sum(y_values * p_Y)

# Calculate the variance of Y for X = 1,2,3,4,5
variance_Y <- sum((y_values - expected_values_Y)^2 * p_Y)

p_Y
expected_values_Y
variance_Y

