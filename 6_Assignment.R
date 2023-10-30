#Ques 1
# Load the necessary library for numerical integration
library(pracma)

# Define the joint probability density function f(x, y)
f <- function(x, y) {
  valid_indices <- (0 <= x & x <= 1) & (0 <= y & y <= 1)
  result <- ifelse(valid_indices, 2 * (2 * x + 3 * y) / 5, 0)
  return(result)
}


# (i) Check if it's a joint density function using integral2
is_joint_density <- integral2(f, 0, 1, 0, 1)

if (is_joint_density$Q = 1){
  cat("The provided function is a joint density function.\n")
} else{
  cat("The provided function is not a joint density function.\n")
}

# (ii) Find the marginal distribution g(x) at x = 1
x_value <- 1
g_x <- integrate(function(y) f(x_value, y), lower = 0, upper = 1)$value
cat("The marginal distribution g(x) at x = 1 is:", g_x, "\n")

# (iii) Find the marginal distribution h(y) at y = 0
y_value <- 0
h_y <- integrate(function(x) f(x, y_value), lower = 0, upper = 1)$value
cat("The marginal distribution h(y) at y = 0 is:", h_y, "\n")

# (iv) Find the expected value of g(x, y) = xy
expected_value <- integral2(function(x, y) x * y * f(x, y), 0, 1, 0, 1)

cat("The expected value of g(x, y) = xy is:",expected_value$Q, "\n")



## Ques 2 
# Define the joint mass function
f <- matrix(0, nrow = 4, ncol = 3)
x_values <- 0:3
y_values <- 0:2

for (i in 1:length(x_values)) {
  for (j in 1:length(y_values)) {
    f[i, j] <- (x_values[i] + y_values[j]) / 30
  }
}

# Display the joint mass function in rectangular (matrix) form
print(f)

is_joint_mass_function <- sum(f) == 1

if (is_joint_mass_function) {
  cat("The provided function is a joint mass function.\n")
} else {
  cat("The provided function is not a joint mass function.\n")
}

x_marginal <- apply(f, 1, sum)
cat("Marginal distribution g(x) for x = 0, 1, 2, 3:", x_marginal, "\n")

y_marginal <- apply(f, 2, sum)
cat("Marginal distribution h(x) for y = 0, 1, 2:", y_marginal, "\n")

x_conditioned_on_y <- f[1, 2] / y_marginal[2]
cat("Conditional probability at x = 0 given y = 1:", x_conditioned_on_y, "\n")

x_values <- 0:3
y_values <- 0:2

# Calculate E(x) and E(y)
E_x <- sum(x_values * x_marginal)
E_y <- sum(y_values * y_marginal)

# Calculate E(xy)
E_xy <- sum(outer(x_values, y_values, "*") * f)

# Calculate Var(x) and Var(y)
Var_x <- sum((x_values - E_x)^2 * x_marginal)
Var_y <- sum((y_values - E_y)^2 * y_marginal)

# Calculate Cov(x, y)
Cov_xy <- E_xy - E_x * E_y

# Calculate the correlation coefficient
correlation_coefficient <- Cov_xy / (sqrt(Var_x) * sqrt(Var_y))

cat("E(x):", E_x, "\n")
cat("E(y):", E_y, "\n")
cat("E(xy):", E_xy, "\n")
cat("Var(x):", Var_x, "\n")
cat("Var(y):", Var_y, "\n")
cat("Cov(x, y):", Cov_xy, "\n")
cat("Correlation Coefficient:", correlation_coefficient, "\n")
