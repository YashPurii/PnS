#Ques 1
n <- 12
p <- 1/6

prob_7 = pbinom(7,n,p)-pbinom(6,n,p)
prob_8 = pbinom(8,n,p)-pbinom(7,n,p)
prob_9 = pbinom(9,n,p)-pbinom(8,n,p)

total_prob =prob_9+prob_8+prob_7

cat("Probability of getting 7 sixes: ",prob_7,"\n")
cat("Probability of getting 8 sixes: ",prob_8,"\n")
cat("Probability of getting 9 sixes: ",prob_9,"\n")
cat("Total Probability of getting sixes: ",total_prob,"\n")

#Ques 2
mu <- 72
sigma <- 15.2
X <- 84
z_score <- (X - mu)/sigma
z_score
abv_84 = pnorm(z_score,lower.tail = FALSE)*100
cat("% of scoring above 84% or more: ",abv_84)

#Ques 3
#part a
lambda <- 5
prob_nocar <- dpois(0,lambda)
cat("Probability of no car arriving during 10am to 11am",prob_nocar,"\n")

#part b
lambda_y <- 50
lambda_48 = dpois(48,lambda_y)
lambda_49 = dpois(49,lambda_y)
lambda_50 = dpois(50,lambda_y)

#ppois for direct answer between 48-50

total_prob_48to50 =lambda_50+lambda_49+lambda_48
cat("Probability of having 48 to 50 customers:",total_prob_48to50)

#Ques 4
N <- 250
D <- 17
n <- 5

p_x_3 = dhyper(3,D,N-D,n)
p_x_3

#Ques 5
#part (a)
n <- 31         # Sample size
p_success <- 0.447   # Probability of success (using Wikipedia)

# X follows a binomial distribution
X_distribution <- dbinom(0:n, n, p_success)

X_distribution

#part (b)
#Plotting the PMF
plot(0:n, X_distribution, type="h", ylim=c(0, max(X_distribution)),
     xlab="Number of students using Wikipedia as a source", 
     ylab="Probability", main="PMF of X")

#part (c)
# Cumulative distribution function
X_cumulative <- pbinom(0:n, n, p_success)

# Plotting the CDF
plot(0:n, X_cumulative, type="s", ylim=c(0, 1),
     xlab="Number of students using Wikipedia as a source", 
     ylab="Cumulative Probability", main="CDF of X")

#part (d)
# Calculate mean, variance, and standard deviation
mean_X <- n * p_success
var_X <- n * p_success * (1 - p_success)
sd_X <- sqrt(var_X)

mean_X
var_X
sd_X
