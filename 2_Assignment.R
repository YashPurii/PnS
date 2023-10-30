#Ques 1
#part a
chest <- c(rep("gold",20),rep("silver",30),rep("bronze",50))
smpl_space <- sample(chest,size=10,replace = FALSE)
smpl_space
#part b
outcomes <-c("success","failure")
chances <- c(0.9,0.1)

sample_sp <- replicate(10,sample(outcomes,1,replace = TRUE, prob = chances))
print(sample_sp)

#Ques 2
n_values <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
num_simulations <- 10000
probabilities <- numeric(length(n_values))

for (i in seq_along(n_values)) {
  matches <- replicate(num_simulations, any(duplicated(sample(1:365, n_values[i], replace = TRUE))))
  probabilities[i] <- sum(matches) / num_simulations
}

result_df <- data.frame(N = n_values, Probability = probabilities)
print(result_df)

# Smallest n for P(match) > 0.5
smallest_n <- min(result_df$N[result_df$Probability > 0.5])
print(smallest_n)

#Ques 3
cond_prob <- function(p_A, p_B_given_A, p_B)
{
  p_A_given_B <- (p_A*p_B_given_A)/p_B;
  return(p_A_given_B)
}

p_cloudy = 0.4
p_rainy_given_cloudy =0.85
p_rain = 0.2

p_clouds_given_rain = cond_prob(p_rain,p_rainy_given_cloudy,p_cloudy)
p_clouds_given_rain
#Ques 4
data("iris")

print(head(iris))

#structure of attribuest in the Iris data set
str(iris)

range_ds <- range(iris$Sepal.Length)
range_ds

mean_splen<- mean(iris$Sepal.Length)
mean_splen

median_splen<- median(iris$Sepal.Length)
median_splen

quar_splen <- quantile(iris$Sepal.Length, probs = c(0.25,0.75))
int_quar <- quar_splen[2] - quar_splen[1]
int_quar

std_splen<- sd(iris$Sepal.Length)
var_splen<- var(iris$Sepal.Length)
std_splen
var_splen

summary(iris)
#Ques 5
getMode <- function(v)
{
  uniq <- unique(v)
  uniq [which.max(tabulate(match(v,uniq)))]
}

v <- c(2,1,3,5,1,7,5,5,2,4,2,7)
mode_d <- getMode(v)
mode_d
