n <- 100
df <- n - 1
data <- rt(n, df)
hist(data, main="t-distribution Histogram", xlab="Value", ylab="Frequency")

n <- 100
df_values <- c(2, 10, 25)
for (df in df_values) {
  data <- rchisq(n, df)
  hist(data, main=paste("Chi-Square df =", df, "Histogram"), xlab="Value", ylab="Frequency")
}

x <- seq(-6, 6, length.out = 100)
df_values <- c(1, 4, 10, 30)

for (df in df_values) {
  y <- dt(x, df)
  lines(x, y, col=rainbow(length(df_values)))
}

legend("topright", legend=paste("df =", df_values), col=rainbow(length(df_values), start = 0.7), lty=1)

v1 <- 10
v2 <- 20
percentile_95 <- qf(0.95, v1, v2)
print(paste("95th percentile:", percentile_95))

v1 <- 10
v2 <- 20
area_1_5 <- pf(1.5, v1, v2)
area_greater_1_5 <- 1 - area_1_5
print(paste("Area under the curve for [0, 1.5]:", area_1_5))
print(paste("Area under the curve for [1.5, +∞):", area_greater_1_5))

v1 <- 10
v2 <- 20
quantiles <- qf(c(0.25, 0.5, 0.75, 0.999), v1, v2)
print(paste("Quantiles for probabilities 0.25, 0.5, 0.75, and 0.999:", quantiles))

v1 <- 10
v2 <- 20
n <- 1000
data <- rf(n, v1, v2)
hist(data, main="F-distribution Histogram", xlab="Value", ylab="Frequency")
