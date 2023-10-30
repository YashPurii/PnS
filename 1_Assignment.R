#Ques 1
c <- seq(5,30,by=5)
cat("Max element: ",max(c))
cat("Min element: ",min(c))

#Ques 2
# Function to calculate factorial
calculateFactorial <- function(n) {
  if (n < 0) {
    cat("Error: Factorial is undefined for negative numbers.\n")
  } else if (n == 0) {
    return(1)
  } else {
    result <- 1
    for (i in 1:n) {
      result <- result * i
    }
    return(result)
  }
}

# Input from the user
num <- as.integer(readline("Enter a non-negative integer: "))

# Calculate and print the factorial
factorial <- calculateFactorial(num)
if (!is.na(factorial)) {
  cat(paste("The factorial of", num, "is", factorial, "\n"))
}

#Ques 3
#Fibonacci series
fibo <- function(n){
  if (n == 0 || n == 1){
    return(n)
  } else {
    return (fibo(n-1)+fibo(n-2))
  }
}

num <- as.integer(readline("Enter a non-negative integer: "))
cat("Fibonacci required is: ",fibo(num))

#Ques 4
add <- function(a,b){
  return(a+b)
}

subtract <- function(a,b){
  return(a-b)
}

multiply <- function(a,b){
  return(a*b)
}

division <- function(a,b){
  return(a/b)
}

calculator <- function(a,b,ch){
  switch(ch,
         '+' = add(a,b),
         '-' = subtract(a,b),
         '*' = multiply(a,b),
         '/' = division(a,b))
}
n1 <- as.integer(readline("Enter an integer: "))
n2 <- as.integer(readline("Enter an integer: "))
ch <- as.character(readline("Enter an integer: "))

calculator(n1,n2,ch)

#Ques 5
x = seq(1,10)
y = seq(11,20)

plot(x,y,main = "ScatterPlot", xlab = "1 to 10 readings", ylab = "11 to 20 days")

slices <- c(10,20,55,5)
lables <- c("A","B","C","D")
pie(slices,lables)

heights <- c(10, 20, 15, 5)
barplot(heights, names.arg=c("A", "B", "C", "D"), main="Bar Plot Example", xlab="Categories", ylab="Values", col="skyblue")

data <- c(23, 45, 67, 34, 56, 78, 89, 12, 45, 67, 34, 56, 78, 89)
boxplot(data, main="Box Plot Example", xlab="Values", col="lightgreen")

