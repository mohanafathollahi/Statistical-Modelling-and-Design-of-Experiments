result <- "result.txt"
work <- read.csv(result_2)
avg_value <- mean(work$End_time)
avg_value
var_unbias <- var(work$End_time)
n <- max(work$Repetition)
t_value <- qt(0.90, df = n-1)
h <- t_value*(var_unbias/n)^0.5
h_star <- 0.02*avg_value
n_star <- n*(h/h_star)^2
n_star

