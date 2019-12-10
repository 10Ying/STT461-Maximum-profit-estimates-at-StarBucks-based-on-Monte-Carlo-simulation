# 1 comparison for estimation values of cups in two ways

data <- read.table("/Users/ying/Desktop/starbucksrmdandcsvfiles/Starbucks.csv", header = TRUE, sep = ",")
data <- data.frame(data)

cups <- data$cups
employees <- data$employees
morning <- 60
afternoon <- 60
evening <- 54
segments <- morning + afternoon + evening

B <- 10000
cup.estimate <- numeric(B)

morning.estimate <- numeric(B)
afternoon.estimate <- numeric(B)
evening.estimate <- numeric(B)
day.estimate <- numeric(B)

cup.average <- numeric(B)
day.average <- numeric(B)

for(i in 1:B)
{random.cups <- sample(cups, segments, replace = TRUE)

cup.estimate[i] <- sum(random.cups)

morning.random <- sample(cups[1:3], morning, replace = TRUE)
afternoon.random <- sample(cups[4:13], afternoon, replace = TRUE)
evening.random <- sample(cups[14:19], evening, replace = TRUE)

morning.estimate[i] <- sum(morning.random)
afternoon.estimate[i] <- sum(afternoon.random)
evening.estimate [i] <- sum(evening.random)
day.estimate[i] <- morning.estimate[i] + afternoon.estimate[i] + evening.estimate[i]}

mean(cup.estimate)
sd(cup.estimate)
mean(day.estimate)
sd(day.estimate)

# 2 Estimation for profits in two ways 

# The histogram of cups

data <- read.table("/Users/ying/Desktop/starbucksrmdandcsvfiles/Starbucks.csv", header = TRUE, sep = ",")
prices <- read.table("/Users/ying/Desktop/starbucksrmdandcsvfiles/Starbucks_Prices.csv", header = TRUE, sep = ",")
data <- data.frame(data)
prices <- data.frame(prices)
hist(data$cups,breaks = 10,main = ("Histogram of cups”,xlab=“cups"))
     
     # Estimate the revenues in two different ways
     
     cups <- data$cups
     employees <- data$employees
     morning <- 60
     afternoon <- 60
     evening <- 54
     segments <- morning + afternoon + evening
     
     B <- 10000
     cup.estimate <- numeric(B)
     
     morning.estimate <- numeric(B)
     afternoon.estimate <- numeric(B)
     evening.estimate <- numeric(B)
     day.estimate <- numeric(B)
     
     cup.average <- numeric(B)
     day.average <- numeric(B)
     revenue <- numeric(B)
     revenue2 <- numeric(B)
     
     for(i in 1:B)
     {random.cups <- sample(cups, segments, replace = TRUE)
     
     cup.estimate[i] <- sum(random.cups)
     
     morning.random <- sample(cups[1:3], morning, replace = TRUE)
     afternoon.random <- sample(cups[4:13], afternoon, replace = TRUE)
     evening.random <- sample(cups[14:19], evening, replace = TRUE)
     
     morning.estimate[i] <- sum(morning.random)
     afternoon.estimate[i] <- sum(afternoon.random)
     evening.estimate [i] <- sum(evening.random)
     day.estimate[i] <- morning.estimate[i] + afternoon.estimate[i] + evening.estimate[i]}
     
     for(i in 1:B)
     {cups.sold <- sample(prices$prices, cup.estimate[i], replace = TRUE)
     cups2.sold <- sample(prices$prices, day.estimate[i], replace = TRUE)
     revenue[i] <- sum(cups.sold)
     revenue2[i] <- sum(cups2.sold)}
     
     # The t-test tests the difference between two profits
     
     u <- runif(10000, min = 8.15, max = 10.92)
     u2 <- runif(10000, min = 8.15, max = 10.92)
     
     profit <- (2/3)*revenue - u*mean(employees)
     max(profit)                      # obtain the maximum profit
     profit2 <- (2/3)*revenue2 - u2*mean(employees)
     max(profit2)                   # obtain the maximum profit
     
     set.seed(42)
     p1<-hist(profit)
     p2<-hist(profit2)
     plot(p1,col=rgb(0,0,1,1/4),ylim=c(0,2000))
     plot(p2,col=rgb(1,0,0,1/4),ylim=c(0,2000),add=T)
     
     profit.ratio <- profit / profit2
     
     t <- (mean(profit.ratio)-1)/(sd(profit.ratio))
     1-pt(t, df = 173)
     