## Calculating population mean and variance of temp

df <- read.csv("/Users/sophiahu/Downloads/STAT440 Data Winter.csv")
temp <- df[1:720,5]
var(temp)
mean(temp)

## using SRS without replacement with n1-n5 to estimate y-bar
N = 720 

## calculations for n=498
n1 = 498
SRS1 <- sample(temp,n1,replace= FALSE)
sum1 <- sum(SRS1)
sum1

ybar1 <- (1/n1)*sum(SRS1)
ybar1

var1 <- var(SRS1)
var1

varhatybar1 = ((N-n1)/N)*(var1/n1)
varhatybar1

## calculations for n=60 
n2 = 60
SRS2 <- sample(temp,n2,replace= FALSE)
sum2 <- sum(SRS2)
sum2
ybar2 <- (1/n2)*sum(SRS2)
ybar2

var2 <- var(SRS2)
var2

varhatybar2 = ((N-n2)/N)*(var2/n2)
varhatybar2

## calculations for n=16
n3 = 16
SRS3 <- sample(temp, n3, replace=FALSE)
sum3 <- sum(SRS)
sum3

ybar3 <- (1/n3)*sum(SRS3)
ybar3

var3 <- var(SRS3)
var3

varhatybar3 = ((N-n3)/N)*(var3/n3)
varhatybar3

## calculations for n=549
n4 = 549
SRS4 <- sample(temp, n4, replace=FALSE)
sum4 <- sum(SRS4)
sum4

ybar4 <- (1/n4)*sum(SRS4)
ybar4

var4 <- var(SRS4)
var4

varhatybar4 = ((N-n4)/N)*(var4/n4)
varhatybar4

## calculations for n=82
n5 = 82
SRS5 <- sample(temp, n5, replace=FALSE)
sum5 <- sum(SRS5)
sum5

ybar5 <- (1/n5)*sum(SRS5)
ybar5

var5 <- var(SRS5)
var5

varhatybar5 = ((N-n5)/N)*(var5/n5)
varhatybar5


## calculations for n=23
n6 = 23
SRS6 <- sample(temp, n6, replace=FALSE)
sum6 <- sum(SRS6)
sum6

ybar6 <- (1/n6)*sum(SRS6)
ybar6

var6 <- var(SRS6)
var6

varhatybar6 = ((N-n6)/N)*(var6/n6)
varhatybar6

# Finding the CI using t (n-1 DF)

n1 <- 498 
t1 <- qt(0.05, n1-1, lower.tail=FALSE)
lower_bound1 <- 39.01426 - t1*sqrt(0.060366)
upper_bound1 <- 39.01426 + t1*sqrt(0.060366)


n2 <- 60
t2 <- qt(0.05, n2-1, lower.tail=FALSE)
lower_bound2 <- 39.04000 - t2*sqrt(1.159113)
upper_bound2 <- 39.04000 + t2*sqrt(1.159113)

n3 <- 16
t3 <- qt(0.05, n3-1, lower.tail=FALSE)
lower_bound3 <- 38.06875 - t3*sqrt(3.48437)
upper_bound3 <- 38.06875 + t3*sqrt(3.48437)

n4 <- 549
t4 <- qt(0.05, n4-1, lower.tail=FALSE)
lower_bound4 <- 39.31967 - t4*sqrt(0.04022805)
upper_bound4 <- 39.31967 + t4*sqrt(0.04022805)

n5 <- 82
t5 <- qt(0.05, n5-1, lower.tail=FALSE)
lower_bound5 <- 39.07805 - t5*sqrt(1.002576)
upper_bound5 <- 39.07805 + t5*sqrt(1.002576)

n6 <- 23
t6 <- qt(0.05, n6-1, lower.tail=FALSE)
lower_bound6 <- 39.72609 - t6*sqrt(3.635956)
upper_bound6 <- 39.72609 + t6*sqrt(3.635956)

# find d with 100 samples of n=82
rep <- replicate(100, sample(x,size=82, replace=FALSE))
samp_mean <- (1/82)*replicate(100, sum(sample(x,size=82, replace= FALSE)))
diff <- abs(39.177-samp_mean)
diff

sum(diff >1.95885)


























