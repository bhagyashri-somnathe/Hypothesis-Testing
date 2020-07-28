# Question 1
cutlets <- (read.csv(file.choose())) # cutlets.csv
View(cutlets)
attach(cutlets)
str(cutlets)

### Normality test ###

shapiro.test(Unit.A) # p value is 0.32 > 0.05 i.e. data is normally distributed

shapiro.test(Unit.B) # p value is 0.5225 > 0.05 i.e. data is normally distributed

### Variance Test ###

var.test(Unit.A,Unit.B)

### 2 Sample t Test ###

t.test(Unit.A,Unit.B,alternative ="two.sided",conf.level = 0.95,correct = TRUE)

## alternative ="two.sided" this is for checking mean is equal or not equal


## Question 2 ###

LabTAT <- read.csv(file.choose())
View(LabTAT)
str(LabTAT)
stacked_Data <- stack(LabTAT) ## using stack function to arrange all data in one column so that we do not have to check normality for each lab.
View(stacked_Data)
attach(stacked_Data)

### Normality Test ###
install.packages("nortest")
library(nortest)
ad.test(stacked_Data$values)

### Variance Test ###

install.packages("car")
## install.packages("inferr")
## library(inferr)
library(car)
leveneTest(stacked_Data$values ~ stacked_Data$ind,data = stacked_Data)


### ANOVA Test ###
 Anova_result <- aov(values~ind,data = stacked_Data)
 summary(Anova_result)
 ## p value is > 0.05 so accept null Hypothesis and there is no significant diff in avg.TAT of all lab.
 
 ### Question 3 ###
 
 buyer_ration <- read.csv(file.choose())
 View(buyer_ration)
 stacked_Data <- stack(buyer_ration)
 View(stacked_Data)
attach(stacked_Data)
table(values,ind) 
chisq.test(table(values,ind))
## pvalue 0.2931 is greater than 0.05 so buyer rations of male and female are same.

### Question 4 ###

customer_orderform <- read.csv(file.choose())
View(customer_orderform)
df <- data.frame(col=c("true" ,"false", "true"))
df
stacked_orderform <- stack(customer_orderform)
View(stacked_orderform)

### Question 5 ###

faltoons <- read.csv(file.choose())
View(faltoons)
#stacked1_data <- stack(faltoons)
attach(faltoons)
#table1 <- table(faltoons$Weekdays,faltoons$Weekend)
table1 <- table(Weekdays,Weekend)
table1
table(Weekdays)
table(Weekend)
prop.test(x=c(66,47),n=c(113,167),conf.level = 0.95,correct =TRUE,alternative = "two.sided",)
 # p value is less than 0.05 so Ha is accepted 
prop.test(x=c(66,47),n=c(113,167),conf.level = 0.95,correct = TRUE,alternative = "greater")
# p value is less than 0.05 Ha is accepted

qnorm(0.025)
