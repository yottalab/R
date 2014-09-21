# package C50
library(C50)
library(plyr)

data(churn)
rm(churnTest)
head(churnTrain)

## C50 test
treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn,
                  control = C5.0Control(winnow = TRUE))
summary(treeModel)

# show random 10 rows from subset
churnTrain[sample(nrow(churnTrain), 10), ]

## my rules

## Average value of Customer Service Calls (indicator #1)
churn_rule_1 <- function(source) {
  mean(source$number_customer_service_calls)
}
avg_serv_call <- churn_rule_1(churnTrain)
## print
avg_serv_call

## Sum of outboud charges (domestic - day/night, international) (value #1)
churn_rule_2 <- function(source) {
  sum(source$total_day_charge + source$total_night_charge + source$total_intl_charge)
}
sum_charge <- churn_rule_2(churnTrain)
## print
sum_charge

## Count of Churn detected customers
churn_rule_3 <- function(source) {
  chr3<- count(churnTrain, "churn")
  chr3yes<- chr3$freq[1]
}
count_churn_Y <- churn_rule_3(churnTrain)
## print
count_churn_Y

## Count of customers without Churn detection
churn_rule_4 <- function(source) {
  chr4<- count(churnTrain, "churn")
  chr4yes<- chr4$freq[2]
}
count_churn_N <- churn_rule_4(churnTrain)
## print
count_churn_N

## Sum - Churn
sum_charge - (count_churn_Y * (sum_charge / (count_churn_Y + count_churn_N)))