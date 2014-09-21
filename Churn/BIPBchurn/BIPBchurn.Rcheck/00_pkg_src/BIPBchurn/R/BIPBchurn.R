## dependency plyr/scales

## Dataset Load
get_dataset = read.csv("dataset/BIPBchurnTraining.csv")

## Calculations

## C01 - Count of Churn detected customers
count_churn_Y <- function(source) {
  chr3<- count(source, "churn")
  chr3yes<- chr3$freq[1]
}

## C02 - Count of customers without Churn detection
count_churn_N <- function(source) {
  chr4<- count(source, "churn")
  chr4yes<- chr4$freq[2]
}

## C03 - Average value of Customer Service Calls (indicator #1)
avg_serv_call <- function(source) {
  mean(source$number_customer_service_calls)
}

## C04 - Sum of outboud charges (domestic - day/night, international) (value #1)
revenue_start <- function(source) {
  sum(source$total_day_charge + source$total_night_charge + source$total_intl_charge)
}

## C05 - Sum of lost revenue
revenue_churn <- function(source) {
  revenue_start(source) - (count_churn_Y(source) * (revenue_start(source) / (count_churn_Y(source) + count_churn_N(source))))
}

## C06 - Sum revenue after churn lost
revenue_end <- function(source) {
  revenue_start(source) - revenue_churn(source)
}

## C07 - Churn value in percent
revenue_churn_perc<- function(source) {
  percent((revenue_start(source)/revenue_end(source))-1)
}