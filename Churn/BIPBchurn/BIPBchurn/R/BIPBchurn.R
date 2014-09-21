## Dataset Load
get_dataset = read.csv("dataset/BIPBchurnTraining.csv")

## Calculations

## C01 - Count of Churn detected customers
count_churn_Y <- function(source) {
  count(source, "churn")$freq[1]
}

## C02 - Count of customers without Churn detection
count_churn_N <- function(source) {
  count(source, "churn")$freq[2]
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

## C07 - Churn value in percent, decimals
revenue_churn_perc<- function(source,decimal) {
  tVar1<- round((revenue_start(source)/revenue_end(source))-1, digits=2+decimal)
  paste0(comma(tVar1 * 100), "%")
}

## C08 - Count of Service Calls >= x
churn_SC3 <- function(source,calls) {
  data.frame(table(source$number_customer_service_calls >= calls))$Freq[2]
}

## C09 - Sum of minutes under limit <= x
churn_SC4 <- function(source,minutes) {
  data.frame(table(dataset$total_day_minutes + dataset$total_night_minutes <= minutes))$Freq[2]
}

## C10 - Revenue lost vs. count of SC
churn_SC5 <- function(source,calls) {
  tVar2<- subset(dataset, number_customer_service_calls >= calls)
  sum(tVar2$total_day_charge + tVar2$total_night_charge + tVar2$total_intl_charge)
}