## Load sample data
dataset<- get_dataset

## Print Head
head(dataset)

## show random 10 rows from subset
dataset[sample(nrow(dataset), 10), ]

## C01 - Count of Churn detected customers
count_churn_Y<- count_churn_Y(dataset)
count_churn_Y

## C02 - Count of customers without Churn detection
count_churn_N<- count_churn_N(dataset)
count_churn_N

## C3 - Average value of Customer Service Calls (indicator #1)
avg_serv_call<- avg_serv_call(dataset)
avg_serv_call

## C4 - Sum of outboud charges (domestic - day/night, international) (value #1)
revenue_start<- revenue_start(dataset)
revenue_start

## C5 - Calculated lost value (churn)
revenue_churn<- revenue_churn(dataset)
revenue_churn

## C6 - Calculated revenue - churn
revenue_end<- revenue_end(dataset)
revenue_end

## C7 - Churn value in percent, decimals
revenue_churn_perc<- revenue_churn_perc(dataset,1)
revenue_churn_perc

## C8 - Function (count service calls)
churn_SC30<- churn_SC3(dataset,3)
churn_SC31<- churn_SC3(dataset,4)
churn_SC32<- churn_SC3(dataset,5)
churn_SC33<- churn_SC3(dataset,6)
churn_SC34<- churn_SC3(dataset,7)
churn_SC35<- churn_SC3(dataset,8)
churn_SC36<- churn_SC3(dataset,9)
## Number of Service Calls
calls_3<- c(3,4,5,6,7,8,9)
## Probability to leave
prob_3<- c(5,8,15,26,42,68,98)
group_3<- c(churn_SC30,churn_SC31,churn_SC32,churn_SC33,churn_SC34,churn_SC35,churn_SC36)
df_3<- data.frame(calls_3,group_3,prob_3)
chart_3<- ggplot(data = df_3) + geom_line(aes(x = calls_3, y = prob_3)) + geom_area(aes(x = calls_3, y = group_3),alpha=0.2,fill="#AACC00") + scale_y_log10()
chart_3

## C9 - Function (sum of minutes under limit)
churn_SC4<- churn_SC4(dataset,200)
churn_SC4

churn_SC50<- churn_SC5(dataset,3)*.05
churn_SC51<- churn_SC5(dataset,4)*.08
churn_SC52<- churn_SC5(dataset,5)*.15
churn_SC53<- churn_SC5(dataset,6)*.26
churn_SC54<- churn_SC5(dataset,7)*.42
churn_SC55<- churn_SC5(dataset,8)*.68
churn_SC56<- churn_SC5(dataset,9)*.98
## Number of Service Calls
calls_5<- c(3,4,5,6,7,8,9)
group_5<- c(churn_SC50,churn_SC51,churn_SC52,churn_SC53,churn_SC54,churn_SC55,churn_SC56)
df_5<- data.frame(calls_5,group_5)
chart_5<- ggplot(data = df_5) + geom_line(aes(x = calls_5, y = group_5))
chart_5