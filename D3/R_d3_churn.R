cat("\014")
Source <- c("A","A","A","A","A","B","B")
Target <- c("C","D","E","F","B","G","H")
NetworkData <- data.frame(Source,Target)
NetworkData
cat("\014")
d3SimpleNetwork(NetworkData, heigh=300, width=700)

links <- read.csv("links.csv", header = TRUE)
nodes <- read.csv("nodes.csv", header = TRUE)
cat("\014")
d3ForceNetwork(Links = links, Nodes = nodes,
               Source = "source", Target = "target",
               Value = "value", NodeID = "name",
               Group = "group", width = 1280, height = 720,
               opacity = 0.9,zoom = TRUE)

library(RCurl)
URL <- "https://raw.githubusercontent.com/christophergandrud/d3Network/master/JSONdata/flare.json"
Flare <- getURL(URL, ssl.verifypeer = FALSE)

# Convert to list format
Flare <- rjson::fromJSON(Flare)

# Create Graph
cat("\014")
d3Tree(List = Flare, fontsize = 8, diameter = 800)

# package C50
library(C50)
library(plyr)
data(churn)
rm(churnTest)
head(churnTrain)
treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn,
                  control = C5.0Control(winnow = TRUE))
summary(treeModel)

# show random 3 rows
churnTrain[sample(nrow(churnTrain), 10), ]

churn_rule_1 <- function(source) {
    mean(source$number_customer_service_calls)
}
avg_serv_call <- churn_rule_1(churnTrain)
avg_serv_call

churn_rule_2 <- function(source) {
    sum(source$total_day_charge + source$total_night_charge + source$total_intl_charge)
}
sum_charge <- churn_rule_2(churnTrain)
sum_charge

churn_rule_3 <- function(source) {
  chr3<- count(churnTrain, "churn")
  chr3yes<- chr3$freq[1]
}
count_churn_Y <- churn_rule_3(churnTrain)
count_churn_Y

churn_rule_4 <- function(source) {
  chr4<- count(churnTrain, "churn")
  chr4yes<- chr4$freq[2]
}
count_churn_N <- churn_rule_4(churnTrain)
count_churn_N

sum_charge - (count_churn_Y * (sum_charge / (count_churn_Y + count_churn_N)))

EXAMPLE
Company ADG had 500 customers at the beginning of the month and only 450 customers
at the end of the month.  Their customer churn rate would be:
(500-450)/500 = 50/500 = 10%

EXAMPLE
Company ADG had $500,000 MRR at the beginning of the month and only $450,000 MRR at the end.
They also had $65,000 MRR in upgrades that month from existing customers.
Their revenue churn rate would be:
(($500,000 - $450,000) - $65,000)/$500,000 = ($50,000 - $65,000)/$500,000 =
(-$15,000)/$500,000 = -3%
Note the negative revenue churn rate means you actually gained revenue that month!
  
EXAMPLE
Company ADG has 2 product lines:
Basic: 5000 customers, $500/month per customer = $2,500,000 MRR
Premium: 1000 customers, $1250/month per customer = $1,250,000 MRR
This gives them a total of 6,000 customers and $3,750,000 MRR
In a month, 180 Basic customers and 20 premium customers churn.
Customer Churn
(180 + 20)/6,000 = 200/6,000 = 3.33%
Revenue Churn
((180 * $500) + (20 * $1250))/$3,750,000 =
($90,000 + $25,000)/$3,750,000 = $115,000/$3,750,000 = 3.07%

EXAMPLE
Company ADG wants to calculate quarterly churn. 
Month 1: 
1000 customers at the beginning of the month and 50 churn, leaving 950 customers at the end
(refer to this as Cohort A); 100 new sales (refer to this as Cohort B).
Month 2:
Of the 950 customers in Cohort A, another 50 churn, leaving 900; of the 100 in Cohort B,
5 churn, leaving 95; another 100 new sales this month (call this Cohort C)
Month 3:
Of the 900 customers still in Cohort A, another 50 churn, leaving 850; of the 95 customer
in Cohort B, 5 more churn, leaving 90; of the 100 customers in Cohort C, 5 churn, leaving 95.
The summary is:
Cohort A - begins Month 1 with 1000 customers; ends Month 3 with 150
Cohort B - Begins Month 2 with 100 customers; ends Month 3 with 90 customers
Cohort C - Begins Month 3 with 100 customers; ends Month 3 with 95 customers
If we look over the quarter, our initial cohort of 1000 customers only has 850 customers
remaining, giving a customer churn rate of 150/1000 = 15%
During that same time frame, there were 300 new sales, of which 15 churn.  If you included
those 15 churns in your calculation, you'd have 165/1000 = 16.5%

EXAMPLE
Cohort A - 1000 customers; churn rate of 15%
Cohort B - 100 customers; churn rate of 10%
Cohort C - 100 customers; churn rate or 5%
[(1000 * .15) + (100 * .1) + (100 * .05)] / (1000+100+100)=[150 + 10 + 5]/1200 = 165/1200 = 13.75%