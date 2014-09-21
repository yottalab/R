pkgname <- "BIPBchurn"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('BIPBchurn')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BIPBchurn-package")
### * BIPBchurn-package

flush(stderr()); flush(stdout())

### Name: BIPBchurn-package
### Title: BIPBchurn - churn function, calculation, prediction
### Aliases: BIPBchurn-package BIPBchurn
### Keywords: package

### ** Examples

## Load sample data
dataset<- get_dataset

## Print Head
head(dataset)

## show random 10 rows from subset
dataset[sample(nrow(dataset), 10), ]

## Average value of Customer Service Calls (indicator #1)
avg_serv_call<- avg_serv_call(dataset)
avg_serv_call

## Sum of outboud charges (domestic - day/night, international) (value #1)
revenue_start<- revenue_start(dataset)
revenue_start

## Calculated lost value (churn)
revenue_churn<- revenue_churn(dataset)
revenue_churn

## Calculated revenue - churn
revenue_end<- revenue_end(dataset)
revenue_end

## Churn value in percent
revenue_churn_perc<- revenue_churn_perc(dataset)
revenue_churn_perc

## Count of Churn detected customers
count_churn_Y<- count_churn_Y(dataset)
count_churn_Y

## Count of customers without Churn detection
count_churn_N<- count_churn_N(dataset)
count_churn_N



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
