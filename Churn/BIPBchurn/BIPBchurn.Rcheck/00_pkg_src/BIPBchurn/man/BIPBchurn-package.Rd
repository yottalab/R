\name{BIPBchurn-package}
\alias{BIPBchurn-package}
\alias{BIPBchurn}
\docType{package}
\title{
BIPBchurn - churn function, calculation, prediction
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab BIPBchurn\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2014-06-16\cr
License: \tab Free \cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Jan Rock, BIPB Ltd.

Maintainer: Jan Rock (jan.rock@bipb.com)
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
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
}
