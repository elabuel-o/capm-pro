
##-----------------------------------------------------------------------------
## Title: CAPM portfolio analysis
## Author: Armando Enriquez Z.
## Date: November 12th, 2014
## Purpose: Analyze 3 securities returns with the CAPM model
##-----------------------------------------------------------------------------

## R required libraries
library(tikzDevice)

##-----------------------------------------------------------------------------
## Reading the data
## Data of monthly returns (return.csv) and monthly excess return (excess.csv)
## from 3 firm stocks (JNJ, PG and WMT) and Standard and Poor's 500
## (available at yahoo-finance)
mydata <- read.csv("return.csv", header = TRUE)
myexcess <- read.csv("excess.csv", header = TRUE)

names(myexcess) <- c("month", "SP500", "MMM", "JNJ", "PG", "WMT")

##-----------------------------------------------------------------------------
## Descrptive statistics of the excess returns
excess <- myexcess[, -1] ## without months
mean.x <- sapply(excess, mean)
sd.x <- sapply(excess, sd)

tab <- cbind(mean.x, sd.x) ## tabulate the means and sd

##-----------------------------------------------------------------------------
## Regression analysis
## CAPM estimation proper
lm.jnj <- lm(JNJ ~ SP500, data = myexcess)
lm.pg <- lm(PG ~ SP500, data = myexcess)
lm.wmt <- lm(WMT ~ SP500, data = myexcess)

## Computing the beta coefficients
beta.jnj <- lm.jnj$coefficients[2]
beta.pg <- lm.pg$coefficients[2]
beta.wmt <- lm.wmt$coefficients[2]

## Computing the expected excess returns
## First we extract the market mean (mean.sp for Standard & Poors's)
mean.sp <- tab[1, 1]

## According to the formula the expected excess return is beta - mean.sp
expRet.jnj <- beta.jnj * mean.sp
expRet.pg <- beta.pg * mean.sp
expRet.wmt <- beta.wmt * mean.sp

## Computing the risks (variances)
## To compute the residual standard error of the models, just type 
## summary(lm.xxx) for each security
resErr.jnj <- 0.03418
resErr.pg <- 0.03538
resErr.wmt <- 0.04147

## Market standard deviation
## First we extract the market sd (sd.sp for Standard & Poors's)
sd.sp <- tab[1, 2]

## Securities Risks (variances) proper
## According to the formula
risk.jnj <- (beta.jnj)^2 * (sd.sp)^2 + (resErr.jnj)^2
risk.pg <- (beta.pg)^2 * (sd.sp)^2 + (resErr.pg)^2
risk.wmt <- (beta.wmt)^2 * (sd.sp)^2 + (resErr.wmt)^2

##-----------------------------------------------------------------------------
## Make a table with all the values for all the securities (3X3 matrix)
## betas, excess returns and risks
betas <- c(beta.jnj, beta.pg, beta.wmt)
expRets <- c(expRet.jnj, expRet.pg, expRet.wmt)
risks <- c(risk.jnj, risk.pg, risk.wmt)

tab.def <- cbind(betas, expRets, risks)

##-----------------------------------------------------------------------------
## Correlatin plot
## Using the tikz package (for use with LaTeX - TikZ)
tikz(file = "corr.tex", standAlone = TRUE)
pairs(~T.Bills + SP500 + JNJ + PG + WMT, data = mydata)
dev.off()
