## In this project, we will be simulating and solving claims triangles.

install.packages("tidyverse")
install.packages("ChainLadder")
install.packages("here")
install.packages("tinytex")

library(tidyverse)
library(ChainLadder)
library(here)
library(tinytex)

## ChainLadder comes with several datasets included, most already in claim triangle form
data(ABC)
ABC

plot(RAA/1000,  main = "Claims development by origin year", lattice = TRUE)

## plot makes it questionable whether there is a consistent set of weights
## that make variance constant between observations, as is required for
## the Mack model, at least for claims prior to 1981

## ChainLadder also comes with convenient cumulative increment functions for easy
## calculations

abc.inc <- cum2incr(ABC)

abc.inc[1,]

## First we need to calculate the age to age factors, dividing the sum of each vector
## by the sum of the next year

Age2AgeCalculator <- function(data = ABC) {

    size <- dim(data)
    len <- size[1] - 1
    a2a <- sapply(1:len,
              function(i){
                  sum(data[c(1:(size[1] - i)), i+1])/sum(data[c(1:(size[1] - i)),i])
              })
    return(a2a)
}

devRat <- Age2AgeCalculator() # now returns vector of development ratios

## regressing the log of our development ratios on time gives us
## the log linear extrapolation of age to age effects

LogLinearExtrapolation <- function(data = ABC) {

    ## firstly we can regress the devellopment ratios on time
    dataSize <- dim(data)[1]
    regLine <- lm(log(devRat-1) ~ c(1:(dataSize-1)))

    ## next we can ask ggplot2 to put these up
    df <- data.frame(log(devRat-1), c(1:(dataSize-1)))
    colnames(df) <- c("Development Ratios from Age to Age Calculator", "Development Years")
    df

    plt <- ggplot(data = df, aes(x = `Years from Start`, y = `Development Ratios from Age to Age Calculator`))
    plt <- plt + geom_point() + geom_smooth(method = "lm", se = FALSE)
    plt <- plt + labs(title = "Log-Linear extrapolation of Age-to-Age effects")
    plt
    ## somewhat questionable whether the relationship is linear or would be
    ## better approximated with a polynomial

    ## finally we can make projections for the future to see how long it will take
    ## before old claims stop being paid out
    modelCoef <- coef(regLine)
    projections <- exp(modelCoef[1] + c(dataSize:(dataSize+100))*modelCoef[2]) + 1

    return(projections)
}



## we have now extrapolated a value for the age to age factors
projections <- LogLinearExtrapolation()
ProdProj <- prod(projections)
ProdProj


## putting up our projections

## this variable contains the projections for expected claims over time
## until 99.9999% of claims are paid out 
DevCulmProd <- 100*(rev(1/cumprod(rev(c(devRat, projections[projections>1.0001])))))
ClaimProj <- tibble(DevCulmProd, 1:length(DevCulmProd))
colnames(ClaimProj) <- c("Cultimative Development Projections", "Development Years")
plt <- ggplot(data = ClaimProj, aes(x = `Development Years`, y = `Cultimative Development Projections`))
plt <- plt + geom_smooth(se = FALSE) + geom_point() + labs(title = "Estimated Claims projections")
plt


## we can now feed the projected claims back into the original claims table
ProjectedLosses <- function(data = ABC) {

    dataDimension <- dim(data)[1]
    devRat <- c(devRat, ProdProj)
    fullClaims <- cbind(data, Total = rep(0, dataDimension))

    for (k in 1:dataDimension) {
        fullClaims[(dataDimension-k+1):dataDimension, k+1] <- fullClaims[(dataDimension-k+1):dataDimension,k]*devRat[k]
    }

    UnpaidLosses <- sum(fullClaims[ ,(dataDimension+1)] - getLatestCumulative(data))
    print("The total expected unpaid losses are:")
    print(UnpaidLosses)


    return(fullClaims)
}

extrapolatedClaims <- ProjectedLosses()

round(extrapolatedClaims)

## Completing the Loss Development Method with:
UnpaidLosses <- sum(extrapolatedClaims[ ,((dim(ABC)[1])+1)] - getLatestCumulative(ABC))
sprintf("The total expected unpaid losses are %4.2f", UnpaidLosses)


## we can use Mack's model in ChainLadder to get robust estimates of
## different loss development factors, assumptions elaborated on in pdf

## Mack model is functionally a weighted linear autoregression of claims

mackmodel <- MackChainLadder(ABC, est.sigma = "Mack") # ChainLadder handles the Mack model on its own

mackmodel$f # loss development factors (taper off after 10 years here)

mackmodel$FullTriangle # full triangle of projected losses for ABC

summary(mackmodel) # shows us the key totals in the model, including ultimate loss

plot(mackmodel) # unfortunately the residuals here show clear increasing complex time trends, suggesting Mack assumptions aren't satisfied

## we can make weights for only the last 5 years of data in claims
calPeriods <- (row(ABC) + col(ABC) - 1)
(weights <- ifelse(calPeriods <= 5, 0, ifelse(calPeriods > 10, NA, 1)))

mackmodel5 <- MackChainLadder (ABC, weights, est.sigma = "Mack")
mackmodel5
plot(mackmodel5) #while this lightly reduced residual trends over development years, it seems more
## effort would be necessary to deal with trends between years.
    

## claims <- tibble(read.csv(here("data", "insurance_claims.csv")))
## claims
