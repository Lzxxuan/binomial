## Overview
"binomial" is an R package that implements functions for calculating probabilities of a Binomial random variable, and related calculations such as the probability distribution, the expected value, variance, etc.


`bin_variable()` creates a binomial random variable (of class "binvar")
`bin_probability()` gets a value of probability with a certain combination of trials, successes and probability for a success
`bin_distribution()` displays the dataframe of the binomial distribution
`plot(bin_distribution())` method for a binomial distribution object to plot distribution over successes
`bin_cumulative()` displays the dataframe of the binomial cumulative distribution
`plot(bin_cumulative())` method for a binomial cumulative distribution object to plot cumulative distribution over successes

## Motivation
This package has been developed to illustrate some of the concepts behind the creation of an R package.

## Installation
Install the development version from GitHub via the package "devtools":
```{r}
# development version from GitHub:
#install.packages("devtools") 

# install "cointoss" (without vignettes)
devtools::install_github("Lzxxuan/binomial")

# install "cointoss" (with vignettes)
devtools::install_github("Lzxxuan/binomial", build_vignettes = TRUE)
```
## Usage
```{r}
library(binomial)
# default variable
bin1 <- bin_variable()
bin1

# summary of default variable
bin1 <- bin_variable()
binsum1 <- summary(bin1)
binsum1

# probability of n trials, k successes, prob p
bin_probability()

# binomial distribution
bin_distribution()

# plot of binomial distribution
dis1 <- bin_distribution()
plot(dis1)

# binomial cumulative distribution
bin_cumulative()

# plot of binomial cumulative distribution
dis2 <- bin_cumulative()
plot(dis2)
```
