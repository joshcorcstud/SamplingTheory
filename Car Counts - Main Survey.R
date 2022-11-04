library(readxl)
library(mt4608)
library(sampling)
setwd("/Users/joshuacorcoran/Documents/SamplingTheory")
rm(list = ls(all.names = TRUE)) 


carcounts <- read_xlsx('Car Counts Survey.xlsx')
head(carcounts)

n <- length(which(carcounts[c(2:73),c(2:6)] > 0))
N <- 360
f=n/N

#Calculations of ybar, sample variance, and variance of ybar
sumy <- sum(carcounts[,c(2:6)], na.rm=TRUE)
ybar <- sumy/n
s2 <- sum((carcounts[,c(2:6)]-ybar)**2, na.rm=TRUE)/(n-1)
varybar <- (1-f)*s2/n
cat('Sample mean is (ybar): ', ybar)
cat('Variance for ybar is: ', varybar)


#Above calculations required for point estimate of total population
tau_bar <- N*ybar
totalcars <- floor(tau_bar)
cat('We estimate the total # of cars passing Lupos, North St. between 10am-4pm Mon-Fri as: ', totalcars)
vartaubar <- (N**2)*varybar ### eqn 2.19
cat('Variance of total cars is: ', vartaubar)


# 95% Confidence Interval for Total Population

alpha <- 0.05
normcrit <- abs(qnorm(alpha/2)) #we have a sample size of greater than 50 so use of normal (over t) valid 

CarsCI <- totalcars + floor(c(-1,1)*normcrit*sqrt(vartaubar)) #use the floor as cars are whole units
CarsCI
cat('The 95% Confidence interval for total cars is: ', CarsCI)

Width <- (CarsCI[2]-CarsCI[1])
Width

###Investigating Stratas:
# I would like to see whether splitting the frame into different strata
#will minimise the variance, therefore would suggest a stratified random sample would be suitable 

#strata by time of day (mornng, mid, afternoon)
str_i <- c(1,25,49)
timevars <- c()
for (i in 1:3){ 
  ni <- length(which(carcounts[str_i[i]:(str_i[i]+23), c(2:6)] > 0))
  Ni <- 120
  sumyi <- sum(carcounts[str_i[i]:(str_i[i]+23), c(2:6)], na.rm=TRUE)
  ybari <- sumyi/ni
  s2i <- sum(((carcounts[str_i[i]:(str_i[i]+23), c(2:6)] - ybari)**2), na.rm=TRUE)/(ni - 1)
  taubari <- Ni*ybari
  fi <- ni/Ni
  var <- (Ni**2)*(1-fi)*s2i/ni
  timevars <- c(timevars, var)
}
timevars
sum(timevars)

#strata by day

dayvars <- c()
for (i in 2:6){ 
  ni <- length(which(carcounts[,i] > 0))
  Ni <- 72
  sumyi <- sum(carcounts[,i], na.rm=TRUE)
  ybari <- sumyi/ni
  s2i <- sum(((carcounts[,i] - ybari)**2), na.rm=TRUE)/(ni - 1)
  taubari <- Ni*ybari
  fi <- ni/Ni
  var <- (Ni**2)*(1-fi)*s2i/ni
  dayvars <- c(dayvars, var)
} 
dayvars
sum(dayvars)






