library(readxl)
library(mt4608)
library(sampling)
rm(list = ls(all.names = TRUE)) 

n=35
N=360
f=n/N

setwd("/Users/joshuacorcoran/Documents/SamplingTheory")
pil_dat <- read_excel('pilotdata.xls')
head(pil_dat)

sumy <- sum(pil_dat[,c(3,4,5,6,7)], na.rm=TRUE)
ybar <- sumy/n
cat('Sample Mean, y bar is: ', ybar)

s2 <- sum(((pil_dat[,c(3,4,5,6,7)])-ybar)**2, na.rm=TRUE)/(n-1) #s2 #sample variance
varybar <- ((1-f)*s2)/n
cat('Variance of y-bar is: ', varybar)

tau_bar <- N*ybar
cat('We estimate that ', floor(tau_bar),' pass Lupos NorthStreet Mon-Fri between 10am-4pm.')

vartau <- (N**2)*(varybar)


#95% CI for Sample Mean
#n is less than 50 so approximate using t-dist (34 dof)
alpha <- 0.05
tcrit <- qt(1-(alpha/2), df=n-1)
muCI <- ybar +c(-1,1)*tcrit*sqrt(varybar)
muCI

#95% CI for total population
popCI <- tau_bar + c(-1,1)*tcrit*sqrt(vartau)
popCI

s <- sqrt(s2)
npop_estim <- ((0.2*tau_bar/tcrit*s*N)**2 + 1/N)


##From the pilot survey we would like to achieve a standard error lower than that of the pilot survey

#standard error = sqrt(s2/n)
#Using eqn from notes (on V0 - (2.16))
#Don't have population variance so will use sample variance 

V0 <- (N**2)*s2*(1/n - 1/N)*0.5 #this will be the desired maximum variance bound to half s.e.
min_n <- N/(1 + (N*V0/(N**2)*s2))
cat('Minimum sample size required to bound variance, to half of that from the pilot, is: ', min_n)

#say we wanted to lower the width of the confidence interval from the pilot survey
pilotB0 <- tcrit*sqrt(vartau) #our current 95%CI is 2*PilotB0 wide
#Eq'n from notes to find sample size to bound CI width by PilotB0 (eqn 2.17) (half that of the pilot survey's width = 2B0)

B0 <- 0.7*pilotB0
min_n <- N/(1+(N*(B0**2)/(4*(N**2)*s2)))
min_n