library(readxl)
library(mt4608)
library(sampling)
library(data.table)
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
cat('Variance of taubar is: ' , format(round(vartau, 2), nsmall = 2))

#95% CI for Sample Mean
#n is less than 50 so approximate using t-dist (34 dof)
alpha <- 0.05
tcrit <- qt(1-(alpha/2), df=n-1)
muCI <- ybar +c(-1,1)*tcrit*sqrt(varybar)
muCI

#95% CI for total population
popCI <- tau_bar + c(-1,1)*tcrit*sqrt(vartau)
floor(popCI)

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

### Going to check variance between the days to see whether stratified sampling applicable

pildat2 <- read_excel('pildat2.xls')
dailymean <- c(mean(pildat2$Monday, na.rm = TRUE), mean(pildat2$Tuesday, na.rm = TRUE), mean(pildat2$Wednesday, na.rm = TRUE), mean(pildat2$Thursday, na.rm = TRUE), mean(pildat2$Friday, na.rm = TRUE))

dailyn <- c()
dailyN <- 36
dailys2 <- c(0,0,0,0,0)
for (i in 3:7) {
	ni <- length(which(pildat2[,i] > 1)) 
	dailyn <- c(dailyn, ni)
	dailys2[i-2]<-sum((pildat2[,i]-dailymean[i-2])**2, na.rm=TRUE)/(dailyn[i-2]-1)
}
dailyvar <- (dailyN**2)*dailys2*(1-dailyn/37)
sum(dailyvar)



#so we cannot make valid judgment on whether stratifying the days would be suitable as variance on each day larger than total variance

#Now going to check whether forming strata on time of day would work (Morning vs afternoon)
timepil <- read_excel('pilot.xls')

mornsumy <- sum(timepil[c(2:10),c(3:7)], na.rm=TRUE)
morn_n <- length(which(timepil[c(2:10),c(3:7)] > 0))
morn_ybar <- mornsumy/morn_n
cat('The Sample Mean from the Morning Data is: ', morn_ybar)

noonsumy  <- sum(timepil[c(12:36),c(3:7)], na.rm=TRUE)
noon_n <- length(which(timepil[c(12:36),c(3:7)] > 0))
noon_ybar <- noonsumy/noon_n
cat('The Sample Mean from the afternoon Data is: ', noon_ybar)

morns2 <- sum(((timepil[c(2:10),c(3:7)] - morn_ybar)**2), na.rm=TRUE)/(morn_n - 1)
mornN <- 24*5
mornf <- morn_n/mornN
mornvar <- (mornN**2)*(1-mornf)*morns2/morn_n

noons2 <- sum(((timepil[c(12:36),c(3:7)] - noon_ybar)**2), na.rm=TRUE)/(noon_n - 1)
noonN <- 48*5
noonf <- noon_n/noonN
noonvar <- (noonN**2)*(1-noonf)*noons2/noon_n

totalvar <- noonvar + mornvar

vartable <- data.table(Morn_Var = mornvar,
                       Noon_Var = noonvar,
                       Strat_Time_Var = totalvar,
                       Strat_Day_Var = sum(dailyvar),
                       Tau_Var = vartau)

vartable

