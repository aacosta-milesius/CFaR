# Prototype of debt instrument risk measurement.
setwd("H:/Development")

Treasury<-read.table("584/Treasury Curves.txt", header=T, skip=2)
portf<-read.table("Portfolio by Year.txt", header=T)

par(mfcol=c(3,2)) # put 6 plots on one page

skewness<-function(x) {
	m<-mean(x)
	me<-median(x)
	s<-sqrt(var(x))
	sk<-(m-me)/s
	return(sk)
}


CIR<-function(k, theta, sigma, steps) {
		# k is speed of mean reversion
		# theta is long-term mean
		# sigma is rate volatility
		# Cox-Ingersoll-Ross keeps rates from being negative: .5*sqrt(ratepath[NROW(ratepath)])
	ratepath<-c(theta+sigma*(rbinom(1,2,.5)-1)) # start at theta + or - sigma

		for(n in 1:steps) {
			ratepath<-c(ratepath, k*(theta-ratepath[NROW(ratepath)]) + 
						sigma*.5*sqrt(ratepath[NROW(ratepath)])*rnorm(1,sd=sqrt(sigma))+ratepath[NROW(ratepath)])
		}
		ratepath
}


vasicek<-function(k, theta, sigma, steps) {
		# k is speed of mean reversion
		# theta is long-term mean
		# sigma is rate volatility
		# The bad thing about Vasicek model, rates can go negative!
	ratepath<-c(theta+sigma*(rbinom(1,2,.5)-1))
	
	for(n in 1:steps) {
		ratepath<-c(ratepath, k*(theta-ratepath[NROW(ratepath)])+sigma*rnorm(1)+ratepath[NROW(ratepath)])
	}
	ratepath
}

# mr.ratepath<-vasicek(.3, 0.05, 0.005, 200)

# Generates a two-dimensional array of 500 rows and [steps] columns
meanrevert<-function(rate.start, volatility, steps) {
	walk<-rbind(CIR(1, mean(Treasury[,'m10.yr']), sd(Treasury[,'m10.yr']), 50))
	
	for(n in 1:499) {
		walk<-rbind(walk, CIR(1, mean(Treasury[,'m10.yr']), sd(Treasury[,'m10.yr']), 50))
	}
	walk<-walk/100
}

# Generates a two-dimensional array of 500 rows and [steps] columns
normwalk<-function(rate.start, volatility, steps) {
	walk<-rbind(cumsum(rnorm(steps, mean=0, sd=volatility)) + rate.start)
	
	for(n in 1:499) {
		walk<-rbind(walk, cumsum(rnorm(steps, mean=0, sd=volatility)) + rate.start)
	}
	walk<-walk/100
}

# Make 500 trials of a 30-year step rate walk
cf_year<-seq(from=2009, to=2068)

# my.paths<-normwalk(4.78, 0.1675, 30)
my.paths<-meanrevert(4.78, sd(Treasury[,'m10.yr']), 30)

plot(100*my.paths[1,], typ='l',col='red', ylab='Rate', xlab='Years', main='Interest Rate Path', ylim=c(2,6))

for(n in 1:499) {
		lines(100*my.paths[n,], typ='l',col=floor(runif(1)*657))
	}

# Create a cash flow change matrix that holds the difference of refinanced cash flows by year & trial
cf_delta=matrix(data=0,nrow=500,ncol=60)

# Rates step every year, then the year 2009 is point 1, and 2038 is point 30 {2038-2008}
rate.year<-portf[,'Maturity']-2008

# Get a BBB corporate bond spread (This cheap hack will do for now)
corp.spread<-rep(.025, 50)

# Compute the cash flow difference for each month of refinanced debt per instrument
for(n in 1:length(portf[,1])) { # Loop through each instrument
	for(sim in 1:500) { # 500 trials
		# The cash flow difference is repeated every year for 'Term' years
		cf_vector<-rep((portf[n,'Coupon']-my.paths[sim, rate.year[n]]-corp.spread[portf[n,'Term']])*
					portf[n,'Debt.Outstanding'], portf[n,'Term'])
	
									
		# Insert the cash flows changes beginning at the term year for each trial
		cf_delta[sim, rate.year[n]:(rate.year[n]+length(cf_vector)-1)] <-
		cf_delta[sim, rate.year[n]:(rate.year[n]+length(cf_vector)-1)] + cf_vector
	}
}

#cf_delta=matrix(data=0,nrow=500,ncol=60)
# Add in floating rate debt that has a principal of 20% of the 5-year sum of fixed.
cf_delta<-cf_delta*.8
floating<-777696738.93
for(n in 1:5) {
	cf_delta[,n]<-cf_delta[,n]+ (my.paths[,n]-mean(my.paths[,n]))*floating
}

# Mini Debt CFaR looks at just 25 years.
colnames(cf_delta)<-cf_year
write.csv(cf_delta[,1:5], "Debt CFaR.csv")

boxplot(cf_delta[,1:5]/1e6, main='CFaR for Bond Portfolio', ylab='Cash Flow Changes $ (in millions)',xlab='Year')
abline(0,0, col='red', lty=4)
abline(-100,0, col='blue', lty=3)

plot(cf_year[1:5],sd(cf_delta[,1:5]/1e6),typ='l', col='red', main='Volatility of Cash Flow',xlab='Year',ylab='Cash Flow Std. Dev. $ (in millions)')

cfar.density<-density(cf_delta[,1:5]/1e6)
plot(cfar.density,main="CFaR Density", xlab="Cash Flow Changes $ (in millions)",col=238)

#cfar.density<-density(cf_delta[,21:40]/1e6)
#plot(cfar.density,main="CFaR Density (Year 21-40)", xlab="Cash Flow Changes $ (in millions)",col=68)

#cfar.density<-density(cf_delta[,41:60]/1e6)
#plot(cfar.density,main="CFaR Density (Year 41-60)", xlab="Cash Flow Changes $ (in millions)",col=339)

hist(cf_delta[,1:5]/1e6, main="CFaR Histogram", xlab="Cash Flow Changes $ (in millions)")
#hist(cf_delta[,21:40]/1e6, main="CFaR Year 21-40", xlab="Cash Flow Changes $ (in millions)")
#hist(cf_delta[,41:60]/1e6, main="CFaR Year 41-60", xlab="Cash Flow Changes $ (in millions)")

#hist(cf_delta/1e6, main="Aggregate CFaR Histogram", xlab="Cash Flow Changes $ (in millions)")

plot(density(cf_delta[,2]/1e6), main='CFaR Distributions', col=2,xlim=c(-50,50))
lines(density(cf_delta[,3]/1e6), col=3)
lines(density(cf_delta[,4]/1e6), col=4)
lines(density(cf_delta[,5]/1e6), col=5)

labels<-c(
	sprintf('2010 ($%6.1fMM)', density(cf_delta[,2]/1e6)$x[512*.05]-density(cf_delta[,2]/1e6)$x[512*.5]),
	sprintf('2011 ($%6.1fMM)', density(cf_delta[,3]/1e6)$x[512*.05]-density(cf_delta[,3]/1e6)$x[512*.5]),
	sprintf('2012 ($%6.1fMM)', density(cf_delta[,4]/1e6)$x[512*.05]-density(cf_delta[,4]/1e6)$x[512*.5]),
	sprintf('2013 ($%6.1fMM)', density(cf_delta[,5]/1e6)$x[512*.05]-density(cf_delta[,5]/1e6)$x[512*.5])
	)
		
legend(10,.12, labels,col=c(2,3,4,5), lty=c(1,1,1,1), bty='o', bg='white')

v<-NULL
for(b in 1:5) {
	ptiles<-quantile(cf_delta[,b], probs=c(.5,.05))
	v<-rbind(v, sprintf("%d CFaR $%7.2f", cf_year[b], ptiles["50%"]-ptiles["5%"]))
}
#print(v)

print(labels)
# END
