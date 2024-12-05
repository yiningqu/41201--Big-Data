####### donohue and levitt 2001/2008: abortion and crime

## example reading non csv data: this is a dump from STATA
## skip says skip the first line of the file, sep="/t" says 'tab separated'

data <- read.table("abortion.dat", skip=1, sep="\t")

names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')


## y_viol  log of violent crime per capita
## y_prop  log of property crime per capita
## y_murd  log of murder crime per capita
## year  : only years 85 to 97

## prison: log of lagged prisoners per capita (t-1)
## police: the log of lagged police per capita (t-1)
## ur: the unemployment rate
## inc: per-capita income
## pov: the poverty rate
## AFDC: lagged state welfare generosity (at year t-15)
## gun: dummy for concealed weapons law
## beer: beer consumption per capita 

data <- data[!(data$state%in%c(2,9,12)),] # AK, FL, HI are strange places

data <- data[data$year>84 & data$year<98,] # incomplete data outside these years

data$pop <- log(data$pop)

t <- data$year-85

t2 <- t^2

s <- factor(data$state) ## the states are numbered alphabetically

controls <- data.frame(data[,c(3,10:17)])

## y is log crime rate, a is abortion rate as described below
## note we also have violent and property crime versions

y <- data$y_murd

d <- data$a_murd

# Plot crime rate in each state


first<-1

index<-as.numeric(levels(as.factor(data$state)))

par(mfrow=c(1,2))

for (i in index){
	
	sub<-subset(data,data$state==i)

	if (first==1){ 
		
		plot(y_murd~year,type="l",data=sub,ylim=c(min(data$y_murd),max(data$y_murd)))
		
				first<-0
		
		} else{

		points(y_murd~year,type="l",data=sub)

	
		}
	title("log-crime rate")
}


first<-1

for (i in index){
	
	sub<-subset(data,data$state==i)

	
	if (first==1){ 
		
		
		plot(a_murd~year,type="l",data=sub,ylim=c(min(data$a_murd),max(data$a_murd)))
	
		first<-0
		
		} else{

	
		points(a_murd~year,type="l",data=sub)
		

		}
		title("abortion rate")

}



## The abortion 'a_' variables are weighted average of abortion rates where
## weights are determined by the fraction of the type of crime committed by
## various age groups. For example, if 60% of violent crime were committed by 18
## year olds and 40% were committed by 19 year olds in state i, the abortion rate
## for violent crime at time t in state i would be constructed as .6 times the
## abortion rate in state i at time t − 18 plus .4 times the abortion rate in
## state i at time t − 19. See Donohue and Levitt (2001) for further detail.

## we'll just look at murder
## note for convenience here I've made y,d,t, global: they are not in controls.

summary(orig <- glm(y ~ d+t+s+., data=controls) )$coef['d',]

## this is the levitt analysis: higher abortion leads to lower crime

## Now the same analysis, but for cellphones rather than abortion

cell <- read.csv("us_cellphone.csv")

# center on 1985 and scale by 1997-1985

cellrate <- (cell[,2]-cell[1,2])/(cell[13,2]-cell[1,2]) 

## what if we're just fitting a quadratic trend?
## there are many things that increased with similar shapes over time
## (cellphone usage, yoga revenues, home prices, ...)

plot(1985:1997, tapply(d, t, mean), xlab="year", ylab="adjusted rate", pch=21, bg=2)

points(1985:1997, cellrate, bg=4, pch=21)

legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
phone <- cellrate[t+1]

## clearly, cellphones fight crime.

summary(tech <- glm(y ~ phone+t+s+., data=controls))$coef['phone',]

## what is happening here is that murder has been decreasing quadratically,
## and we have no other controls that do so.  To be correct, you need
## to allow quadratic trends that could be caused by other confounding variables (e.g. technology)

## each state should also have a different baseline linear and quadratic trend
## and, at very least, controls should interact with each other.
## we also allow the control effects to change in time (interact with t+t2)
# (no intercept, since we've removed the reference level from state)

summary(orig2 <- glm(y ~ d+t+t2+s+phone+., data=controls))$coef['d',] 



summary(interact <- glm(y ~ d + (s + .^2)*(t+t2), data=controls))$coef['d',] 

## Abortion is no longer significant.

dim(model.matrix(y ~ d + (s + .^2)*(t+t2), data=controls))

## we have very few observations relative to number of parameters.

## so we need a way to select only important controls
## try using a lasso 

library(gamlr)

## refactor state to have NA reference level

s <- factor(s, levels=c(NA,levels(s)), exclude=NULL)

x = sparse.model.matrix(~ (s + .^2)*(t+t2), data=controls)[,-1]

dim(x)

## NAIVE LASSO regression

# Naive LASSO adds "treatment" as an extra covariate without giving it any special attention

naive <- gamlr(cbind(d,x),y)

coef(naive)["d",] # effect is AICc selected <0

# this is the effect of treatment (abortion), given everything else that LASSO keeps in the model
# the "everything else", however, might not include all the confounders :(


## now, we do double LASSO to fix this

# FIRST STAGE:

# do LASSO of treatment on confounders

treat <- gamlr(x,d,lambda.min.ratio=1e-4)

plot(treat) # there are some x's predictive of trestment

# Now, grab the predicted treatment
# type="response" is redundant here (gaussian), 
# but you'd want it if d was binary

# we isolate dhat (the part of treatment that we can predict with x's)

dhat <- predict(treat, x, type="response") 

## not much signal in d not predicted by dhat

plot(dhat,d,bty="n",pch=21,bg=8) 

# it seems that abortion does not have much extra information on top of x

## IS R^2?

cor(drop(dhat),d)^2

## Note: IS R2 is what governs how much independent signal
## you have for estimating 

# SECOND STAGE: 

# do lasso of outcome (crime) on treatment (d), predicted treatment (dhat) and predictors x

causal <- gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-4)

# free=2 is needed because we need to make sure that dhat always stays in the model, i.e. we free it from LASSO shrinkage

# dhat separates d from x, now the effect of d will be "pure" (isolated from x)

coef(causal)["d",] # AICc says abortion has no causal effect.

# no extra effect it turns out


## BOOTSTRAP 

n <- nrow(x)

## Bootstrapping our lasso causal estimator is easy

gamb <- c() # empty gamma

for(b in 1:20){
	## create a matrix of resampled indices

	ib <- sample(1:n, n, replace=TRUE)

	## create the resampled data

	xb <- x[ib,]

	db <- d[ib]

	yb <- y[ib]

	## run the treatment regression

	treatb <- gamlr(xb,db,lambda.min.ratio=1e-3)

	dhatb <- predict(treatb, xb, type="response")

	fitb <- gamlr(cbind(db,dhatb,xb),yb,free=2)

	gamb <- c(gamb,coef(fitb)["db",])

	print(b)
}

## not very exciting though: all zeros

summary(gamb) 

## it's saying there's near zero chance AICc selects gamma!=0

# Simple bootstrap

data(airquality)

laq <- log(airquality[,1:4]) 

mle <- glm(Ozone ~ Solar.R+Wind+Temp, data=laq) 

gamma <- c()

n <- nrow(airquality)

for(b in 1:100){

	ib<-sample(1:n,n,replace=TRUE)
	fb <- glm(Ozone ~ Solar.R+Wind+Temp, data=laq, 
					subset=ib) 
	gamma <- c(gamma,coef(fb)["Temp"]) 
	
	} 

hist(gamma,freq=FALSE)

se<-summary(mle)$coef[4,2]

abline(v=coef(mle)["Temp"], col=2,lwd=2)

# Confidence interval from R output

abline(v=coef(mle)["Temp"]+2*se,col=4,lwd=2)

abline(v=coef(mle)["Temp"]-2*se,col=4,lwd=2)

# Confidence interval from bootstrap

abline(v=quantile(gamma,0.025),col=3,lwd=2)

abline(v=quantile(gamma,0.975),col=3,lwd=2)

# get a standard error from Bootstrap

sd(gamma)
mean(gamma)+2*sd(gamma)
mean(gamma)-2*sd(gamma)

se   # very similar