# Data analysis

library(readtext)
library(SnowballC)
library(tidytext)

setwd("/Users/yiningqu/Desktop/MIDTERM")

# Let's get to know the data a bit

data<-readtext("RedditNews.csv",skip=1)

date<-data[2] # this is the day of the news

subset<-date=="7/1/16" # let's take a look at news headlines on 7/1/16

data[subset,3] # we have 23 news headlines


# Read the DJIA data

dj<-read.csv("DJIA.csv")

head(dj) # Open price, highest, lowest and close price

ndays<-nrow(dj) # 1989 days



# Read the words

words<-read.csv("WordsFinal.csv",header=F)

words<-words[,1]

head(words)


# Read the word-day pairings

doc_word<-read.table("WordFreqFinal.csv",header=F)

# Create a sparse matrix


library(gamlr)

spm<-sparseMatrix(
		i=doc_word[,1],
		j=doc_word[,2],
		x=doc_word[,3],
		dimnames=list(id=1:ndays,words=words))

dim(spm)

# We select only words at occur at least 5 times

cols<-apply(spm,2,sum)

index<-apply(spm,2,sum)>5

spm<-spm[,index]

# and words that do not occur every day

index<-apply(spm,2,sum)<ndays

spm<-spm[,index]

dim(spm) # we end up with 3183 words


#  *** FDR *** analysis

spm<-spm[-ndays,]

time<-dj[-ndays,1]

# Take returns 

par(mfrow=c(1,2))

R<-(dj[-ndays,7]-dj[-1,7])/dj[-1,7]

plot(R~time,type="l")


# Take the log of the maximal spread

V<-log(dj[-ndays,3]-dj[-ndays,4])

plot(V~time,type="l")


# FDR: we want to pick a few words that correlate with the outcomes (returns and volatility)

# create a dense matrix of word presence

P <- as.data.frame(as.matrix(spm>0))

# we will practice parallel computing now

library(parallel)


margreg <- function(x){
	fit <- lm(Outcome~x)
	sf <- summary(fit)
	return(sf$coef[2,4]) 
}

cl <- makeCluster(detectCores())

# pull out stars and export to cores

# **** Analysis for Returns ****

Outcome<-R

clusterExport(cl,"Outcome") 

# run the regressions in parallel

mrgpvals <- unlist(parLapply(cl,P,margreg))

# continue on your own


# **** Repeat for volatility

Outcome<-V

clusterExport(cl,"Outcome") 

# run the regressions in parallel

mrgpvals <- unlist(parLapply(cl,P,margreg))


# continue on your own

# ***** LASSO analysis *****

# First analyze returns 

lasso1<- gamlr(spm, y=R, lambda.min.ratio=1e-3)


# continue on your own

# **** LASSO Analysis of volatility **** #

lasso2<- gamlr(spm, y=V, lambda.min.ratio=1e-3)

# continue on your own

# let's try to predict future volatility from past volatility, we will add one more predictor-> volatility from the previous days


Previous<-log(dj[-1,3]-dj[-1,4]) # remove the last return

spm2<-cbind(Previous,spm) # add the previous return to the model matrix

colnames(spm2)[1]<-"previous" # the first column is the previous volatility

lasso3<- gamlr(spm2, y=V, lambda.min.ratio=1e-3)


# continue on your own

# Bootstrap to obtain s.e. of 1.s.e. chosen lambda

# We apply bootstrap to approximate
# the sampling distribution of lambda 
# selected by AICc

# export the data to the clusters 

Outcome<-V

clusterExport(cl,"spm2")
clusterExport(cl,"V")

# run 100 bootstrap resample fits

boot_function <- function(ib){

	require(gamlr)

	fit <- gamlr(spm2[ib,],y=V[ib], lambda.min.ratio=1e-3)

	fit$lambda[which.min(AICc(fit))]
}

boots <- 100

n <- nrow(spm2)

resamp <- as.data.frame(
			matrix(sample(1:n,boots*n,replace=TRUE),
			ncol=boots))

lambda_samp <- unlist(parLapply(cl,resamp,boot_function))

# continue on your own



# High-dimensional Covariate Adjustment 

d <- Previous # this is the treatment

# marginal effect of past on present volatility

summary(glm(V~d)) 



# we want to isolate the effect of d from external influences. We saw that words can explain some of the volatility.

# Stage 1 LASSO: fit a model for d on x


# continue on your own


# Stage 2 LASSO: fit a model for V using d, dhat and x

# continue on your own


