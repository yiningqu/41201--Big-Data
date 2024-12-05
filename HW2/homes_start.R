##### ******** Mortgage and Home Sales Data ******** #####

## Read in the data

homes <- read.csv("homes2004.csv")

# conditional vs marginal value


par(mfrow=c(1,2)) # 1 row, 2 columns of plots 

hist(homes$VALUE, col="grey", xlab="home value", main="")

plot(VALUE ~ factor(BATHS), 
    col=rainbow(8), data=homes[homes$BATHS<8,],
    xlab="number of bathrooms", ylab="home value")

# You can try some quick plots.  Do more to build your intuition!

#par(mfrow=c(1,2)) 
#plot(VALUE ~ STATE, data=homes, 
#	col=rainbow(nlevels(homes$STATE)), 
#	ylim=c(0,10^6), cex.axis=.65)
#plot(gt20dwn ~ FRSTHO, data=homes, 
#	col=c(1,3), xlab="Buyer's First Home?", 
#	ylab="Greater than 20% down")

## code hints 


# QUESTION 1
#Regress log price onto all variables but mortgage.
#What is the R2? How many coefficients are used in this model and how many are significant at 10% FDR?
#Re-run regression with only the significant covariates, and compare R2 to the full model. (2 points)


# regress log(PRICE) on everything except AMMORT
fullmodel <- glm(log(LPRICE) ~ .-AMMORT, data=homes)

#Calculate R^2

# Method1: R^2 = 1 - SSE/SST
predicted_values <- predict(fullmodel, type="response")
actual_values <- log(homes$LPRICE)
# Calculate SSE
sse <- sum((actual_values - predicted_values)^2)
# Calculate SST
sst <- sum((actual_values - mean(actual_values))^2)
# Calculate R-squared
r_squared <- 1 - (sse/sst)
cat("R-squared: ", r_squared, "\n")

# Method2: R^2 = 1 - D/D0
# Get the residual deviance
D <- fullmodel$deviance
# Get the null deviance
D0 <- fullmodel$null.deviance
# Calculate pseudo-R^2
pseudo_R2 <- 1 - (D / D0)
# Print pseudo-R^2
cat("Full Model R-squared: ", pseudo_R2, "\n")



#Total number of coefficients
num_coefficient <- length(coef(fullmodel))
cat("Total number of coefficients:", num_coefficient, "\n")

#Number of significant coefficients at 10% FDR
# extract pvalues
pvals <- summary(fullmodel)$coef[-1,4]
# Adjust p-values for FDR using the Benjamini-Hochberg method
adjustedPvals <- p.adjust(pvals, method = "fdr")
# Determine significant coefficients at a 10% FDR level
cat("Number of significant coefficients at 10% FDR:", sum(adjustedPvals < 0.10), "\n")


#Re-run regression with only the significant covariates
significant_var_names <- names(adjustedPvals)[adjustedPvals < 0.10]
print(significant_var_names)
# Step 1: Create a model matrix for the full model
full_model_matrix <- model.matrix(fullmodel)
# Step 2: Subset the matrix to keep only significant variables and the intercept
# Note: model.matrix automatically includes an intercept column as the first column
significant_matrix <- full_model_matrix[, c("(Intercept)", significant_var_names)]
# Step 3: Fit the new model using the subsetted matrix
# Since the response was transformed via log, ensure to apply that transformation here as well
new_model <- glm(log(homes$LPRICE) ~ ., data = as.data.frame(significant_matrix))
# Get the residual deviance
D_new <- new_model$deviance
# Get the null deviance
D0_new <- new_model$null.deviance
# Calculate pseudo-R^2
pseudo_R2_new <- 1 - (D_new / D0_new)
# Print pseudo-R^2
cat("New Model R-squared: ", pseudo_R2_new, "\n")
cat("Difference in R-squared between full and reduced models:",pseudo_R2 -  pseudo_R2_new, "\n")



# QUESTION 2
#Fit a regression for whether the buyer had more than 20 percent down (onto everything but AMMORT and LPRICE). Interpret effects for Pennsylvania state, 1st home buyers and the number  of bathrooms.Add and describe an interaction between 1st home-buyers and the number of baths. (2 points)
# - don't forget family="binomial"!
# - use +A*B in forumula to add A interacting with B
# create a var for downpayment being greater than 20%
homes$gt20dwn <- 
  factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)
q2_model1 <- glm(gt20dwn ~ . -AMMORT -LPRICE, data = homes, family = binomial())
q2_model2 <- glm(gt20dwn ~ . -AMMORT -LPRICE+ FRSTHO*BATHS, data = homes, family = binomial())

summary(q2_model1)
coef_statepa <- coef(q2_model1)["STATEPA"]
coef_frsthoy <- coef(q2_model1)["FRSTHOY"]
coef_bedrms <- coef(q2_model1)["BEDRMS"]
# Calculate the odds multiplier
print(coef_statepa)
print(coef_frsthoy)
print(coef_bedrms)

odds_multiplier_statepa <- exp(coef_statepa)
odds_multiplier_frsthoy <- exp(coef_frsthoy)
odds_multiplier_bedrms <- exp(coef_bedrms)
print(odds_multiplier_statepa)
print(odds_multiplier_frsthoy)
print(odds_multiplier_bedrms)

coef_inter <- coef(q2_model2)["BATHS:FRSTHOY"]
print(coef_inter)
print(exp(coef_inter))



# QUESTION 3
#Focus only on a subset of homes worth $>100k$.
#Train the full model from Question 1 on this subset. Predict the left-out homes using this model. What is the out-of-sample fit (i.e. R2)? Explain why you get this value. (1 point)


# training sample
subset <- which(homes$VALUE>100000)
homes <- subset(homes, select = -gt20dwn)

pricey2 <- glm(log(LPRICE) ~ .-AMMORT, data=homes[subset,])
pred <- predict(pricey2, newdata=homes[-subset,])

# Use the code ``deviance.R" to compute OOS deviance
source("deviance.R")
# Null model has just one mean parameter
ybar <- mean(log(homes$LPRICE[-subset]))
D0_sub <- deviance(y=log(homes$LPRICE[-subset]), pred=ybar)
D_sub  <- deviance(y=log(homes$LPRICE[-subset]), pred=pred)
R2_sub <- 1 - D_sub / D0_sub
print(R2_sub)






