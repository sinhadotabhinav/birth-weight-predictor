# clean up environment
rm(list = ls(all=TRUE))

# import libraries
library(foreign)
library(descr)
library(lattice)
library(dplyr)
library(mosaic)
library(ggplot2)
library(generalhoslem)
library(pROC)
library(lmtest)
library(cowplot)

# read data
ds <- read.dta("/Users/abhinav/codinglife/furtherstats/low birthweight.dta")

# analyse data set and data structure
names(ds)
str(ds)
class(ds[,"age"])
class(ds[,"weightKG"])
class(ds[,"prev_prem"])
class(ds[,"smoke"])
class(ds[,"gp_visits"])
class(ds[,"hyper_tension"])

# data preparation and cleansing
ds[ds == "?"] <- NA
nrow(ds[is.na(ds$low) | is.na(ds$smoke),])
str (ds)
head(ds)
# create a new data frame with variables low, age, weight, prev_prem, smoking_status, gp_visits 
# and hyper_tension following correct naming conventions
# converting hyper tension to a factor with labels
new_ds <- cbind.data.frame(low = ds$low, age = ds$age, weight = ds$weightKG, 
                           prev_prem = ds$prev_prem, smoking_status = ds$smoke,
                           hyper_tension = ds$hyper_tension, gp_visits = ds$gp_visits)
new_ds$hyper_tension[new_ds$hyper_tension == 1] <- 2
new_ds$hyper_tension[new_ds$hyper_tension == 0] <- 1
new_ds$hyper_tension <- factor(new_ds$hyper_tension, levels = c(1,2),
                           labels = c("no hypertension history", "hypertension history"))
new_ds$prev_prem[new_ds$prev_prem > 0] <- 1
new_ds$prev_prem[new_ds$prev_prem == 0] <- 2
new_ds$prev_prem <- factor(new_ds$prev_prem, levels = c(1,2),
                               labels = c("multiple previous premature babies", "no previous premature babies"))
str(new_ds)
head(new_ds)

#############################
# part a: data interpreation
#############################
# dependant variable is low and independent variables are age, weight, smoking_status,
# prev_prem, hyper_tension and gp_visits
# make sure low variable is an integer i.e. 0 or 1 rather than a factor
freq(new_ds$low)
# plot graphs - make sure you label all axes for report
xyplot(new_ds$low ~ new_ds$age, ylab="Birth Weight Level", xlab="Age of Mother")
boxplot(new_ds$age ~ new_ds$low, ylab="Birth Weight Level", xlab="Age of Mother", horizontal=TRUE)
# median of age is same for both birth weight levels 0 and 1
# lot of overlap
# mention the outliers that you see
favstats(new_ds$age ~ new_ds$low)
# summary statistics also validate the identical median of age
xyplot(new_ds$low ~ new_ds$weight, ylab="Birth Weight Level", xlab="Weight of Mother (Kg)")
boxplot(new_ds$weight ~ new_ds$low, ylab="Birth Weight Level", xlab="Weight of Mother (Kg)", horizontal=TRUE)
# lot of overlap
# median is higher for low = 0
xyplot(new_ds$low ~ new_ds$gp_visits, ylab="Birth Weight Level", xlab="GP visits of mother")
boxplot(new_ds$gp_visits ~ new_ds$low, ylab="Birth Weight Level", xlab="GP visits of mother", horizontal=TRUE)
# cross check with summary stats if required
favstats(ds$weight ~ ds$low)
favstats(ds$prev_prem ~ ds$low)
favstats(ds$gp_visits ~ ds$low)
# check all categorical variables now
crosstab(new_ds$low, new_ds$smoking_status, plot = T, prop.c = T, chisq = T)
crosstab(new_ds$low, new_ds$prev_prem, plot = T, prop.c = T, chisq = T)
crosstab(new_ds$low, new_ds$hyper_tension, plot = T, prop.c = T, chisq = T)
freq(new_ds$smoking_status)
freq(new_ds$prev_prem)
freq(new_ds$hyper_tension)
xtabs(~ low + smoking_status, data=new_ds)
xtabs(~ low + prev_prem, data=new_ds)
xtabs(~ low + hyper_tension, data=new_ds)

##############################
# part b: logistic regression
##############################
# logistic regression with age as predictor variable
model_age <- glm(new_ds$low ~ new_ds$age, family = binomial)
summary(model_age)
confint.default(model_age)
exp(cbind(OR = coef(model_age), confint.default(model_age)))
# Interpretation
# 0 lies between minimum and maximum
# p value is 0.188 > 0.05 suggesting non-siginificant relationship
# OR is 0.965 < 1 suggests a -ve coefficient as seen in the summary of regression, also odds is close to evens

# logistic regression with weight as predictor variable
model_weight <- glm(new_ds$low ~ new_ds$weight, family = binomial)
summary(model_weight)
confint.default(model_weight)
exp(cbind(OR = coef(model_weight), confint.default(model_weight)))
# Interpretation
# 0 lies between minimum and maximum
# p value is 0.0217 < 0.05 implying a significant relationship
# y intercept is 0.8173
# slope is -0.01195 which means with increase of 1 Kg in last menstrual weight the birthweight reduces by 1.2%
# OR is 0.988 < 1 suggests a -ve coefficient as seen in the summary of regression, also odds is close to evens
# check for non-linear possibility
new_ds$weight2 <- new_ds$weight * new_ds$weight
model_weight2 <- glm(new_ds$low ~ new_ds$weight + new_ds$weight2, family=binomial)
summary(model_weight2)
# p value is 0.293 > 0.05 hence, the squared relation is not significant - stick with linear for weight

# logistic regression with smoking status as predictor variable
model_smoking <- glm(new_ds$low ~ new_ds$smoking_status, family = binomial)
summary(model_smoking)
exp(cbind(OR = coef(model_smoking), confint.default(model_smoking)))
# Interpretation
# 0 lies between minimum and maximum
# p value is 0.005 < 0.05 implying a significant relationship
# y intercept is -1.0369
# slope is 0.8033
# OR is 2.233 > 1 implying a +ve coefficient and more than double odds

# logistic regression with prev_prem as predictor variable
model_prev_prem <- glm(new_ds$low ~ new_ds$prev_prem, family = binomial)
summary(model_prev_prem)
exp(cbind(OR = coef(model_prev_prem), confint.default(model_prev_prem)))
# Interpretation
# 0 lies between minimum and maximum
# p value is 8.05*e-05 < 0.05 implying a significant relationship
# y intercept is 0.4964 and slope is -1.477 : positive relationship
# OR is 0.228 > 1 implying a +ve coefficient and log odds of 4.38

# logistic regression with hyper_tension as predictor variable
model_hyper_tension <- glm(new_ds$low ~ new_ds$hyper_tension, family = binomial)
summary(model_hyper_tension)
exp(cbind(OR = coef(model_hyper_tension), confint.default(model_hyper_tension)))
# Interpretation
# 0 lies between minimum and maximum and almost symmetrical
# p value is 0.00642 < 0.05 implying a significant relationship
# y intercept is 0.6061 and slope is -1.4404 : negative relationship
# OR is 4.22 > 1 4 times the odds and +ve coefficient

# logistic regression with gp_visits as predictor variable
model_gp_visits <- glm(new_ds$low ~ new_ds$gp_visits, family = binomial)
summary(model_gp_visits)
# Interpretation
# 0 lies between minimum and maximum
# p value is 0.16779 > 0.05 implying a non-significant relationship

# final logistic regression model
logistic_model_one <- glm(new_ds$low ~ new_ds$age + new_ds$weight + new_ds$smoking_status +
                            new_ds$prev_prem + new_ds$hyper_tension, family = binomial)
summary(logistic_model_one)
exp(cbind(OR = coef(logistic_model_one), confint.default(logistic_model_one)))
# all p values except for age show significant relationship
# OR for age and weight < 1 => -ve coefficients which implies -ve relationship
# OR for others > 1 => +ve coefficients which implies +ve relationship

######################################
# part c: model interpreation and fit
######################################

# Hoshmer-Lemshow statistical tests
new_ds_temp <- cbind.data.frame(low = new_ds$low, age = new_ds$age, weight = new_ds$weight,
                                smoking_status = new_ds$smoking_status,
                                prev_prem = new_ds$prev_prem, hyper_tension = new_ds$hyper_tension)
freq(new_ds_temp$low)
freq(new_ds_temp$age)
freq(new_ds_temp$weight)
freq(new_ds_temp$smoking_status)
freq(new_ds_temp$prev_prem)
freq(new_ds_temp$hyper_tension)
ds_final <- new_ds_temp[complete.cases(new_ds_temp),]
logistic_model_final <- glm(ds_final$low ~ ds_final$age + ds_final$weight + ds_final$smoking_status +
                        ds_final$prev_prem + ds_final$hyper_tension, family = binomial)
ds_final$low_predict <- predict.glm(logistic_model_final, type="response")
logitgof(ds_final$low, ds_final$low_predict, g=6)
logitgof(ds_final$low, ds_final$low_predict, g=10)
logitgof(ds_final$low, ds_final$low_predict, g=12)
logitgof(ds_final$low, ds_final$low_predict, g=14)
# Interpreation
# All p-values are greater than 0.05 suggesting that the model is GOOD FIT
# Receiver Operatoe Curve
low_roc <- roc(ds_final$low ~ ds_final$low_predict, ci= TRUE)
print(low_roc)
# area under ROC is 0.7332
# 95% CI: 0.6624-0.804
# aligns with the outputs of HL test => GOOD FIT
plot(low_roc$specificities, low_roc$sensitivities, xlim = c(1.0, 0.0), ylim = c(0.0, 1.0),
     type = "s", lwd = 2, col = "#999999", xaxs = "i", yaxs = "i",
     xlab = "Specificity", ylab = "Sensitivity")
polygon(c(low_roc$specificities[low_roc$specificities>=0], 0),
        c(low_roc$sensitivities[low_roc$sensitivities>=0], 0),
        density = c(10, 20), angle = c(-45, 45),
        col="#b3b3b3")
abline(a = 1, b = -1, col = "#999999")

# predicted probability plot
predicted <- data.frame(probability_low = logistic_model_final$fitted.values, low = ds_final$low)
predicted <- predicted[order(predicted$probability_low, decreasing = FALSE),]
predicted$rank <- 1:nrow(predicted)
ggplot(data = predicted, aes(x = rank, y = probability_low)) +
  geom_point(aes(color = low), alpha = 0.75, shape = 1, stroke = 1, size = 1) +
  xlab("Index") +
  ylab("Predicted probability of low birth weight")
ggsave("low_birth_weight_probabilities.pdf")

 #################################
# part c: (i) # interaction test
#################################
# testing the interaction between smoking status and age
logistic_modelA <- glm(ds_final$low ~ ds_final$smoking_status +
                         ds_final$age + ds_final$smoking_status:ds_final$age, family = binomial)
summary(logistic_modelA)
exp(cbind(OR = coef(logistic_modelA), confint.default(logistic_modelA)))
logistic_modelA0 <- glm(ds_final$low ~ ds_final$smoking_status + ds_final$age, family = binomial)
summary(logistic_modelA0)
exp(cbind(OR = coef(logistic_modelA0), confint.default(logistic_modelA0)))
lrtest(logistic_modelA,logistic_modelA0)
# p value 0.1398 > 0.05 which implies insignificant interaction between smoke status and age

##################################
# part c: (ii) # interaction test
##################################
# testing the interaction between hypertension and age
logistic_modelB <- glm(ds_final$low ~ ds_final$hyper_tension +
                         ds_final$age + ds_final$hyper_tension:ds_final$age, family = binomial)
summary(logistic_modelB)
exp(cbind(OR = coef(logistic_modelB), confint.default(logistic_modelB)))
logistic_modelB0 <- glm(ds_final$low ~ ds_final$hyper_tension + ds_final$age, family = binomial)
summary(logistic_modelB0)
lrtest(logistic_modelB,logistic_modelB0)
# p value 0.1003 > 0.05 which implies insignificant interaction between hypertension and age
