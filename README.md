# Regression-models-with-fixed-and-random-effects-PSYP13
Home assignments for the 2017 autumn semester, Analysis PSPY13 (HT2017) 

####################################
#Assignment 1 
####################################

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
require(psych) # for describe
require(lm.beta) # for lm.beta
require(dplyr)
require(gsheet)
require(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
require(ggplot2) # for ggplot
require(rgl) # for scatter3d

library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(rgl) # for scatter3d

describe(data_sample_1)

data_sample_new <- data_sample_1 [-15,]

summary(data_sample_new)
describe(data_sample_new)

#exclude data from mindfullness 
#ppt 24,25,66 scored <1 on Mindfulness were excluded
data_sample_new<-data_sample_new[!data_sample_new$mindfulness<=1,] 


#histograms and scatterplots 

plot(age ~ pain, data = data_sample_new)
plot(STAI_trait ~ pain, data = data_sample_new)
plot(pain_cat ~ pain, data = data_sample_new)
plot(cortisol_serum ~ pain, data = data_sample_new)
plot(cortisol_saliva ~ pain, data = data_sample_new)
plot(mindfulness ~ pain, data = data_sample_new)

# fit regression model I 
mod_pain1 = lm(pain ~ age + sex, data = data_sample_new)

plot(pain ~ age, data = data_sample_new)
abline(lm(pain ~ age, data = data_sample_new))
plot(pain ~ sex, data = data_sample_new)
abline(lm(pain ~ sex, data = data_sample_new))

#Cooks distance to see outliers
cooks.distance(mod_pain1)
plot(mod_pain1, which = 4) #plot cooks distance to see our outliers
#plot is showing us the 3 highest values: cooks distance criteria: value greater 1 is outliers,
#these three outliers showing in the plot are not greater 1 so can stay in the dataset

mod_pain1
#run regression including outliers
predict(mod_pain1) 

#interpreting the results and regression weights
summary(mod_pain1)
AIC(mod_pain1)
confint(mod_pain1)
lm.beta(mod_pain1)
#Model1: sex and age influences only 10% of the variance of pain
#--> where do 10% come from? Summary of mod_pain1: Adjusted R-squared: 0.1022


#Multiple Regression Model 2
#predicting pain by sex, age, STAI-State, Pain Cat, MAAS, Cortisol
#Fit regression model, model with outliers 
mod_pain2<- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_new)

#interpreting the results and regression weights
summary(mod_pain2)
AIC(mod_pain2)
confint(mod_pain2)
lm.beta(mod_pain2)


#Model1: age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness
#influences only 46,93% of the variance of pain
#--> where do 46,93% come from? Summary of mod_pain1: Adjusted R-squared: 0.4693


#checking assumptions

##Checking assumptions Model 2 --> first assumptions only in table and mutlicollinearity in text 
#Normality assumption
#QQ plot
plot(mod_pain2, which = 2)
# skew and kurtosis
describe(residuals(mod_pain2))
# histogram
hist(residuals(mod_pain2), breaks = 20)

#Linearity assumption
# redicted values against actual values
pred <- predict( object = mod_pain2 )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain2)

#Homoscedasticty assumption (homogeneity of variance)
plot(mod_pain2, which = 3)
ncvTest(mod_pain2)

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_pain2)
pairs.panels(data_sample_new[,c("pain", "age", "sex")], col = "red", lm = T)


#model without one of the cortisol variables 
#--> since high correlation between them 

#cortisol saliva --> mod_pain3_cortisolserum_out --> with outliers 
mod_pain3_cortisolserum_out <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = data_sample_new)

#regression mod_pain3_cortisolserum_out
predict(mod_pain3_cortisolserum_out)

#interpreting the results and regression weights
summary(mod_pain3_cortisolserum_out)
AIC(mod_pain3_cortisolserum_out)
confint(mod_pain3_cortisolserum_out)
lm.beta(mod_pain3_cortisolserum_out)

#look for outliers 
#Cooks distance to see outliers
cooks.distance(mod_pain3_cortisolserum_out)
plot(mod_pain3_cortisolserum_out, which = 4)

#cortisol serum --> mod_pain3_cortisolsaliva_out 
mod_pain3_cortisolsaliva_out <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_new)

#regression mod_pain3_cortisolsaliva_out
predict(mod_pain3_cortisolsaliva_out)

#interpreting the results and regression weights
summary(mod_pain3_cortisolsaliva_out)
AIC(mod_pain3_cortisolsaliva_out)
confint(mod_pain3_cortisolsaliva_out)
lm.beta(mod_pain3_cortisolsaliva_out)

#look for outliers 
#Cooks distance to see outliers
cooks.distance(mod_pain3_cortisolsaliva_out)
plot(mod_pain3_cortisolsaliva_out, which = 4)

#cortisol saliva --> explains varianca higher than if serum is included 
# if serum is excluded than the adjusted r-squared = .4728 
# nevertheless theory based 
#cortisol serum often regarded as more reliable related to stress in medical research
# exclude cortisol saliva 


#checking assumptions

##Checking assumptions Model with cortisol serum --> first assumptions only in table and mutlicollinearity in text 
#Normality assumption
#QQ plot
plot(mod_pain3_cortisolsaliva_out, which = 2)
# skew and kurtosis
describe(residuals(mod_pain3_cortisolsaliva_out))
# histogram
hist(residuals(mod_pain3_cortisolsaliva_out), breaks = 20)

#Linearity assumption
# redicted values against actual values
pred <- predict( mod_pain3_cortisolsaliva_out)
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain3_cortisolsaliva_out, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain3_cortisolsaliva_out)

#Homoscedasticty assumption (homogeneity of variance)
plot(mod_pain3_cortisolsaliva_out, which = 3)
ncvTest(mod_pain3_cortisolsaliva_out)

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_pain3_cortisolsaliva_out)
pairs.panels(data_sample_new[,c("pain", "age", "sex")], col = "red", lm = T)

#comparing models
anova(mod_pain1, mod_pain3_cortisolsaliva_out)

#compare with AIC 
AIC(mod_pain1)
AIC(mod_pain3_cortisolsaliva_out)


#########################################
#Assignment 2#
#########################################


#data_sample_new --> backward regression (backward elimination)

mod_back_pain <- lm(pain ~ age+ sex+ STAI_trait + pain_cat + cortisol_serum + mindfulness + weight, data = data_sample_new)

step(mod_back_pain) #non indicates the actual new model with the excluded variables 

#save model in new R object --> best model with four predicotr 

mod_back_pain_best <- lm(formula = pain ~ age + pain_cat + cortisol_serum + mindfulness,data = data_sample_new)

summary(mod_back_pain_best)

#compare mod_back_pain and mod_back_best 
AIC(mod_back_pain,mod_back_pain_best)
anova(mod_back_pain,mod_back_pain_best)

AIC(mod_back_pain_best)
confint(mod_back_pain_best) 
lm.beta(mod_back_pain_best)

# Assumption check backward model 

### influential cases -> Cook's distance
cooks.distance(mod_back_pain_best)
plot(mod_back_pain_best, which = 4)

### normality assumption
# QQ plot
plot(mod_back_pain_best, which = 2)
# skew and kurtosis
describe(residuals(mod_back_pain_best))
# histogram
hist(residuals(mod_back_pain_best), breaks = 20)

### linearity assumption
# predicted values against actual values
pred <- predict( object = mod_back_pain_best )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
#predicted values against residuals
plot(mod_back_pain_best, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_back_pain_best)

### homoscedasticty assumption (homogeneity of variance)
plot(mod_back_pain_best, which = 3)
ncvTest(mod_back_pain_best)

### multicollinearity (VIF > 3 or 5 -> high multicollinearity)
vif(mod_back_pain_best)
pairs.panels(data_sample_new[,c("age", "pain_cat", "cortisol_serum", "mindfulness")], col = "red", lm = T)

## -> all assumptions are met my mod_back_pain_best

#compare models, which one is better? 

#mod_pain3_cortisolsaliva_out = theory based model 
#mod_back_pain_best = backward model 

AIC(mod_pain3_cortisolsaliva_out, mod_back_pain_best)

Anova(mod_pain3_cortisolsaliva_out,mod_back_pain_best)

anova(mod_pain3_cortisolsaliva_out,mod_back_pain_best)


#applying best models (mod_pain3_cortisolserum_out and mod_back_pain_best) on new data 
#check if it does not depend on data and is transferable on other data sets 
home_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")

# test the performance of backward regression model on the test set
# NOTE that we did not re-fit the models on the test set, we use the models fitted
# on the training set for the prediction


## Check data 
who(TRUE)
summary(home_sample_2)
describe(home_sample_2)

# Exclude ID = 26, 30, 43, 78, 93, 158 because of wrong value in mindfulness
home_sample_2 -> home_sample_2_new
home_sample_2_new=data_sample_2[!home_sample_2$mindfulness<=1,]

pred_test <- predict(mod_pain3_cortisolsaliva_out, home_sample_2_new)
pred_test_back <- predict(mod_back_pain_best, home_sample_2_new )

RSS_test = sum((home_sample_2_new["pain"] - pred_test)^2)
RSS_test_back = sum((home_sample_2_new["pain"] - pred_test_back)^2)
RSS_test
RSS_test_back



#############################################################
#Assignment 3#
#############################################################


### Load packages 
require(psych) #for describe
require(car) #for residualPlots, vif, pair.panels. ncvTest
require(ggplot2) #for ggplot
require(cAIC4) #for cAIC
require(r2glmm) #for r2beta
require(influence.ME) #for influence
require(lattice) #for qqmath
require(reshape2) #for melt function
require(lme4)
require(lmerTest)

### Load data file
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

#descriptive statistics 
describe(data_sample_3)
summary(data_sample_3)
#no missing values 

# Histograms to show the distribution 
hist(data_sample_3$pain_cat, breaks = 15)
hist(data_sample_3$cortisol_serum, breaks = 20) 
hist(data_sample_3$STAI_trait, breaks = 20) 
hist(data_sample_3$cortisol_saliva, breaks = 20) 
hist(data_sample_3$mindfulness, breaks = 20) 
hist(data_sample_3$weight, breaks = 20) 
hist(data_sample_3$pain1, breaks = 20) 
hist(data_sample_3$pain2, breaks = 20) 
hist(data_sample_3$pain3, breaks = 20) 
hist(data_sample_3$pain4, breaks = 20) 
# None of the histograms looks normally distributed 
#(nicht sichtbar in meiner Tabelle, aber skew & kurtosis ist normalverteilt) s. describe (eigentlich)


#we would like to build a model that can accurately describe change over time in pain ratings
#taking into account the demographic, psychological, and hormone variables 
#used in the theory-based model in assignment 2.

### Show variables included in data set 
names(data_sample_3)

## define which are the the repeated measures
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")

### Correlation of repeated variables
cor(data_sample_3[,repeated_variables]) #the closer you are to one data in time the higher the correlation 


#Transform wide to long format

# id.vars should be all non-repeated variables
data_pain_long = melt(data_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")

# order data frame by participant ID(not necessary, just makes the dataframe look more intuitive)
data_pain_long = data_pain_long[order(data_pain_long[,"ID"]),]

# change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)

# lets look at how the data looks like in long format
data_pain_long

#ANALYSIS 
#intercept = Schnittpunkt mit y-Achse  --> random = wie hoch Schnittpunkt ist 
#slope = Steigung der Funktion --> random = sowohl Schnittpunkt als auch Steigung varriiert 

mod_rep_int = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (1|ID), data = data_pain_long)
mod_rep_slope = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (time|ID), data = data_pain_long)
summary(mod_rep_int)
summary(mod_rep_slope)

#adjusted r-squared 
r2beta(mod_rep_int, method="nsj")

r2beta(mod_rep_slope, method="nsj")

### model comparison to see whether to use random slope or random intercept models
## plot the regression line (prediction)
# save the predictions of bot models to variables
data_pain_long$pred_int = predict(mod_rep_int)
data_pain_long$pred_slope = predict(mod_rep_slope)

# random intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# random slope and intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)

# compare models with cAIC
# not too different
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic
# mod_rep_slope better cAIC 175.8321 (random slope and intercept)

# compare models with anova
# not significantly different
anova(mod_rep_int, mod_rep_slope)
# Chisq(2/13) = 20.732, p = 3.149e-05 *** Signif. codes:  0 ‘***’
# cAIC values differ significantly from one another 

#since there is a benefit from the slope we do further analysis with this model 
### adding a quadratic term of time to the slope model
# to account for curved relationship between time and wound rating
mod_rep_slope_quad = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + I(time^2)+ (time|ID), data = data_pain_long)

#summary
summary(mod_rep_slope_quad)

#adjusted r- squared 
r2beta(mod_rep_slope_quad, method="nsj")


## plot the results
# save prediction of the model to new variable
data_pain_long$pred_slope_quad = predict(mod_rep_slope_quad)

# random slope model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# this looks like a better fit than the others

# compare models with cAIC
cAIC(mod_rep_slope)$caic #175.8321
cAIC(mod_rep_slope_quad)$caic #121.9584

# compare models with anova
anova(mod_rep_slope, mod_rep_slope_quad)
# Chisq(1/14) = 29.885 , p = 4.584e-08 *** Signif. codes:  0 ‘***’ 

# based on the results it seems that the slope intercept model,
# including the quadratic term of time would be the best model to 
# choose to describe change of pain over time 

#Data and model diagnostics 

# checking for influential outliers
influence(mod_rep_slope_quad, group = "ID")$alt.fixed 
influence(mod_rep_slope_quad, obs = T)$alt.fixed 
###if there would be a very influential case, you could spot it by having a look at the data (I(time^2))
##varibility of the cases are really small --> so no influential outlier

## checking assumptions

# normality assumption
# QQ plot
qqmath(mod_rep_slope_quad, id=0.05) 
# this might require the lattice package, but you can get the same graph wit the qqnorm() function

# linearity assumption
# linearity of prediction and standardized residuals
plot(mod_rep_slope_quad)
#linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors=c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")


for(i in 1:length(predictors)){
  predictor_to_test = data_pain_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rep_slope_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}

# homoscedasticty assumption (homogeneity of variance)
# look for funnel shape on this graph
plot(mod_rep_slope_quad)
# Levens model for testing the for heteroscedasticity, from here: http://ademos.people.uic.edu/Chapter18.html
# look at the overall model F and p, if it is significant, there may be heteroscedasticity
summary(lm(residuals(mod_rep_slope_quad)^2 ~ data_pain_long[,"ID"]))
# no significant value, homoscedasticty not violated 


# multicollinearity
# there are some functions out there, but for now just look at the correlation matrix
# some example of a function designed to extract vif from lmer: https://raw.githubusercontent.com/aufrank/R-hacks/master/mer-utils.R
pairs.panels(data_pain_long, col = "red", lm = T)

pairs.panels(data_pain_long[,c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")], col = "red", lm = T)
# no high correlations between the variables, no need to exclude any 


