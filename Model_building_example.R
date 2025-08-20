library(leaps)
library(performance)
library(ggplot2)

pred <-read.csv("C:/Users/Path/to/coronado_pred.csv")  #use the file path to physical data we are trying to predict

#set working directory, where models will be saved to
setwd("C:/Users/Path/to/save/folder/")

vars <-read.csv("C:/Users/Path/to/AZCNF_vars.csv")#use the file path to point cloud metrics

## Split data into training and test datasets
# Set a random seed for reproducibility
set.seed(123)

# Calculate the number of rows for the test set
sample_size <- floor(0.1 * nrow(vars))

# Randomly sample row indices for the test set
test_indices <- sample(seq_len(nrow(vars)), size = sample_size)

# Split the data into training and test sets
train_vars <- vars[-test_indices, ]
test_vars <- vars[test_indices, ]
train_pred <- pred[-test_indices, ]
test_pred <- pred[test_indices, ]

#
#use pred$"response variable of interest" as response variable and vars for predictors. For method, "exhaustive" works best and is not affected by order of metrics, but on small processors, 
#it will take hours. You can use "forward", "backward" or "seqrep", but these are affected by order, "seqrep" will iteratively replace variables and strikes a nice ballence between
#"exhaustive" and efficiency.

regfit = regsubsets(train_pred$no_fuel_cov ~., train_vars,  nvmax = 3, method = "exhaustive" , really.big = T) 
regsum<- summary(regfit)
plot(regsum$rsq, type = "l") #look for the best bang for your buck, when more variables don't improve r2, then that is a candidate, we are looking for parsimony not iffy giant r2 values. Don't chase r2
plot(regsum$rss, type = "l") #look for the opposite on this curve

plot(regsum$bic, type = "l") #low BIC is best
which.min(regsum$bic) #this prints the lowest
plot(regfit, scale = "r2") # this shows all of the variables by model size


vcov(regfit, 4) # change the number to the best model, most balancing model simplicity, bic, collinearity (calculated below), and accuracy
coef(regfit,4)# change the number to the best model then copy the predictors 

attach(train_vars) #the R community hates "attach" but it increases efficiency here for the next step + vars$

# put the selected predictor variables in the model after the "~" seperated by "+" and run the model
lm1 <- lm(formula = train_pred$no_fuel_cov~        )


#look at the summary r2 values and RMSE
summary(lm1)
r2(lm1)
rmse(lm1)

# check collinearity between variables used in the model
check_collinearity(lm1)

# Visual check of model assumptions. Make sure Linearity, homogeneity of variance, influential observations, colinearity, normality of risiduals all check out
check_model(lm1)

#use the model to predict out new values from variables alone
lm_out <- predict.lm(lm1, train_vars)


#format the predictions and observations for plotting
data<-data.frame(x=lm_out, y=train_pred$no_fuel_cov)

# Plot observed values vs. predicted
ggplot(data,aes(x,y)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE,) +
  theme_light() +
  labs(x='Predicted Values', y='Observed Values', title='No Fuel Cover') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold') 
  ) 

# use the model to predict test data
test_out<- predict.lm(lm1,test_vars)

# Calculate RMSE of test data
errors<- test_out - test_pred$no_fuel_cov
mae <- mean(abs(errors))
mse <- mean(errors^2)
rmse <- sqrt(mse)
rmse

# format test predicted and observed
data2<- data.frame(x=test_out, y=test_pred$no_fuel_cov)

# Plot test observed values vs. test predicted
ggplot(data2,aes(x,y)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE,) +
  theme_light() +
  labs(x='Predicted Values', y='Observed Values', title='No Fuel Cover test') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold') 
  ) 


# If the model looks good, save to disk
saveRDS(test1, "C:/Users/spokswinski/Documents/NoFuelMod_AZCNF.rda" )

