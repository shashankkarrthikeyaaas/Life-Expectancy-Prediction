
# Setting Work Directory
setwd("/Users/shashank7/OneDrive/PGP - BABI/24 Capstone Project/Project/Code")

# Reading Dataset
dataset_df = read.csv("Cleaned Data.csv")

# Setting the Status as a Factor
dataset_df$Status = as.factor(dataset_df$Status)

# Storing Life Expectancy as a seperate variable
life_expectancy = dataset_df$Life.expectancy

# Storing Status as a seperate variable
status = dataset_df$Status

# Saving Unscaled values for SVM
dataset_df_unscaled = dataset_df

# Scaled Data Frame
dataset_df = as.data.frame(scale(dataset_df[-c(1,2)]))

# Adding Back Life Expectancy Columns
dataset_df = cbind(life_expectancy,status,dataset_df)

# Setting Seed
set.seed(1609)

# Splitting into Testing and Training Data Sets
library(caTools)
sample = sample.split(dataset_df,SplitRatio = 0.5)
train_data_df = subset(dataset_df,sample == TRUE)
test_data_df = subset(dataset_df, sample == FALSE)

# ----------------------- #

# Linear Regression
lm_model = lm(formula = life_expectancy ~., 
              data = train_data_df)

# Summary of the Linear Model
cat("\nThe Summary of the model is as follows: ","")
print(summary(lm_model))

# VIF Test
library("car")
cat("\nThe Output of the VIF Test is as follows: ","")
print(vif(mod = lm_model))

# Prediction of Values
lm_predicted_values = predict(lm_model,
                              test_data_df[-c(1)])

# Loading Package Metrics
library("Metrics")
cat("\n\nLinear Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,predicted = lm_predicted_values))
cat("\nLinear Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,predicted = lm_predicted_values)))
cat("\nLinear Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,predicted = lm_predicted_values)))


# ----------------------- #

# CART
library("rpart")
cart_mdl = rpart(formula = life_expectancy ~., 
                 data = train_data_df, 
                 method = "anova", 
                 control=rpart.control(minsplit=30, cp=0.001))

# Plotting CART Model
plot(cart_mdl)

# Prediction using CART Model
cart_predicted_values = predict(cart_mdl,test_data_df[-c(1)])

# Summary of the Model
cat("\n The summary of the CART Model is as follows:","")
print(summary(cart_mdl))

# Loading Package Metrics
library("Metrics")
cat("\nCART Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,predicted = cart_predicted_values))
cat("\nCART Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,predicted = cart_predicted_values)))
cat("\nCART Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,predicted = cart_predicted_values)))

# ----------------------- #
# Random Forest Model
library("randomForest")
rf_model = randomForest(life_expectancy ~ ., data = train_data_df)

cat("\n The summary of the Random Forest Model is as follows:","")
print(rf_model)

rf_predicted_values = predict(rf_model,test_data_df)

cat("\nRandom Forest -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,predicted = rf_predicted_values))
cat("\nRandom Forest -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,predicted = rf_predicted_values)))
cat("\nRandom Forest -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,predicted = rf_predicted_values)))

# ----------------------- #


# K Fold Cross Validation

# Number of Sub Samples
no_of_sub_samples = 10

# Splitting Data Set into Sub Samples 
# Getting the Sub Sample Numbers
sub_samples = cut(x = seq(1, nrow(dataset_df)),breaks = no_of_sub_samples, labels = FALSE)

# Linear Regression

# Vectors to store the errors for all Sub Samples
rmse_all_sub_samples = vector()
mae_all_sub_samples = vector()
mape_all_sub_samples = vector()

for (sub_sample in 1:no_of_sub_samples){
    
    # Getting the Index of Test Data Set
    test_set_index = which(sub_samples == sub_sample, arr.ind = TRUE)
    
    # Getting the Training Data Set
    train_set_df = dataset_df[-test_set_index,]
    
    # Getting the Testing Data Set
    test_set_df = dataset_df[test_set_index,]
    
    # Considering Linear Model
    lm_model_kfold = lm(formula = life_expectancy ~., 
                  data = train_set_df)
    
    # Prediction of Values
    lm_predicted_values_kfold = predict(lm_model,
                                  test_set_df[-c(1)])
    
    # Calculation of Errors
    rmse_all_sub_samples = cbind(rmse_all_sub_samples,
                                 rmse(actual = test_set_df$life_expectancy,
                                      predicted = lm_predicted_values_kfold))
    
    mae_all_sub_samples = cbind(mae_all_sub_samples,
                                mae(actual = test_set_df$life_expectancy,
                                    predicted = lm_predicted_values_kfold))
    
    mape_all_sub_samples = cbind(mape_all_sub_samples,
                                 mape(actual = test_set_df$life_expectancy,
                                      predicted = lm_predicted_values_kfold))
}
cat("\n\n K Fold Corss Validation (K = 10)\n","Linear Regression")
cat("\n\nRMSE for Various Sub Samples:\n","")
print(rmse_all_sub_samples)
cat("\nMAE for Various Sub Samples:\n","")
print(mae_all_sub_samples)
cat("\nMAPE for Various Sub Samples:\n","")
print(mape_all_sub_samples)
cat("\nMean RMSE across various Sub Samples","")
print(mean(rmse_all_sub_samples))
cat("\nMean MAE across various Sub Samples","")
print(mean(mae_all_sub_samples))
cat("\nMean MAPE across various Sub Samples","")
print(mean(mape_all_sub_samples))

# CART Model

# Vectors to store the errors for all Sub Samples
rmse_all_sub_samples = vector()
mae_all_sub_samples = vector()
mape_all_sub_samples = vector()

for (sub_sample in 1:no_of_sub_samples){
    
    # Getting the Index of Test Data Set
    test_set_index = which(sub_samples == sub_sample, arr.ind = TRUE)
    
    # Getting the Training Data Set
    train_set_df = dataset_df[-test_set_index,]
    
    # Getting the Testing Data Set
    test_set_df = dataset_df[test_set_index,]
    
    # Considering Linear Model
    cart_model_kfold = rpart(formula = life_expectancy ~., 
                             data = train_data_df, 
                             method = "anova", 
                             control=rpart.control(minsplit=30, cp=0.001))
    
    # Prediction of Values
    cart_predicted_values_kfold = predict(cart_model_kfold,
                                        test_set_df[-c(1)])
    
    # Calculation of Errors
    rmse_all_sub_samples = cbind(rmse_all_sub_samples,
                                 rmse(actual = test_set_df$life_expectancy,
                                                           predicted = cart_predicted_values_kfold))
    
    mae_all_sub_samples = cbind(mae_all_sub_samples,
                                mae(actual = test_set_df$life_expectancy,
                                                        predicted = lm_predicted_values_kfold))
    
    mape_all_sub_samples = cbind(mape_all_sub_samples,
                                 mape(actual = test_set_df$life_expectancy,
                                                           predicted = lm_predicted_values_kfold))
}
cat("\n\n K Fold Corss Validation (K = 10)\n","CART Model")
cat("\n\nRMSE for Various Sub Samples:\n","")
print(rmse_all_sub_samples)
cat("\nMAE for Various Sub Samples:\n","")
print(mae_all_sub_samples)
cat("\nMAPE for Various Sub Samples:\n","")
print(mape_all_sub_samples)
cat("\nMean RMSE across various Sub Samples","")
print(mean(rmse_all_sub_samples))
cat("\nMean MAE across various Sub Samples","")
print(mean(mae_all_sub_samples))
cat("\nMean MAPE across various Sub Samples","")
print(mean(mape_all_sub_samples))

# -------------------------------- #

# Randome Forest Tuning
tuneRF(x=train_data_df[-c(1)],
       y=train_data_df$life_expectancy,
       ntreeTry = 50,
       stepFactor = 2,
       improve = 0.05,
       trace=TRUE, 
       plot=TRUE)

rf_model = randomForest(life_expectancy ~ ., 
                        data = train_data_df,
                        mtry = 4)

cat("\n The summary of the Random Forest Model (Tuned) is as follows:","")
print(rf_model)

rf_predicted_values = predict(rf_model,test_data_df)

cat("\nRandom Forest -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,
         predicted = rf_predicted_values))
cat("\nRandom Forest -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,
         predicted = rf_predicted_values)))
cat("\nRandom Forest -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,
          predicted = rf_predicted_values)))
# ---------------------------------- #


# Bagging 
library(ipred)

bagging_mdl = bagging(formula = life_expectancy ~. , 
                      data = train_data_df, nbagg = 50)
cat("\n The summary of the Bagging Model is as follows :","")
print(bagging_mdl)

bagging_predicted_values = predict(object = bagging_mdl,
                                   test_data_df)

cat("\nBagging -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,
         predicted = bagging_predicted_values))
cat("\nBagging -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,
         predicted = bagging_predicted_values)))
cat("\nBagging -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,
          predicted = bagging_predicted_values)))

# ----------------------- #

# Gradient Boosting
library(gbm)

gbm_mdl = gbm(
    formula = life_expectancy ~ .,
    distribution = "gaussian",
    data = train_data_df,
    n.trees = 10000, 
    interaction.depth = 1,
    shrinkage = 0.001,
    cv.folds = 5,
    n.cores = 1, 
    verbose = FALSE
)  

cat("\n\n The Summary of the Gradient Boosting Model is :","")
print(gbm_mdl)
gbm_predicted_values = predict(object = gbm_mdl, test_data_df)

cat("\nGradient Boosting -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,
         predicted = gbm_predicted_values))
cat("\nGradient Boosting -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,
         predicted = gbm_predicted_values)))
cat("\nGradient Boosting -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,
          predicted = gbm_predicted_values)))

# ----------------------- #
cat("\n XGB Boost","")
# XG boost
library(xgboost) 

train_data_df_xgb = train_data_df
train_data_df$status = as.numeric(train_data_df$status)

xgb_mdl = xgboost(
    data = as.matrix(train_data_df[-c(1)]),
    label = train_data_df$life_expectancy,
    eta = 0.7,
    max_depth = 5,
    nrounds = 50,
    nfold = 5,
    objective = "reg:linear",  
    verbose = FALSE,               
    early_stopping_rounds = 10
)

# Summary of XGB Model
cat("\n\n Summary of the X Gradient Boosting Model is as follows \n\n","")
print(xgb_mdl)

# ----- Tuning XGB Boost Model ----

cat("\n\n Tuning of XGB Model","")

# Tuning the Model
# Tuning the Model
eta_check_list <- c(0.001, 0.01, 0.1, 0.3, 0.5, 0.7, 1)
max_depth_check_list = c(1,3,5,7,9,15,20)
nrounds_check_list = c(2, 50, 100, 1000, 10000)

# EtA 
accuracy_vector = vector()
eta_vector = vector()
for (eta_val in eta_check_list){
    print(eta_val)
    
    xgb_mdl_tuning = xgboost(
        data = as.matrix(train_data_df[-c(1)]),
        label = train_data_df$life_expectancy,
        eta = as.numeric(eta_val),
        max_depth = 5,
        nrounds = 50,
        nfold = 5,
        objective = "reg:linear",  
        verbose = FALSE,               
        early_stopping_rounds = 10 
    )
    
    mdl_tuning_predicted_val = predict(xgb_mdl_tuning,
                                       as.matrix(train_data_df[-c(1)]))
    accuracy = mape(actual = train_data_df$life_expectancy,
                    predicted = mdl_tuning_predicted_val)
    
    accuracy_vector = cbind(accuracy_vector,accuracy)
    eta_vector = cbind(eta_vector,eta_val)
    
}
plot(eta_vector, accuracy_vector, mains = " ETA Tuning")
lines(eta_vector, accuracy_vector)

# MAX Depth
accuracy_vector = vector()
md_vector = vector()
for (max_d_val in max_depth_check_list){
    print(max_d_val)
    
    xgb_mdl_tuning = xgboost(
        data = as.matrix(train_data_df[-c(1)]),
        label = train_data_df$life_expectancy,
        eta = 0.1,
        max_depth = as.numeric(max_d_val),
        nrounds = 50,
        nfold = 5,
        objective = "reg:linear",  
        verbose = FALSE,               
        early_stopping_rounds = 10 
    )
    
    mdl_tuning_predicted_val = predict(xgb_mdl_tuning,
                                       as.matrix(train_data_df[-c(1)]))
    accuracy = mape(actual = train_data_df$life_expectancy,
                    predicted = mdl_tuning_predicted_val)
    
    accuracy_vector = cbind(accuracy_vector,accuracy)
    md_vector = cbind(md_vector,max_d_val)
    
}
plot(md_vector, accuracy_vector, mains = " Max Depth Tuning")
lines(md_vector, accuracy_vector)

# N Rounds
accuracy_vector = vector()
round_val_vector = vector()
for (r_round_val in nrounds_check_list){
    print(r_round_val)
    
    xgb_mdl_tuning = xgboost(
        data = as.matrix(train_data_df[-c(1)]),
        label = train_data_df$life_expectancy,
        eta = 0.1,
        max_depth = 5,
        nrounds = as.numeric(r_round_val),
        nfold = 5,
        objective = "reg:linear",  
        verbose = FALSE,               
        early_stopping_rounds = 10 
    )
    
    mdl_tuning_predicted_val = predict(xgb_mdl_tuning,
                                       as.matrix(train_data_df[-c(1)]))
    accuracy = mape(actual = train_data_df$life_expectancy,
                    predicted = mdl_tuning_predicted_val)
    
    accuracy_vector = cbind(accuracy_vector,
                            accuracy)
    round_val_vector = cbind(round_val_vector,
                             r_round_val)
    
}
plot(round_val_vector, accuracy_vector, mains = " N Rounds Tuning")
lines(round_val_vector, accuracy_vector)

# ---------- Final XG Boost Model ----------

xgb_mdl = xgboost(
    data = as.matrix(train_data_df[-c(1)]),
    label = train_data_df$life_expectancy,
    eta = 0.1,
    max_depth = 20,
    nrounds = 2000,
    nfold = 5,
    objective = "reg:linear",  
    verbose = FALSE,               
    early_stopping_rounds = 10
)

# Summary of XGB Model
cat("\n\n\n Summary of the Gradient Boosting Model is as follows \n\n","")
print((xgb_mdl))

test_data_df_xgb = test_data_df
test_data_df_xgb$status = as.numeric(test_data_df_xgb$status)

# Prediction
xgb_predicted_values = predict(xgb_mdl,as.matrix(test_data_df_xgb[-c(1)]))

cat("\nX Gradient Boosting -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,
         predicted = xgb_predicted_values))
cat("\nX Gradient Boosting -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,
         predicted = xgb_predicted_values)))
cat("\nX Gradient Boosting -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,
          predicted = xgb_predicted_values)))

# ----------------------- #
# Support Vector Machines

test_data_df$status = as.numeric(test_data_df$status)


library(e1071)
svm_mdl = svm(formula = life_expectancy ~., 
              data = train_data_df,scale = FALSE)
cat("\n\n Summary of the SVM Model is as follows \n","")
print(summary(svm_mdl))

svm_predicted_values = predict(svm_mdl,test_data_df)

cat("\nX SVM -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,
         predicted = svm_predicted_values))
cat("\nX SVM -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,
         predicted = svm_predicted_values)))
cat("\nX SVM -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,
          predicted = svm_predicted_values)))

# ----------------------- #
