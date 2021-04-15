
# Setting Work Directory
setwd("/Users/shashank7/OneDrive/PGP - BABI/24 Capstone Project/Project/Code")

# Loading the Data
dataset_df = read.csv("Cleaned Data Non PCA.csv")

# Removing Unnecessaryy Columns
dataset_df = dataset_df[-c(1,2)]

# Building Linear Model
lm_mdl = lm(formula = Life.expectancy ~., data = dataset_df)

# Summary of the model
cat("\n The Summary of the model is as follows :","")
print(summary(lm_mdl))

# Storing Life Expectancy as a seperate variable
life_expectancy = dataset_df$Life.expectancy

# Storing Status as a seperate variable
status = dataset_df$Status

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

library("randomForest")
rf_model = randomForest(life_expectancy ~ Adult.Mortality + Infants.Mortality + BMI + under.five.deaths + HIV.AIDS + Income.composition.of.resources + Schooling, 
                        data = train_data_df,
                        mtry = 4)

cat("\n The summary of the Random Forest Model (Tuned) is as follows:","")
print(rf_model)


rf_predicted_values = predict(rf_model,test_data_df)

library("Metrics")
cat("\nRandom Forest -  Regression RMSE :",
    rmse(actual = test_data_df$life_expectancy,
         predicted = rf_predicted_values))
cat("\nRandom Forest -  Regression MAE :",
    (mae(actual = test_data_df$life_expectancy,
         predicted = rf_predicted_values)))
cat("\nRandom Forest -  Regression MAPE : ",
    (mape(actual = test_data_df$life_expectancy,
          predicted = rf_predicted_values)))


library("corrplot")
dataset_df = dataset_df[c("life_expectancy","Adult.Mortality",
"Infants.Mortality",
"BMI","under.five.deaths",
"HIV.AIDS",
"Income.composition.of.resources",
"Schooling")]

corTable = cor(dataset_df)
corrplot(corTable,method = "number",type="upper")

lm_mdl = lm(formula = life_expectancy ~ Adult.Mortality + Infants.Mortality + BMI + under.five.deaths + HIV.AIDS + Income.composition.of.resources + Schooling, data = train_data_df)

# Coeffiecients of the Model
cat("\n\nThe  intercepts are as follows :\n\n","")
print(lm_mdl$coefficients)

# Exponentials of Intercepts
cat("\n\nThe Exponentials of intercets are as follows :\n\n","")
for (coeff in lm_mdl$coefficients){
  print(coeff)
  cat("Exponential : ",exp(coeff))
  cat("\n\n","")}

