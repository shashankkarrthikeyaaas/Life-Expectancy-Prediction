
# Setting Working Directory
setwd("/Users/shashank7/OneDrive/PGP - BABI/24 Capstone Project/Project/Code")

# Setting Seed
set.seed(290871)

# Importing Required Libraries
library("caroline")
library("tidyverse")
library("plotly")
library("corrplot")
library(DMwR)
library("rfUtilities")

options(warn=-1)

# Loading the Data Set
dataset_df = read.csv("Life_expectancy.csv")

# Printing the Data Set
#print(dataset_df)

# ------------------------------------------------------- #
# 1.2 Data Report Start

# 1.2.1 Process of Data Collection
cat("\nThe Number of Rows in the Dataset are :",dim(dataset_df)[1])
cat("\nThe Number of Columns in the Dataset are :",dim(dataset_df)[2])
cat("\nThe Various Columns in the Dataset are as follows :\n","")
print(names(dataset_df))

cat("\n The number of countries we have the dataset for are:",length(unique(dataset_df$Country)))

# Getting the Count of Years we have data
library("caroline")
grouped_df = groupBy(df = dataset_df,
                     by = "Country",
                     aggregation = "length",
                     clmns = c("Country","Year"),
                     full.names = TRUE)

cat("\n The number of years of data for each country:\n","")
print(grouped_df[-c(1)])

cat("\n The countries with only 1 year of data are\n","")
print(grouped_df[grouped_df$Year_length <= 1,][-c(1)])

library("tidyverse")
dataset_df = dataset_df %>% rename(
  "Infants.Mortality" = "infant.deaths",
  "thinness.10.19.years" = "thinness..1.19.years",
  "domestic.pvt.health.exp" = "percentage.expenditure"
)

cat("\n The Various Attributes after renaming\n","")
print(names(dataset_df))

# ------------------------------------------------------- #

# Univariate Analysis

# Converting Data to factors
dataset_df$Country = as.factor(dataset_df$Country)
dataset_df$Year = as.factor(dataset_df$Year)
dataset_df$Status = as.factor(dataset_df$Status)

# Basic Data Summary
cat("\n The Basic Data Summary is as follows:\n","")
print(summary(dataset_df))

# par(mfrow=c(7,3),mar=c(2.1,2.1,2.1,2.1))
# hist(dataset_df$Life.expectancy,main = "Life Expectancy")
# hist(dataset_df$Adult.Mortality,main = "Adult.Mortality")
# hist(dataset_df$Infants.Mortality,main = "Infants.Mortality")
# hist(dataset_df$Alcohol,main = "Alcohol")
# hist(dataset_df$domestic.pvt.health.exp,main = "domestic.pvt.health.exp")
# hist(dataset_df$Hepatitis.B,main = "Hepatitis.B")
# hist(dataset_df$Measles,main = "Measles")
# hist(dataset_df$BMI,main = "BMI")
# hist(dataset_df$under.five.deaths,main = "under.five.deaths")
# hist(dataset_df$Polio,main = "Polio")
# hist(dataset_df$Total.expenditure,main = "Total.expenditure")
# hist(dataset_df$Diphtheria,main = "Diphtheria")
# hist(dataset_df$HIV.AIDS,main = "HIV.AIDS")
# hist(dataset_df$GDP,main = "GDP")
# hist(dataset_df$Population,main = "Population")
# hist(dataset_df$thinness.10.19.years,main = "thinness.10.19.years")
# hist(dataset_df$thinness.5.9.years,main = "thinness.5.9.years")
# hist(dataset_df$Income.composition.of.resources,main = "Income.composition")
# plot.new()
# hist(dataset_df$Schooling,main = "Schooling")
# 
# par(mfrow=c(3,3),mar=c(2.1,2.1,2.1,2.1))
# boxplot(dataset_df$Life.expectancy,main = "Life Expectancy")
# boxplot(dataset_df$Adult.Mortality,main = "Adult.Mortality")
# boxplot(dataset_df$Infants.Mortality,main = "Infants.Mortality")
# boxplot(dataset_df$Alcohol,main = "Alcohol")
# boxplot(dataset_df$domestic.pvt.health.exp,main = "domestic.pvt.health.exp")
# boxplot(dataset_df$Hepatitis.B,main = "Hepatitis.B")
# boxplot(dataset_df$Measles,main = "Measles")
# boxplot(dataset_df$BMI,main = "BMI")
# boxplot(dataset_df$under.five.deaths,main = "under.five.deaths")
# 
# par(mfrow=c(3,3),mar=c(2.1,2.1,2.1,2.1))
# boxplot(dataset_df$Polio,main = "Polio")
# boxplot(dataset_df$Total.expenditure,main = "Total.expenditure")
# boxplot(dataset_df$Diphtheria,main = "Diphtheria")
# boxplot(dataset_df$HIV.AIDS,main = "HIV.AIDS")
# boxplot(dataset_df$GDP,main = "GDP")
# boxplot(dataset_df$Population,main = "Population")
# boxplot(dataset_df$thinness.10.19.years,main = "thinness.10.19.years")
# boxplot(dataset_df$thinness.5.9.years,main = "thinness.5.9.years")
# boxplot(dataset_df$Income.composition.of.resources,main = "Income.composition")

par(mfrow=c(1,1),mar=c(2.1,2.1,2.1,2.1))
boxplot(dataset_df$Schooling,main = "Schooling")

cat("\nThe Summary of the Column Country is \n","")
print(summary(dataset_df$Country))

cat("\nThe Summary of the Column Year is \n","")
print(summary(dataset_df$Year))

cat("\nThe Summary of the Column Status  is \n","")
print(summary(dataset_df$Status))

# ------------------------------------------------------- #

# Bivariate Analysis

# Corelation
library("corrplot")
cor_df = dataset_df
cor_df[is.na(cor_df)] = 0
corTable = cor(cor_df[-c(1,2,3)])
cat("\n The Corelation Table is as follows :\n","")
print(corTable)
corrplot(corr = corTable , 
         method = "shade", 
         type = "upper", 
         main = "Correlation Plot")

# Trend Analysis
trend_df = groupBy(df = cor_df,
                    by = "Year",
                    aggregation = "median",
                    clmns = c(names(dataset_df[-c(1,2,3)])),
                    full.names = FALSE)

 # Adding Year to the Final Data Frame
 Year_values = rownames(trend_df)
 trend_df = add_column(.data = trend_df, 
                       Year = Year_values, 
                       .before = "Life.expectancy")

 # Adding Plots
 plot_life_exp <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Life.expectancy,
                  name = 'Life.Expectancy', 
                  type = 'scatter', 
                  mode = 'lines')
 

 # --

 plot_adult_mortal <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Adult.Mortality,
                  name = 'Adult.Mortality', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_infants_mortal <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Infants.Mortality,
                  name = 'Infants.Mortality', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_Alcohol <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Alcohol,
                  name = 'Alcohol', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_domestic.pvt.health.exp <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$domestic.pvt.health.exp,
                  name = 'domestic.pvt.health.exp', 
                  type = 'scatter', 
                  mode = 'lines')


 subplot(plot_adult_mortal,
         plot_infants_mortal,
         plot_Alcohol,
         plot_domestic.pvt.health.exp,
         nrows = 4,
         margin = 0.05)
 # --- #

 plot_Hepatitis.B <- plot_ly(trend_df,
                             x = ~trend_df$Year,
                             y = ~trend_df$Hepatitis.B,
                             name = 'Hepatitis.B', 
                             type = 'scatter', 
                             mode = 'lines')

 plot_Measles <- plot_ly(trend_df,
                         x = ~trend_df$Year,
                         y = ~trend_df$Measles,
                         name = 'Measles', 
                         type = 'scatter', 
                         mode = 'lines')

 plot_BMI <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$BMI,
                  name = 'BMI', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_under.five.deaths <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$under.five.deaths,
                  name = 'under.five.deaths', 
                  type = 'scatter', 
                  mode = 'lines')

 subplot(plot_Hepatitis.B,
         plot_Measles,
         plot_BMI,
         plot_under.five.deaths,
         nrows = 4,
         margin = 0.05)
# --- #

 plot_Polio <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Polio,
                  name = 'Polio', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_Total.expenditure <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Total.expenditure,
                  name = 'Total.expenditure', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_Diphtheria <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Diphtheria,
                  name = 'Diphtheria', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_HIV.AIDS <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$HIV.AIDS,
                  name = 'HIV.AIDS', 
                  type = 'scatter', 
                  mode = 'lines')

 subplot(plot_Polio,
         plot_Total.expenditure,
         plot_Diphtheria,
         plot_HIV.AIDS,
         nrows = 4,
         margin = 0.05)

 # --- #

 plot_GDP <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$GDP,
                  name = 'GDP', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_Population <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Population,
                  name = 'Population', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_thinness.10.19.years <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$thinness.10.19.years,
                  name = 'thinness.10.19.years', 
                  type = 'scatter', 
                  mode = 'lines')

 plot_thinness.5.9.years <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$thinness.5.9.years,
                  name = 'thinness.5.9.years', 
                  type = 'scatter', 
                  mode = 'lines')

 subplot(plot_GDP,
         plot_Population,
         plot_thinness.10.19.years,
         plot_thinness.10.19.years,
         nrows = 4,
         margin = 0.05)

 #
 plot_Income <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Income.composition.of.resources,
                  name = 'Income.composition', type = 'scatter', mode = 'lines')

 plot_Schooling <- plot_ly(trend_df,
                  x = ~trend_df$Year,
                  y = ~trend_df$Schooling,
                  name = 'Schooling', type = 'scatter', mode = 'lines')

 subplot(plot_Income,
         plot_Schooling,
         nrows = 2,
         margin = 0.05)

# ------------------------------------------------------- #

# Missing Value Treatment

cat("\n The Total Number of NA's in the Dataset is :",sum(is.na(dataset_df)))
cat("\n The countries with only 1 year of Data\n","")
field_list = c("Cook Islands","Dominica",
               "Marshall Islands","Monaco",
               "Nauru","Niue","Palau",
               "Saint Kitts and Nevis",
               "San Marino","Tuvalu")
print(subset(dataset_df , Country %in% field_list))


# AfterRemoving those data
dataset_df = subset(dataset_df , !Country %in% field_list)

cat("\n The Total Number of NA's in the Life Expectancy(Dependent 
    Variable) Column:",sum(is.na(dataset_df$Life.expectancy)),"")

# Iterating through Various Countries
# Using MICE Algorithm to fill Na's
library("mice")
consol_df = data.frame()
for (country in unique(dataset_df$Country)){
  print(country)
  
  country_df = subset(dataset_df, Country == country)
  miceOutput = mice(country_df,
                    m = 1,
                    maxit = 1, 
                    method = "cart" ,
                    diagnostics = FALSE , 
                    remove.collinear = FALSE, 
                    threshold = 0.05)
  miceDf = complete(miceOutput)
  print(sum(is.na(miceDf)))

  consol_df = rbind(consol_df,miceDf)
}

cat("\n The Number of NA's in 
    Given Data Set",sum(is.na(dataset_df)))
cat("\n The Number of NA's after 
    Imputation using MICE",sum(is.na(consol_df)))

dataset_df = consol_df

miceOutput = mice(dataset_df,
                  m = 1,
                  maxit = 1, 
                  method = "cart" ,
                  diagnostics = FALSE , 
                  remove.collinear = FALSE, 
                  threshold = 0.05)
miceDf = complete(miceOutput)
dataset_df = miceDf

cat("\n The Number of Missing Values after 
    MICE Prediction across all countries :",sum(is.na(dataset_df)),"\n")

#------------------------------ #

# Outlier Treatment

cat("\n The Outlier Values across each 
    attribute is as follows:\n","")
for (column in names(dataset_df[-c(1,2,3,4)])){
  outlier_values = boxplot.stats(dataset_df[,column])$out
  cat("\n",column, length(outlier_values))
}
library("anchors")
consol_df = data.frame()
for (country in unique(dataset_df$Country)){
  
  country_df = subset(dataset_df, Country == country)
  print(country)
  col_lables = names(country_df[-c(1,2,3,4)])
  for (col in col_lables){
   
    outlier_values = boxplot(country_df[,col])$out
    
    if (length(outlier_values)){
      
      for (val in outlier_values){
        
        country_df = replace.value(data = country_df, 
                      names = c(col),
                      from = val, 
                      to = median(country_df[,col]))
      }
      
    }
  }
  consol_df = rbind(consol_df,country_df)
}

dim(consol_df)
cat("\n The Outlier Values across each 
    attribute is as follows(After Outlier Treatment):\n","")
for (column in names(consol_df[-c(1,2,3,4)])){
  outlier_values = boxplot.stats(consol_df[,column])$out
  cat("\n",column, length(outlier_values))
}

dataset_df = consol_df
write.csv(dataset_df,"Cleaned Data Non PCA.csv", row.names = FALSE)

# -------------------------- #

# Encoding String Variables to Numeric
countries_list = c(sort(as.character(unique(dataset_df$Country))))
countries_lables = c(seq(1,length(countries_list),by = 1))

dataset_df$Country = factor(dataset_df$Country, 
                            levels = countries_list, 
                            labels = countries_lables)
dataset_df$Status = factor(dataset_df$Status, 
                           levels = c("Developed","Developing"), 
                           labels = c(1,0))

dataset_df$Country = as.numeric(dataset_df$Country)
dataset_df$Status = as.numeric(dataset_df$Status)
dataset_df$Year = as.numeric(dataset_df$Year)

# Variable Treatment

cat("\n Test for Multi Collinearity")
# Checking for Multi Collinearity
model = lm(formula = dataset_df$Life.expectancy ~ ., 
           data = dataset_df)
library(car)
vif(model)

corTable = cor(dataset_df)

# PFA
library("psych")
cat("\n Performing Bartlett Test \n","")
print(cortest.bartlett(corTable))

cat("\n Performing KMO Test \n","")
print(KMO(corTable))

# Eigen Values
eigenValues = eigen(corTable)$values
cat("\n The Eigen Values are as follows :\n",
    eigenValues)

# Scree Plot
plot(eigenValues, xlab = "Principal Factor", 
     ylab = "Eigen Values" , 
     col = "red") 
lines(eigenValues,col = "red")

#PFA
#Rotated
rotated = fa(dataset_df[-c(1,2,3,4)], 
             nfactors = 6, 
             rotate = "varimax", 
             fm="pa") 
fa.diagram(rotated) 
rotatedScoresDf = data.frame(rotated$scores)
names(rotatedScoresDf)

# Renaming Columns
library(dplyr)
rotatedScoresDf = rename(.data = rotatedScoresDf, 
                         Personal.Factors = PA1,
                         Population.And.Infant.Mortality = PA2,
                         Chronic.Dieseases = PA3,
                         Economic.Factors = PA4,
                         Adult.Factors = PA5,
                         Thinness = PA6)

dataset_df = cbind(dataset_df[c(
  "Country","Year","Status","Life.expectancy","Total.expenditure")],
  rotatedScoresDf)

# Variable Short Listing
model = lm(formula = dataset_df$Life.expectancy ~ ., 
           data = dataset_df)
cat("\nThe Summary of the Model is as foolows","")
print(summary(model))

# Dropping Unnecessary Columns
dataset_df = dataset_df[-c(1,2)]

# ----------------------------------------------- #
# Data Balance
hist(dataset_df$Life.expectancy)

# Insights
corrTable = cor(dataset_df)
library(corrplot)
corrplot(corr = corrTable , 
         method = "shade", 
         type = "upper", 
         main = "Correlation Plot")

#write.csv(dataset_df,"Cleaned Data.csv", row.names = FALSE)
