#-------------------------------------------
#Step 0: Reading the data
setwd("C:\#Datasets 4 import")

df <-as.data.frame(read.csv("income_by_age.csv"))
df

#-------------------------------------------
#Step 1: Understanding the data 
summary(df)

# scatterplots
plot(df)
pairs(df)
plot(df[c(1,4)], pch = 16, col = "blue") 

# correlation
cor(df$Age, df$Income)

# boxplots
boxplot(df$Age, main="Age")  # boxplot for 'age'
boxplot(df$Income, main="Income") # boxplot for 'income'

boxplot.stats(df$Age)$out

#density plot
d <- density(df$Age)
plot(d)
rug(df$Age)

d <- density(df$Income)
plot(d)
rug(df$Income)


#-------------------------------------------
#Step 2: Building the model
model1 <- lm(Income ~ Age,data = df)
model1

#-------------------------------------------
#Step 3: Visualising the results
plot(df$Age, df$Income, pch = 16, col='blue')
abline(model1, col='red')

#-------------------------------------------
#Step 4 - Understanding the results from the model
summary(model1)

#--------------------------------------------------
#Step 5 - Using the model to predict new values
Age = 41
new_val = data.frame(Age)
pred_income = predict(model1, new_val, level=.95)
pred_income

Age = c(37, 55, 28, 33)
new_val = data.frame(Age)
pred_income = predict(model1, new_val, level=.95)
pred_income
