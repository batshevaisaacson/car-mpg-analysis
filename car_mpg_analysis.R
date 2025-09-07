
#read in the file auto-mpg.csv
auto_data <- read.csv(file.choose())
View(auto_data)
summary(auto_data)

#convert horsepower column to numeric values
auto_data$horsepower <- as.numeric(as.character(auto_data$horsepower))
auto_data$horsepower

#remove all NA values from the dataset
data <- na.omit(data)

#access only the first 300 samples
auto_data300 <- auto_data[1:300, ]
View(auto_data300)
summary(auto_data300)

#simple linear regression with mpg and weight
simple_model <- lm(mpg ~ weight, data=auto_data300)
summary(simple_model)

#plot mpg and weight
plot(auto_data300$mpg, auto_data300$weight, 
     main="Mpg and Weight", 
     xlab="mpg",
     ylab="weight",
     col="blue")

#find coefficients for regression equation
coef(slr_model)[1]
coef(slr_model)[2]

#multiple linear regression
full_model <- lm(mpg ~ cylinder+displacement+horsepower+weight+acceleration+
                   model.year+origin, data=auto_data300)
summary(full_model)

#coefficients for multiple linear regression equation (full model)
coef(full_model)[1]
coef(full_model)[2]
coef(full_model)[3]
coef(full_model)[4]
coef(full_model)[5]
coef(full_model)[6]
coef(full_model)[7]
coef(full_model)[8]

#remove all variable with p-values above .05
reduced_model <- lm(mpg ~ origin+model.year+weight, data=auto_data300)
summary(reduced_model)

#coefficients for multiple linear regression equation (reduced model)
coef(reduced_model)[1]
coef(reduced_model)[2]
coef(reduced_model)[3]
coef(reduced_model)[4]

#access last 98 samples in the dataset
auto_data98 <- auto_data[301:398, ]
View(auto_data98)
summary(auto_data98)

#predict mpg for last 98 samples
predictions <- predict(reduced_model, auto_data98)
predictions
actual <- auto_data98$mpg
actual
data.frame(predictions,actual)


#Calculating the residuals
residuals <- auto_data98$mpg - predictions
residuals

#plot residuals
plot(predictions, residuals, 
     main = "Residual Plot", 
     xlab="Predicted Mpg", 
     ylab="Residuals",
     pch=16)
abline(h=0,col="red")

#histogram of residuals
hist(residuals,
     main="Histogram of Residuals",
     xlab="Residuals",
     col="pink")
