# Sudeepti Surapaneni, Mariah Hurt, Elizabeth Driskill, Arti Patel

#Diamond Regression ------

install.packages("car")
library(MASS)
library(car)
library(ggplot2)
setwd("C://Users//Arti Patel//Desktop//STAT//project1")
#import data
diamonds <- read.csv("clean_diamond_data.csv",sep = ',',header = TRUE,na.strings=c(""," ","NA"))
#initial exploration
summary(diamonds)
str(diamonds)
head(diamonds)
tail(diamonds)
summary(diamonds$price)
summary(diamonds$carat)
summary(diamonds$clarity)
summary(diamonds$cut)
summary(diamonds$color)

#checking significance of carat
price_carat <- lm(formula = price ~ carat, data = diamonds)
#before transformation qq and fitted resid plots
extern_s_resids <- studres(price_carat)
qqnorm(extern_s_resids)
qqline(extern_s_resids)
plot(fitted.values(price_carat), extern_s_resids, main = "Residuals vs. Fitted Values before Transformation", ylab = "Externally Studentized Residuals", xlab = "Fitted Values")

# Initial Variable Exploration -----
#added a normal distribution line in histogram

hist(diamonds$price, freq=FALSE, col="gray", xlab="Price", main="The frequency distribution of the price of diamonds")
curve(dnorm(x, mean=mean(diamonds$price), sd=sd(diamonds$price)), add=TRUE, col="red")

hist(diamonds$carat, freq=FALSE, col="gray", xlab="Carat", main="The frequency distribution of carats")
curve(dnorm(x, mean=mean(diamonds$carat), sd=sd(diamonds$carat)), add=TRUE, col="red")

#Scatter Plot plot price vs carat(pre transformation)
plot(diamonds$carat, diamonds$price,
     col = "black", 
     pch = 8, 
     main = "Price VS Carat - Scatter Plot", 
     xlab = "Carat", 
     ylab = "Price", 
     las = 1)

#code to make av plot for carat and price
avPlots((lm(price~carat, data=diamonds)), main="AV Plot For Carat")

#Transformation ------
diamonds$carat_t <- log(diamonds$carat)
diamonds$price_t <- log(diamonds$price)

#scatter plots before and after transformation
plot(diamonds$price,diamonds$carat)
plot(diamonds$price_t,diamonds$carat_t)

#after transformation qq and fitted resid plots
price_carat_t <- lm(formula = diamonds$price_t ~ diamonds$carat_t, data = diamonds)
summary(price_carat_t)
extern_t <- studres(price_carat_t)
qqnorm(extern_t)
qqline(extern_t)
plot(fitted.values(price_carat_t), extern_t, main = "Residuals vs. Fitted Values after Transformation", ylab = "Externally Studentized Residuals", xlab = "Fitted Values")

#Here is the boxcox to confirm if our ln transformation of price was ideal
bc_res <- boxcox(diamonds$price~diamonds$carat_t + diamonds$clarity + diamonds$color + diamonds$cut, data = diamonds)
lambda <- bc_res$x[bc_res$y == max(bc_res$y)]
print(lambda)
# Lambda Value -0.02020202
#since (price^lambda - 1)/lambda is close to log(price), we can confirm ln is an appropriate transformation

#model with transformation and other variables to consider
model <- lm(price_t ~ carat_t + cut + color + clarity , data = diamonds)
summary(model)

#the boxplots price_ vs categorical variables
boxplot(price_t ~ color, data = diamonds, 
        ylab = "y", xlab = "color")
boxplot(price_t ~ cut, data = diamonds, 
        ylab = "y", xlab = "cut")
boxplot(price_t ~ clarity, data = diamonds, 
        ylab = "y", xlab = "price")

#Multicollinearity ------
vif(model)
X <- model.matrix(model)
cor(X[,-1])

# Partial F tests to test additional terms ------
# We used f tests instead of t tests because every t - test could potentially introduce false rejection of the null hypothesis 
# Partial F tests show that all the variables are significant 

#Big mod including all regressors and all interaction terms with carat
big_mod <- lm(price_t~carat_t + color + cut + clarity + carat_t:color + carat_t:cut + carat_t:clarity, data=diamonds)
#Chekcing out big mod
summary(big_mod)

#Little mods for interaction terms
lil_modnocolint <- lm(price_t~carat_t + cut + color + clarity + carat_t:cut + carat_t:clarity, data=diamonds)

lil_modnocutint <- lm(price_t~carat_t + color + cut + clarity + carat_t:color + carat_t:clarity, data=diamonds)

lil_modnoclarint <- lm(price_t~carat_t + color + cut + clarity + carat_t:cut + carat_t:color, data=diamonds)

#little mods for regressors
lil_modnoclar<- lm(price_t~carat_t + color + cut, data=diamonds)

lil_modnocut<- lm(price_t~carat_t + color + clarity, data=diamonds)

lil_modnocol<- lm(price_t~carat_t + clarity + cut, data=diamonds)

lil_modnocar<- lm(price_t~color + clarity + cut, data=diamonds)


#removing the interaction terms in the lil mod
anova(lil_modnoclarint, big_mod)
anova(lil_modnocutint, big_mod)
anova(lil_modnocolint, big_mod)

#removing only categorical var regressors in the lil mod
anova(lil_modnoclar, big_mod)
anova(lil_modnocut, big_mod)
anova(lil_modnocol, big_mod)

#avplots to test signigicance of variables already part of data set
#avPlots(lm(price_t ~ carat_t + cut + color + clarity, data = diamonds))

#Final model ------
#since all terms + carat:color  + carat:cut + carat:clarity are signigicant, final model includes all terms
model_final <- lm(price_t ~ carat_t + cut + color + clarity + carat_t:cut + carat_t:color + carat_t:clarity, data = diamonds)
summary(model_final)

#Conclusion & Interpretation ------
new_df1 <- data.frame(carat_t = -0.9943, cut = "Good", color = "J", clarity = "SI2")
predict(model_final, new_df1, interval = "prediction", level = 0.95)
# fit: 399.6743
# lower: 291.3878
# upper: 548.2026
predict(model_final, new_df1, interval = "confidence", level = 0.95)

new_df2 <- data.frame(carat_t = -0.6733, cut = "Very Good", color = "H", clarity = "VS2")
predict(model_final, new_df2, interval = "prediction", level = 0.95)
# fit: 1145.11
# lower: 834.8892
# upper: 1570.602
predict(model_final, new_df2, interval = "confidence", level = 0.95)

new_df3 <- data.frame(carat_t = 0.0000, cut = "Ideal", color = "F", clarity = "VVS1")
predict(model_final, new_df3, interval = "prediction", level = 0.95)
# fit: 6949.417
# lower: 5066.73
# upper: 9531.677
predict(model_final, new_df3, interval = "confidence", level = 0.95)

new_df4 <- data.frame(carat_t = 3.0180, cut = "Astor Ideal", color = "D", clarity = "FL")
predict(model_final, new_df4, interval = "prediction", level = 0.95)
# fit: 3973071
# lower: 2882965
# upper: 5475368
predict(model_final, new_df4, interval = "confidence", level = 0.95)

#Additional graphics for end of slides ------
#ggplots on transformed data

#color
ggplot(diamonds, aes(x = carat_t, y = price_t)) + geom_point(aes(color= color))
#cut
ggplot(diamonds, aes(x = carat_t, y = price_t)) + geom_point(aes(color= cut))
#clarity
ggplot(diamonds, aes(x = carat_t, y = price_t)) + geom_point(aes(color= clarity))

#not transformed
#color
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(color= color))
#cut
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(color= cut))
#clarity
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(color= clarity))

