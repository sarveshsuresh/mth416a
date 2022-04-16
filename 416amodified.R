install.packages("car")
library(car)
install.packages("olsrr")
library(olsrr)
install.packages("MASS")
library(MASS)
install.packages("lmtest")
library(lmtest)
library(readr)
library(ggplot2)

regdat <- read_csv("C:/Users/sarve/Downloads/regdat.csv")

#read the data
data=regdat

#filter out rows without desirable prices
data=data[data$price>4000,]
data=data[data$price<5000,]

#fit the full model and check for residuals
model=lm(price~.,data)
summary(model)

qqnorm(model$residuals, pch = 1, frame = FALSE)
qqline(model$residuals, col = "steelblue", lwd = 2)

hist(model$residuals,xlab='residuals',prob=TRUE)

model_dat=data.frame(model$fitted.values,model$residuals)
model_dat$prev_error=model_dat$model.residuals
for(i in 2:nrow(model_dat)){
  model_dat$prev_error[i]=model_dat$model.residuals[i-1]
  
}
model_dat$price=dat$price

ggplot(model_dat, aes(x=model.fitted.values, y=model.residuals)) + geom_point()
ggplot(model_dat, aes(y=model.fitted.values, x=prev_error)) + geom_point()

#consider only continuous variables for checking multicollinearity
dat=data[,c('depth','table','price','y','z','x','carat')]
model=(lm((price)~.,dat))
summary(model)
vif(model)

dat=data[,c('depth','table','price','y','z','x')]
model=(lm((price)~.,dat))
summary(model)
vif(model)

dat=data[,c('depth','table','price','y','z')]
model=(lm((price)~.,dat))
summary(model)
vif(model)

#remove the variables causing multicollinearity

dat=data
dat$x=NULL
dat$carat=NULL
model=lm(price~.,dat)
summary(model)

ols_step_backward_p(model)

dat$table=NULL
dat$Good=NULL
dat$z=NULL

model=lm(price~.,dat)
summary(model)

