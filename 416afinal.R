#Install and load required packages
install.packages("car")
library(car)
install.packages("MASS")
library(MASS)
install.packages("lmtest")
library(lmtest)
library(readr)
library(ggplot2)

#Make sure to change the file path as per your device
regdat <- read_csv("C:/Users/sarve/Downloads/regdat.csv")

#read the data
data=regdat

#filter out rows without desirable prices
data=data[data$price>4000,]
data=data[data$price<5000,]

#fit model on only continuous variables and use vif to remove multicollinearity
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

#check for heteroscedasticity
bptest(model)

#box-cox transform to counter heteroscedasticity
bc <- boxcox(data$price ~ data$carat)
x=data[,c('depth','table','price','y','z','x','carat')]

y=data$price
(lambda <- bc$x[which.max(bc$y)])


data$price=((y^lambda-1)/lambda)

#check for heteroscedasticity again
bptest(model)

model_dat=data.frame(dat$price,model$residuals)
model_dat$price=dat$price
ggplot(model_dat, aes(x=dat.price, y=model.residuals)) + geom_point()

#Now check for normality
qqnorm(model$residuals, pch = 1, frame = FALSE)
qqline(model$residuals, col = "steelblue", lwd = 2)

#backward elimination
dat=data[,c('depth','table','price','y','z')]
model=(lm((price)~.,dat))
summary(model)

dat=data[,c('depth','table','price','y')]
model=(lm((price)~.,dat))
summary(model)
#above is the best of this order with slight increase in adj rsq

dat=data[,c('depth','table','price','z')]
model=(lm((price)~.,dat))
summary(model)

dat=data[,c('depth','y','price','z')]
model=(lm((price)~.,dat))
summary(model)

dat=data[,c('y','table','price','z')]
model=(lm((price)~.,dat))
summary(model)

dat=data[,c('depth','table','price')]
model=(lm((price)~.,dat))
summary(model)

dat=data[,c('depth','price','y')]
model=(lm((price)~.,dat))
summary(model)

dat=data[,c('table','price','y')]
model=(lm((price)~.,dat))
summary(model)

#selected model is
dat=data[,c('depth','table','price','y')]
model=(lm((price)~.,dat))
summary(model)

#check the train MSE
val=mean((model$residuals)**2)
val

#Now split the dataset, train on smaller data and obtain test MSE
n = nrow(data)
split = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.75, 0.25))

training = data[split, ]
testing = data[!split, ]

dat=training[,c('depth','table','price','y')]
model=(lm((price)~.,dat))
summary(model)

testing$pred=predict(model,testing)
testing$residuals=testing$pred-testing$price

val=mean((testing$residuals)**2)
val



