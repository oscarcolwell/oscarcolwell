#Question 6:
  
path <- "http://statslab.cam.ac.uk/~rds37/teaching/statistical_modelling/"
BrainSize <- read.csv(file.path(path, "BrainSize.csv"))
attach(BrainSize)
BrainSizeLM2 <- lm(PIQ ~ MRI_Count + Height)

#This code takes some data from the stated webpage and froms a standard linear
#model from some of the variables involved and their associated responses.

install.packages("ellipse")
library(ellipse)

plot(ellipse(BrainSizeLM2, which = c(2, 3), level = 0.95, type = "1"))

#This plots a 95% confidence ellipsoid for the values of our unknown parameters
#calculated in the linear model.
#We now plot 95% confidence lines for each co-efficient.

confline.MRI <- confint(BrainSizeLM2, 'MRI_Count', level = 0.95)
abline(v = confline.MRI[1], col="red")
abline(v = confline.MRI[2], col="red")

confline.height <- confint(BrainSizeLM2, 'Height', level = 0.95)
abline(h = confline.height[1], col="red")
abline(h = confline.height[2], col="red")

#Now plot rectange as in Q6 ES1, which is just considering 97.5% for each 
#variable.

confline.MRI.2 <- confint(BrainSizeLM2, 'MRI_Count', level = 0.975)
abline(v = confline.MRI.2[1], col="blue")
abline(v = confline.MRI.2[2], col="blue")

confline.height.2 <- confint(BrainSizeLM2, 'Height', level = 0.975)
abline(h = confline.height.2[1], col="blue")
abline(h = confline.height.2[2], col="blue")

#This yields the following plot:

summary(BrainSizeLM2, correlation = TRUE)$correlation
cor(Height, MRI_Count)



#Question 7:

library(MASS)
?hills
pairs(hills)

#We observe the plots, noting that we would expect a very strong correlation 
#between distance and time - we notice one data point seems to be a strong 
#outlier, and seems to take a disproportionately large amount of time; 
#We find this entry:

subset(hills, dist < 5)

#We notice that the "Knock Hill" run takes over 5 times longer than any other
#run with shorter distance, and given it has a low climb in addition we conclude
#that this is most likely an error.
#We reduce the run time by one hour, in our own copy:

hills.1 <- hills
hills.1["Knock Hill",'time'] <- hills.1["Knock Hill",'time'] - 60

#We now transform our variables using the log function.

hills.1.log <- log(hills.1)

#We now construct two linear models, one with dependence on height, one with 
#dependence on height and climb.

attach(hills.1.log)
hills.log.lm.1 <- lm(time ~ dist)
res.1 <- resid(hills.log.lm.1)
hills.log.lm.2 <- lm(time ~ dist + climb)
res.2 <- resid(hills.log.lm.2)

#We then look at the data from each model.

plot(fitted(hills.log.lm.1), res.1)
abline(0,0)
plot(fitted(hills.log.lm.2), res.2)
abline(0,0)
summary(hills.log.lm.1)
summary(hills.log.lm.2)

#Looking at our summaries, we prefer the second model. The residuals have a 
#significantly tighter spread, which would be expected introducing an 
#parameter, but the effect is dramatic.

#We get the estimates and std. errors for each value as:
#intercept: 0.29359, 0.27312
#dist: 0.91141, 0.06534
#climb: 0.24889, 0.04761.

#we now use our model to predict log(t) within 95% prediction interval for each 
#model, and noting log is strictly increasing, we take the exponents of the
#endpoints of our intervals.

new.data <- data.frame("dist" = log(5.3), "climb" = log(1000))
predict(hills.log.lm.1, newdata = new.data, interval = "prediction", level = 0.95)
predict(hills.log.lm.2, newdata = new.data, interval = "prediction", level = 0.95)

#Which gives us [3.11599,4.006326] 95% prediction interval for the first model,
#and [3.200242, 3.865404] 95% prediction interval for the second model, both 
#log t.

#We notice a tighter prediction interval for our second model. This makes sense
#as we made our prediction using the addition information of climb, which we
#conject does increase the time of the climb. We finally exponent these 
#intervals to get a prediction interval for the time:

interval.1 <- c(3.11599,4.006326)
interval.2 <- c(3.200242, 3.865404)

interval.1 <- exp(interval.1)
interval.2 <- exp(interval.2)

interval.1
interval.2

#this displays our prediction intervals for t with each model.



