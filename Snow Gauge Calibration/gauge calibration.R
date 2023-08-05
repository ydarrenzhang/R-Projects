#import and read datafile
library(readxl)
gainData = read_excel("C:/Users/dz/Documents/Course Work/STAT 517/proj4/517 project 4.xlsx")
head(gainData)
View(gainData)

#fit nonlinear to linear model log(gain)~density
model = lm(log(gain)~1+density, data = gainData) #note: we fit a transformation of gain to density
model
summary(model) #log(gain) = 5.99727 - 4.60594 * density

#diagnostics, assumption checking
plot(model)
shapiro.test(model$residuals)
library(car)
values = c(gainData$gain, gainData$density)
group = as.factor(c(rep(1,length(gainData$gain)), rep(2,length(gainData$density))))
leveneTest(values, group)

#Plot of log(gain) by density, fit a least squares line
scale = seq(0, 0.7, by = 0.025)
plot(gainData$density, log(gainData$gain), xaxt = "n", xlab = "Density (g per cubic cm)", ylab = "log(Gain)", main = "Gain by Density", cex.axis = 1)
axis(1,at=scale,labels = scale, cex.axis = 0.7)
abline(model)
#prediction interval plotted, PI is avg. of a set of obs. taken at density point x
newx = c(0.0010,0.08,0.148,0.223,0.318,0.412,0.508,0.604,0.6860)
pred_int = predict(model, newdata = data.frame(density = newx), interval = "predict", level = 0.95)
lines(newx, pred_int[,2], col="red")
lines(newx, pred_int[,3], col="red")

#95% PI for gain
x = gainData$density
y = log(gainData$gain)
xbar = mean(gainData$density)
ybar = mean(y)
yhat = 5.99727 - 4.60594 * x
LB = yhat - qnorm(0.975) * var(y - yhat)
UB = yhat + qnorm(0.975) * var(y - yhat)
#95% PI for density
dLB = (LB - 5.99727)/(-4.60594)
dUB = (UB - 5.99727)/(-4.60594)


##fit nonlinear to linear model density~log(gain)
inversemodel = lm(density~log(gain), data = gainData)
summary(inversemodel) #density = 1.298013 - 0.216203 * log(gain)

#diagnostics, assumption checking
par(mfrow=c(2,2))
plot(inversemodel)
shapiro.test(inversemodel$residuals) #fail to reject null
library(car)
values = c(gainData$gain, gainData$density)
group = as.factor(c(rep(1,length(gainData$gain)), rep(2,length(gainData$density))))
leveneTest(values, group) 
dev.off()

#point and interval est. of snow density given gain now w/ PI w/ predict()
#95% PI bounds
predict(inversemodel, data = gainData, interval = "predict", level = 0.95)
predict(inversemodel, data = gainData, interval = "confidence", level = 0.95) #CI

sdensity = function(y) {
  p = predict(inversemodel, newdata = data.frame(gain = y), interval = "predict", level = 0.95)
  print("Estimated Snow Density(g/cm^3) w/ 95% PI:")
  return(p)
}
sdensity(298) #input a gain to get a est. density, Note: this is an inverse estimator

#manually finding 95%PI
ahat = 5.99727 
bhat = -4.60594
gain = log(gainData$gain)
den = gainData$density
dest = (gain - ahat) / bhat
s = 0.01471
s2 = 0.06792
denLB = dest - (qnorm(0.975) * s * sqrt(1 + (1/9) + ((gain - mean(gain))^2 / (sum((gain - mean(gain))^2)))))
denUB = dest + (qnorm(0.975) * s * sqrt(1 + (1/9) + ((gain - mean(gain))^2 / (sum((gain - mean(gain))^2)))))

#manual calc snow density estimate function w point and int. est.
snowdensity = function(y) {
  ahat = 5.99727 
  bhat = -4.60594
  gain1 = log(y)
  s = 0.01471
  avgden = 0.3311111 #mean(density)
  dest = (gain1 - ahat) / bhat
  denLB2 = dest - (qnorm(0.975) * s * sqrt(1 + (1/9) + ((gain1 - mean(gain))^2 / (sum((gain1 - mean(gain))^2)))))
  denUB2 = dest + (qnorm(0.975) * s * sqrt(1 + (1/9) + ((gain1 - mean(gain))^2 / (sum((gain1 - mean(gain))^2)))))
  output = c(dest, denLB2, denUB2)
  print("Estimated Snow Density(g/cm^3) w/ 95% PI:")
  return(output)
}
snowdensity(298) #input a gain to get a est. density, Note: this is an inverse estimator
