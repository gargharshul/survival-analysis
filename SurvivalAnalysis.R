library(survival)
library(readxl)

setwd("C:/Purdue/Advanced Business Analytics")
Diabetic <- read_excel("DiabeticData.xlsx")

colnames(Diabetic)

head(Diabetic, 5)

str(Diabetic)

Diabetic$time=floor(Diabetic$time)
fit.all<-survfit(Surv(time,status==1)~1,data=Diabetic)
plot(fit.all, xlab = "Time", ylab = "Survival Probablity", ylim = c(0.40,1))

Diabetic$laser<- as.factor(Diabetic$laser)
Diabetic$eye <- as.factor(Diabetic$eye)
Diabetic$trt<- as.factor(Diabetic$trt)

fit.laser<-survfit(Surv(time,status==1)~laser,data=Diabetic)
plot(fit.laser,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probablity", main="Status by Laser Type")
llabel<-gsub("x=","",names(fit.laser$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~laser,data=Diabetic)

Diabetic$agecat <- cut(Diabetic$age, breaks=c(0, 22, Inf), labels=c("minor", "adult"))

fit.age<-survfit(Surv(time,status==1)~agecat,data=Diabetic)
plot(fit.age,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probablity", main="Status by Age")
llabel<-gsub("x=","",names(fit.age$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~agecat,data=Diabetic)

fit.eye<-survfit(Surv(time,status==1)~eye,data=Diabetic)
plot(fit.eye,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probablity", main="Status by Eye")
llabel<-gsub("x=","",names(fit.eye$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~eye,data=Diabetic)

fit.trt<-survfit(Surv(time,status==1)~trt,data=Diabetic)
plot(fit.trt,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probablity", main="Status by Treatment")
llabel<-gsub("x=","",names(fit.trt$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~trt,data=Diabetic)

Diabetic$riskcat <- cut(Diabetic$risk, breaks=c(5, 9, 12), labels=c("medium", "high"))

fit.risk<-survfit(Surv(time,status==1)~riskcat,data=Diabetic)
plot(fit.risk,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probablity", main="Status by Risk")
llabel<-gsub("x=","",names(fit.risk$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~riskcat,data=Diabetic)

fit.cox <- coxph(formula = Surv(time, status  ==1) ~ laser + eye + trt + riskcat + age ,data = Diabetic)
summary(fit.cox)

fit.cox2 <- coxph(formula = Surv(time, status  ==1) ~ eye + trt + riskcat ,data = Diabetic)
summary(fit.cox2)

#Assumptions
#Residual Plot
plot(predict(fit.cox),residuals(fit.cox,type = 'martingale'),ylab = 'residuals',xlab = 'fittedvalues')
abline(h=0)
lines(smooth.spline(predict(fit.cox),residuals(fit.cox,type = 'martingale')),col='red')
#Shows clear linear residual plot

#Checking for Constant Hazard Ratio 
k=cox.zph(fit.cox2)
k
plot(k[3,])
abline(h=0)

plot(k[2,])
abline(h=0)

plot(k[1,])
abline(h=0)
#All have constant hazard ratio


#Outlier Analysis
ggcoxdiagnostics(fit.cox2, type = "dfbeta",linear.predictions = FALSE, ggtheme = theme_bw())
summary(fit.cox2)
#No outlier values have significant contribution