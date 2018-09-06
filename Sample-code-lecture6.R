######################################################
### Lecture 6
######################################################
##########################################################################
forbes <- data.frame(
  matrix(c(194.5, 20.79, 1.3179, 131.79,
           194.3, 20.79, 1.3179, 131.79,
           197.9, 22.40, 1.3502, 135.02,
           198.4, 22.67, 1.3555, 135.55,
           199.4, 23.15, 1.3646, 136.46,
           199.9, 23.35, 1.3683, 136.83,
           200.9, 23.89, 1.3782, 137.82,
           201.1, 23.99, 1.3800, 138.00,
           201.4, 24.02, 1.3806, 138.06,
           201.3, 24.01, 1.3805, 138.05,
           203.6, 25.14, 1.4004, 140.04,
           204.6, 26.57, 1.4244, 142.44,
           209.5, 28.49, 1.4547, 145.47,
           208.6, 27.76, 1.4434, 144.34,
           210.7, 29.04, 1.4630, 146.30,
           211.9, 29.88, 1.4754, 147.54,
           212.2, 30.06, 1.4780, 147.80),
         ncol=4, byrow=T,
         dimnames = list(1:17, c("F", "h", "log", "log100"))))
pairs(forbes)
plot(forbes$F, forbes$log100)
lm.sol <- lm(log100 ~ F, data=forbes)
summary(lm.sol)
abline(lm.sol)

y.res<-residuals(lm.sol);
plot(y.res)
text(12,y.res[12], labels=12,adj=1.2)


forbes12 <- forbes[-12, ]
lm12 <- lm(log100 ~ F, data=forbes12)
summary(lm12)

## estimate
coef(lm12)

## CI 
confint(lm12)
confint(lm12, parm = "F")

## prediction
predict(lm12, newdata = data.frame(F = seq(190, 210, by = 10)), interval = "prediction")




##########################################################################
fit1 <- lm(Fertility ~ ., data = swiss)
add1(fit1, ~ I(Education^2) + .^2)
drop1(fit1, test = "F") 

### stepwise 
step(fit1)

### backward
summary(fit1)
fit2 <- update(fit1, .~.-Examination)
summary(fit2)
anova(fit2, fit1)


##########################################################################

Anscombe <- data.frame(
  X =c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0),
  Y1=c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
  Y2=c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74),
  Y3=c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.44, 5.73),
  X4=c(rep(8,7), 19, rep(8,3)),
  Y4=c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)
)
summary(lm(Y1~X, data=Anscombe))
summary(lm(Y2~X, data=Anscombe))
summary(lm(Y3~X, data=Anscombe))
summary(lm(Y4~X4,data=Anscombe))

coef(lm(Y1~X, data=Anscombe))
coef(lm(Y2~X, data=Anscombe))
coef(lm(Y3~X, data=Anscombe))
coef(lm(Y4~X4,data=Anscombe))

attach(Anscombe)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y1)
abline(lm(Y1~X))

plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y2)
abline(lm(Y2~X))

plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y3)
abline(lm(Y3~X))

plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X4,Y4)
abline(lm(Y4~X4))


#########
X2<-X^2
lm2.sol<-lm(Y2~X+X2)
summary(lm2.sol)
x<-seq(min(X), max(X), by=0.1)
b<-coef(lm2.sol)
y<-b[1]+b[2]*x+b[3]*x^2
plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y2)
lines(x,y)

i <- 1:11; 
Y31<-Y3[i!=3]; 
X3<-X[i!=3]
lm3.sol<-lm(Y31~X3)
summary(lm3.sol)
plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y3)
abline(lm3.sol)


par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
lm1 <- lm(Y1~X)
plot(lm1, which = 1:6)


#################################
p <- 1
n <- 11


### H 
fit.lm3 <- lm(Y3~X, data=Anscombe)
h <- hatvalues(fit.lm3) 
h
h > 2*(p+1)/n

### DIFFITS
diffits0 <- dffits(fit.lm3) 
diffits0 
abs(diffits0) > 2*sqrt((p+1)/n)

### cooks distance
cooks.distance(fit.lm3) 
plot(cooks.distance(fit.lm3), type = "h")

### covratio
covratio(fit.lm3) 
plot(covratio(fit.lm3), type = "h")

### all 
influence.measures(fit.lm3)


