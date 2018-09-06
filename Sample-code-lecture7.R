######################################################
### Lecture 7
######################################################
## Eg 7.1
######################################################
#### nls
cl <- data.frame(
  X=c(rep(2*4:21, c(2, 4, 4, 3, 3, 2, 3, 3, 3, 3, 2, 
                    3, 2, 1, 2, 2, 1, 1))),
  Y=c(0.49, 0.49, 0.48, 0.47, 0.48, 0.47, 0.46, 0.46, 
      0.45, 0.43, 0.45, 0.43, 0.43, 0.44, 0.43, 0.43, 
      0.46, 0.45, 0.42, 0.42, 0.43, 0.41, 0.41, 0.40, 
      0.42, 0.40, 0.40, 0.41, 0.40, 0.41, 0.41, 0.40, 
      0.40, 0.40, 0.38, 0.41, 0.40, 0.40, 0.41, 0.38, 
      0.40, 0.40, 0.39, 0.39)
)

nls.sol<-nls(Y~a+(0.49-a)*exp(-b*(X-8)), data=cl,
             start = list( a= 0.1, b = 0.01 ))

nls.sum<-summary(nls.sol); nls.sum

xfit<-seq(8,44,len=200)
yfit<-predict(nls.sol, data.frame(X=xfit))
plot(cl$X, cl$Y)
lines(xfit,yfit)

nls.sum$parameters

#############
#### nlm
fn <- function(p, X, Y){
  f <- Y-p[1]-(0.49-p[1])*exp(-p[2]*(X-8))
  res <- sum(f^2)
  f1 <- -1+exp(-p[2]*(X-8))
  f2 <- (0.49-p[1])*exp(-p[2]*(X-8))*(X-8)
  J <- cbind(f1,f2)
  attr(res, "gradient") <- 2*t(J)%*%f
  res
} 

nlm(fn, p=c(0.1, 0.01), X=cl$X, Y=cl$Y, hessian=TRUE)

######################################################
## Eg 7.2
######################################################
life <- data.frame(
  X1 = c(2.5, 173, 119, 10, 502, 4, 14.4, 2, 40, 6.6, 
       21.4, 2.8, 2.5, 6, 3.5, 62.2, 10.8, 21.6, 2, 3.4, 
       5.1, 2.4, 1.7, 1.1, 12.8, 1.2, 3.5, 39.7, 62.4, 2.4,
       34.7, 28.4, 0.9, 30.6, 5.8, 6.1, 2.7, 4.7, 128, 35, 
       2, 8.5, 2, 2, 4.3, 244.8, 4, 5.1, 32, 1.4),
  X2 = rep(c(0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
           0, 2, 0, 2, 0, 2, 0),
         c(1, 4, 2, 2, 1, 1, 8, 1, 5, 1, 5, 1, 1, 1, 2, 1,
           1, 1, 3, 1, 2, 1, 4)),
  X3 = rep(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), 
         c(6, 1, 3, 1, 3, 1, 1, 5, 1, 3, 7, 1, 1, 3, 1, 1, 2, 9)),
  Y = rep(c(0,  1,   0,  1), c(15, 10, 15, 10))
)
life

fit0 <- glm(Y~X1+X2+X3, family=binomial, data=life)
summary(fit0)

## stepwise selection 
fit1 <- step(fit0)
fit1

## coefficients 
coef(fit1)
confint(fit1)


## prediction 
## log odds
pre0 <- predict(fit1, data.frame(X2=2,X3=0))
## probability
p0 <- exp(pre0)/(1+exp(pre0));
p0

## log odds
pre1 <- predict(fit1, data.frame(X2=2,X3=1))
## probability
p1 <- exp(pre1)/(1+exp(pre1));
p1

## log odds ratio
coef(fit1)[3]
summary(fit1)$coefficients[3,]
summary(fit1)$coefficients[3,1] + qnorm(0.025)*summary(fit1)$coefficients[3,2]
summary(fit1)$coefficients[3,1] + qnorm(0.975)*summary(fit1)$coefficients[3,2]

## diagnosis
influence.measures(fit1)
influence.measures(fit0)

##########################################
## treat example 7.2 as count data 

table0 <- xtabs(~X2+X3+Y, life)
table0
life2 <- as.data.frame(table0)
life2

model0 <- glm(Freq ~ X2 * X3 * Y, family = "poisson", data = life2)
summary(model0)

model1 <- step(model0)
summary(model1)


## log odds 
sum(coef(model1)[4:5])

## log odds 
sum(coef(model1)[4:6])

## log odds ratio 
coef(model1)[6]

summary(model1)$coefficients[6,1] + qnorm(0.025)*summary(model1)$coefficients[6,2] 
summary(model1)$coefficients[6,1] + qnorm(0.975)*summary(model1)$coefficients[6,2] 
#########################################
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)


glm.qD93 <- glm(counts ~ outcome + treatment, family = quasipoisson())
summary(glm.qD93)
