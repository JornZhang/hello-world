######################################################
### Lecture 8
######################################################
## Eg 7.1
######################################################
lamp <- data.frame(
  X=c(1600, 1610, 1650, 1680, 1700, 1700, 1780, 1500, 1640, 
      1400, 1700, 1750, 1640, 1550, 1600, 1620, 1640, 1600, 
      1740, 1800, 1510, 1520, 1530, 1570, 1640, 1600),
  A=factor(c(rep(1,7),rep(2,5), rep(3,8), rep(4,6)))
)

str(lamp)

## scatterplot 
plot(X ~ as.numeric(A), data = lamp)

## boxplot
plot(X ~ A, data = lamp)
boxplot(X ~ A, data = lamp)

## plot univariate effects
plot.design(X ~ A, data = lamp)

## anova 
fit1.aov <- aov(X ~ A, data=lamp)
summary(fit1.aov)

## effects \alpha_i 
model.tables(fit1.aov)
tapply(lamp$X, lamp$A, mean) - mean(lamp$X)


## ANOVA table 
anova(fit1.aov)

## check residual
op <- par(mfrow = c(2,2))
plot(fit1.aov)
par(op)

## normality assumption
shapiro.test(lamp$X[lamp$A == 1])
shapiro.test(lamp$X[lamp$A == 2])
shapiro.test(lamp$X[lamp$A == 3])
shapiro.test(lamp$X[lamp$A == 4])

tapply(lamp$X, lamp$A, shapiro.test) 

## equal variance: Bartlett Test
bartlett.test(X ~ A, data = lamp)

## multiple testing  
pairwise.t.test(lamp$X, lamp$A)
pairwise.t.test(lamp$X, lamp$A, adjust = "bonferroni")
TukeyHSD(fit1.aov)


## Kruskal-Wallis Test
kruskal.test(X ~ A, data = lamp)

## Friedman Test
## friedman.test(X ~ A, data = lamp)


######################################################
## Eg 7.2
######################################################

agriculture <- data.frame(
  Y=c(325, 292, 316, 317, 310, 318, 
      310, 320, 318, 330, 370, 365),
  A=gl(4,3),
  B=gl(3,1,12)
)

## anova
fit2.aov <- aov(Y ~ A + B, data=agriculture)
anova(fit2.aov)

## plot univariate effects
plot.design(Y ~ A + B, data=agriculture)

## effects \alpha_i \beta_j
model.tables(fit2.aov)

tapply(agriculture$Y, agriculture$A, mean) - mean(agriculture$Y)
tapply(agriculture$Y, agriculture$B, mean) - mean(agriculture$Y)



######################################################
## Eg 7.3
######################################################
tree <- data.frame(
  A=gl(3,20,60),
  B=gl(4,5,60),
  Y=c(23, 25, 21, 14, 15, 20, 17, 11, 26, 21, 
      16, 19, 13, 16, 24, 20, 21, 18, 27, 24,
      28, 30, 19, 17, 22, 26, 24, 21, 25, 26,
      19, 18, 19, 20, 25, 26, 26, 28, 29, 23,
      18, 15, 23, 18, 10, 21, 25, 12, 12, 22, 
      19, 23, 22, 14, 13, 22, 13, 12, 22, 19)
)

## anova
fit3.aov <- aov(Y ~ A+B+A:B, data=tree)
anova(fit3.aov)


## plot univariate effects
plot.design(Y ~ A+B+A:B, data=tree)

## effects \alpha_i \beta_j \delta_ij
model.tables(fit3.aov)

## normality assumption
tapply(tree$Y, tree$A, shapiro.test) 
tapply(tree$Y, tree$B, shapiro.test) 

## equal variance: Bartlett Test
bartlett.test(Y~A, data=tree)
bartlett.test(Y~B, data=tree)

##################################################################
## 2^(4-1) fractorial factorial
library(DoE.base) # for oa.design

lohi <- c("0","1")
fnames <- list(D=lohi,C=lohi,B=lohi,A=lohi)
d <-oa.design(factor.names=fnames,nruns=8, nfactors=4, nlevels=2,randomize=F)
cancer.design <-cbind(d[,4],d[,3],d[,2],d[,1])
cancer.design

death.c<-scan()
107 94 121 101 81 103 90 95
mean(1/death.c)

years<-scan()
18626 18736 18701 18686 18745 18729 18758 18792

log.rates<-log(death.c/years)
log.rates

# Below we analyse number of deaths from cancer and
# the log death rate

logcancer.df<-data.frame(log.rates,cancer.design)
logcancer.df

rm(lohi,death.c,log.rates,d,cancer.design)

logcancer.aov <- aov(log.rates~.^2,logcancer.df)
summary(logcancer.aov)
logcancer.aov$effects
model.tables(logcancer.aov,type="effects")

library(gplots)
qqnorm.aov(logcancer.aov) # plot half-normal quantitle

effects<-abs(logcancer.aov$effects[-1])
qq <- qqnorm(effects)  # generic function :qqnorm()
text(qq$x, qq$y-0.006, labels = names(effects))

##
library(FrF2)
DanielPlot(logcancer.aov,datax=FALSE,half=TRUE,autolab=FALSE)

# Main Effects and Interaction Plots
MEPlot(logcancer.aov)
IAPlot(logcancer.aov) 

# provide information of the alias structure
aliases(logcancer.aov,code=TRUE) 
# A:B=C:D; A:C=B:D;  A:D=B:C

FrF2(16, 7)





