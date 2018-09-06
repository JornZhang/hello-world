######################################################
### Lecture 3
######################################################

cov(iris)
cov(iris[,-5])

###########################################################################
people <- read.csv("people2.csv", header = TRUE)
table(people$Eye.Color)
table(people$Eye.Color, people$Sex)
table(people$Eye.Color, people$Height.Cat, people$Sex)

tab1 <- table(people$Eye.Color, people$Sex)
prop.table(tab1)
prop.table(tab1, margin = 2)

addmargins(tab1)
addmargins(tab1, margin = 2)

warpbreaks
xtabs(breaks~wool+tension, warpbreaks)


str(Titanic)
Titanic
ftable(Titanic)
as.data.frame(Titanic)


######################################################
hist(trees$Height)
hist(trees$Height, freq = F, breaks = 10)

curve(dnorm(x, mean=mean(trees$Height), sd=sd(trees$Height)), add= T)

stem(trees$Height)
stem(trees$Height, scale = 0.6)



######################################################
library(MASS)
caith
caith <- as.matrix(caith)
op <- par(mfrow=c(1,3))
barplot(apply(caith, 2, sum))
barplot(caith, legend.text=T)
barplot(caith, beside=T, legend.text=T)
par(op)


pie(apply(caith, 2, sum))
pie(apply(caith, 1, sum))



######################################################
boxplot(count ~ spray, data = InsectSprays)


######################################################
plot(Volume~Girth, trees)
abline(coef(lm(Volume~Girth, trees)))

pairs(iris)
pairs(~Sepal.Length+Sepal.Width+Petal.Length, iris)
coplot(Sepal.Length~Sepal.Width|Species, iris)
   
library(lattice)
xyplot(Sepal.Length~Sepal.Width|Species, iris, auto.key=T)
xyplot(Sepal.Length~Sepal.Width, iris, groups = Species, auto.key=T)




########################################################
x <- rnorm(100)
qqnorm(x)
qqline(x)

qqplot(x, rnorm(100))
abline(0,1)

qq(voice.part ~ height, aspect = 1, data = singer,
   subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))


qqmath(~ rnorm(100), distribution = function(p) qt(p, df = 10))
qqmath(~ height | voice.part, aspect = "xy", data = singer,
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
vp.comb <-
  factor(sapply(strsplit(as.character(singer$voice.part), split = " "),
                "[", 1),
         levels = c("Bass", "Tenor", "Alto", "Soprano"))
vp.group <-
  factor(sapply(strsplit(as.character(singer$voice.part), split = " "),
                "[", 2))
qqmath(~ height | vp.comb, data = singer,
       groups = vp.group, auto.key = list(space = "right"),
       aspect = "xy",
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })



####### Type 1 #######
### Skewed to right (positive skew)
ex1 <- rweibull(300, shape = 1.5, scale = 1)
ex1 <- ex1 - mean(ex1)

op <- par(mfrow=c(2,2))
qqnorm(ex1)
qqline(ex1)

hist(ex1, freq = F)
curve(dnorm(x, mean=mean(ex1), sd=sd(ex1)), add= T)

####### Type 2 #######
### Skewed to left (negative skew)
ex2 <- max(ex1) - ex1
qqnorm(ex2)
qqline(ex2)

hist(ex2, freq = F)
curve(dnorm(x, mean=mean(ex2), sd=sd(ex2)), add= T)

install.packages("moments")
library(moments)

skewness(ex1)
kurtosis(ex1)


skewness(ex2)
kurtosis(ex2)


####### Type 3 #######
### (positive kurtosis)
ex3 <- c(runif(10, -7, -3), runif(10, 3, 7), rnorm(280))
qqnorm(ex3)
qqline(ex3)
hist(ex3, freq = F)
curve(dnorm(x, mean=mean(ex3), sd=sd(ex3)), add= T)

skewness(ex3)
kurtosis(ex3)


####### Type 4 #######
### (negative kurtosis)
ex4 <- rnorm(240)
ex4 <- ex4[ex4 >= -2 & ex4 <= 2]
ex4 <- c(ex4, runif(30, -2, -1), runif(30, 1, 2))
qqnorm(ex4)
qqline(ex4)


hist(ex4, freq = F)
curve(dnorm(x, mean=mean(ex4), sd=sd(ex4)), add= T)

skewness(ex4)
kurtosis(ex4)








