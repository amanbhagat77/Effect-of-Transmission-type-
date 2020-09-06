data(mtcars)
library()
head(mtcars)
g <- ggplot(data = mtcars, aes(x = amf ,y = mpg, fill = am))
g <- g + geom_violin()
g

amf <- as.factor(mtcars$am)
cylf <- as.factor(mtcars$cyl)

fit <- lm(mpg~amf,data = mtcars)
fit1 <- lm(mpg~amf+cylf , data = mtcars)
fit2 <- lm(mpg ~ amf + wt, data = mtcars)
fit3 <- lm(mpg ~ amf + wt + cylf, data = mtcars)

g <- ggplot(mtcars, aes(x = amf, y = mpg))
g <- geom_boxplot()
g

 fit1 <- lm(mpg~ amf,data = mtcars)
 fit2 <- update(fit1 , mpg~ am + cyl)
 fit3 <- update(fit2 , mpg~ am + cyl + disp)
 fit4 <- update(fit3 , mpg~ am + cyl + disp + hp)
 fit5 <- update(fit4 , mpg~ am + cyl + disp + hp + drat)
 fit6 <- update(fit5 , mpg~ am + cyl + disp + hp + drat + wt)
 fit7 <- update(fit6 , mpg~ am + cyl + disp + hp + drat + wt + qsec)
 fit8 <- update(fit7 , mpg~ am + cyl + disp + hp + drat + wt + qsec + vs)
 fit9 <- update(fit8 , mpg~ am + cyl + disp + hp + drat + wt + qsec + vs + gear)
 fit10 <- update(fit9 ,mpg~ am + cyl + disp + hp + drat + wt + qsec + vs + gear + carb)
 anova(fit1, fit2, fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)
 
 
 fit21 <- update(fit1 , mpg~ am + cyl)
 fit22 <- update(fit21, mpg ~ am + cyl + wt)
 fit23 <- update(fit21, mpg ~ am + cyl + wt + hp)
 anova(fit1, fit21,fit22,fit23)