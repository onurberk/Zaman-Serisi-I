a <- read.csv("C:/Users/Onur/OneDrive/Masaüstü/data_return_w-zaman serisi.csv")

summary(a)

par(mfrow=c(2,2))
plot(a$r_bist, type="l")
plot(a$r_garan, type="l")
plot(a$r_vestl, type="l")
plot(a$r_usd, type="l")


#piyasa modeli

reg <- lm(r_garan~r_bist, data=a)
summary(reg)

linrf <- lm(r_garan~r_bist+r_usd, data=a)
summary(linrf)

urss <- sum(residuals(linrf)^2)

linrr <- lm(r_garan~1, data=a)
summary(linrr)

rrss <- sum(residuals(linrr)^2)
k <- length(coef(linrf))
m <- k-1
df2 <- nrow(a)-k

f_value <- ((rrss-urss)/urss)*(df2/m)
fcrit <- qf(0.95, m, df2)

# F_value > Fcrit o zaman Ho hipotezini reddedebiliriz

install.packages("car")
library(car)
linearHypothesis(linrf, c("r_bist=0", "r_usd=0"))
linearHypothesis(linrf, c("r_bist=1", "r_usd=0"))

