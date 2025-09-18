


x_1 <- rnorm(10000)
x_2 <- rnorm(10000)
x_3 <- rnorm(10000)
x_4 <- rnorm(10000)

y <- 0.5*x_1 + 1.9*x_2 - 0.2*x_2^2 - 0.6*x_3 + rnorm(10000, 0, 0.1)


df <- data.frame(y,x_1, x_2, x_3, x_4)

ols1 <- lm(y ~ x_2 + x_3, data = df)
ols2 <- lm(y~x_1 + x_2 + I(x_2^2) + x_3, data = df)
ols3 <- lm(y~x_1 +I(x_1^2)+ x_2 +I(x_2^2) + x_3 + I(x_3^2)+ x_4 +I(x_4^2), data = df)

summary(ols3)
summary(ols2)
