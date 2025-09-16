

### Sem 1
## oppgave 1

summary(mtcars)

sapply(mtcars, mean)
sapply(mtcars, median)
sapply(mtcars, varians)

deskriptiv_stat <- function(x) c(mean = round(mean(x),4),
                                 median = round(median(x),4),
                                 varians = round(var(x),4))

sapply(mtcars, deskriptiv_stat) 
t(sapply(mtcars, deskriptiv_stat)) #transpose




row.names(mtcars)

mtcars$brand <- row.names(mtcars)

row.names(mtcars) <- NULL


order(mtcars$mpg, decreasing = T)
mtcars <- mtcars[order(mtcars$mpg, decreasing = T),] # Justerer etter data


mtcars[1:3,]

mtcars <- mtcars[order(mtcars$mpg, decreasing = F),]

mtcars[1:3, c("brand", "mpg")]

############


mtcars$producer <- substr(mtcars$brand,1,3)


mtcars$producer <- gsub(" .*$", "", mtcars$brand) # 

### f)


aggregate(mtcars, list(mtcars$producer), mean )


df <- aggregate(Filter(is.numeric, mtcars), list(mtcars$producer), mean )


### Oppgave 2

data(mtcars) 
row.names(mtcars) <- NULL

rows <- sample(1:32, 32, replace = T)

mtcars[rows,]


### Oppgave 3

plot(x = mtcars$hp, y = mtcars$mpg, pch = 16, col = "midnightblue")


ols_b <- lm(mpg ~ hp, data = mtcars)

mtcars$pred1 <- predict(ols_b,mtcars) # Kun dfen

mtcars |> View()


plot(x = mtcars$hp, y = mtcars$mpg, pch = 16, col = "midnightblue")
lines(x = mtcars$hp, y =  mtcars$pred1, type = "l", col = "red")


ols_c <- lm(mpg ~ hp + I(hp^2), data = mtcars) ## evt lage ny hp variabel.
mtcars$pred2 <- predict(ols_c,mtcars) # Kun dfen

mtcars <- mtcars[order(mtcars$hp),]

plot(x = mtcars$hp, y = mtcars$mpg, pch = 16, col = "midnightblue")
lines(x = mtcars$hp, y =  mtcars$pred1, type = "l", col = "red")
lines(x = mtcars$hp, y =  mtcars$pred2, type = "l", col = "cyan")





