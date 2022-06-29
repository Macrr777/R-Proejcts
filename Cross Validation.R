install.packages("purrr")
library(purrr)
install.packages("dplyr")
library(dplyr)
install.packages("mtcars")
library(mtcars)
data(mtcars)
head(mtcars)
show(mtcars)
library(modelr)
cv <- crossv_kfold(mtcars, k=5)
cv
models1 <- map(cv$train, ~lm(mpg ~ wt + cyl + hp, data = .))
models2 <- map(cv$train, ~lm(mpg ~ wt + qsec + am, data = .))
models3 <- map(cv$train, ~lm(mpg ~ wt + qsec + hp, data = .))
get_pred <- function(model , test_data){
  data<- as.data.frame(test_data)
  pred<- add_predictions(data , model)
  return(pred)
}

pred1 <- map2_df(models1 , cv$test, get_pred, .id = "Run")
pred2 <- map2_df(models2 , cv$test, get_pred, .id = "Run")
pred3 <- map2_df(models3 , cv$test, get_pred, .id = "Run")

MSE1 <- pred1 %>% group_by(Run)%>%
  summarise(MSE = mean( mpg- pred)^2)
MSE2 <- pred2 %>% group_by(Run)%>%
  summarise(MSE = mean( mpg- pred)^2)
MSE3 <- pred3 %>% group_by(Run)%>%
  summarise(MSE = mean( mpg- pred)^2)

mean(MSE1$MSE)
mean(MSE2$MSE)
mean(MSE3$MSE)
