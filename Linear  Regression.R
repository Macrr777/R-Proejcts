library(dplyr)
library(ggplot2)

placement.df <- read.csv("C:/Users/choud/Desktop/Placement_Data_Full_Class.csv")

# select only relevant columns
colnames(placement.df)
placement.reg <- select(placement.df, degree_p, mba_p)
str(placement.reg)
placement.reg %>%  cor()
ggplot(placement.reg, aes(degree_p, mba_p)) +
  geom_point()
ggplot(placement.reg, aes(x = cut(degree_p, breaks = 5), y = mba_p)) +
  geom_boxplot()
model1 <- lm(mba_p~degree_p, data = placement.reg)
model1
ggplot(placement.reg, aes(degree_p, mba_p)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
