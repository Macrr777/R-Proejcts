
data("mtcars")
mtcars

mtcars_x<- prcomp(mtcars[,c(1:7,10,11)], centre=TRUE, scale=TRUE)
str(mtcars_x)
install.packages ("ggplot2")
library(ggplot2)

#dot plot
ggplot(mtcars, aes(x=mpg, y=am)) + 
  geom_point(col="darkblue", size=1.8)+
  labs(title="DOT PLOT", 
       subtitle="mpg vs am", 
       caption="source: mtcars")

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(mtcars_x, labels = rownames(mtcars))
