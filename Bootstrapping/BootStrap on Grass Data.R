library(readr)
d<- read.csv("C:/Users/choud/Downloads/Video/grass.csv")
View(d)
names(d)
levels(d$graze)

table(d$graze)
attach(d)

table(graze)

boxplot(rich ~ graze, las=1, ylab= "graze", xlab="rich", main="Rich and graze")

mean(rich[graze=="mow"])
mean(rich[graze=="unmow"])


test.stat1 <- abs(mean(rich[graze=="mow"])- mean(rich[graze=="unmow"]))
test.stat1


set.seed(112358)
n <-length(graze)
n
B <- 10000
variable <- rich


BootstrapSamples <- matrix(sample(variable,size=n*B, replace=TRUE), nrow=n,ncol=B)
dim(BootstrapSamples)


Boot.test.stat1 <- rep(0,B)
Boot.test.stat2 <- rep(0,B)


for (i in 1:B) {
  Boot.test.stat1[i] <- abs(mean(BootstrapSamples[1:3,i]) - 
                              mean(BootstrapSamples[4:9,i]))
}

test.stat1;

round(Boot.test.stat1[1:20],1)

(Boot.test.stat1 >=test.stat1)[1:20]

mean(Boot.test.stat1 >=test.stat1)

table(d)

plot(density(Boot.test.stat1),
     xlab=expression(group("|", bar(Yc)-bar(Ym),"|")))
