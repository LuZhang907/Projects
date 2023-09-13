library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(cowplot)
library(ordinal)
library(effects)

#getwd()
dat <- fread("../Data/food_order.csv")
dat <- data.frame(dat)
dim(dat)
head(dat)
str(dat)

dat <- dat[!grepl("Not given",dat$rating), ]#remove records without rating if possible
dat$cuisine_type <- as.factor(dat$cuisine_type) 
dat$cuisine_type <- relevel(dat$cuisine_type, ref = "American") # set American as reference level
dat$day_of_the_week <- as.factor(dat$day_of_the_week)
dat$rating <- as.factor(dat$rating)

dat <- drop_na(dat) # drop na
dat <- dat[!duplicated(dat),] # remove duplicate
str(dat)

table(dat$cuisine_type)
table(dat$day_of_the_week)
table(dat$rating)

#ct table cuisine type vs rating
table1 <- table(dat$cuisine_type, dat$rating)
table1

ptable1 <- data.frame(round(prop.table(table1,1),4))
colnames(ptable1) <- c("cuisine_type","rating","percentage")

table1 <- data.frame(table1)
colnames(table1) <- c("cuisine_type", "rating", "Freq")

ggplot(data=table1, aes(x=cuisine_type, y=rating))+
  geom_tile(aes(fill = Freq))+
  geom_text(aes(label = Freq),color = "black", fontface = "bold", size = 5)
#scale_fill_gradient("Freq",  low = "#c23728",high = "#1984c5")

# pt table cuisine type vs rating
ptable1$labelpos <- ifelse(ptable1$rating == 5, ptable1$percentage/2, ifelse(ptable1$rating ==4, 1-ptable1$percentage, 1-ptable1$percentage/2))

ptable1$labelpos[(ptable1$rating == 4) &(ptable1$cuisine_type == "Mediterranean")] <- 0.65
ptable1$labelpos[(ptable1$rating == 4) &(ptable1$cuisine_type == "Spanish")] <- 0.90
ptable1$labelpos[(ptable1$rating == 4) &(ptable1$cuisine_type == "Thai")] <- 0.80
ptable1$labelpos[(ptable1$rating == 4) &(ptable1$cuisine_type == "Vietnamese")] <- 0.50

ggplot(ptable1, aes(x = cuisine_type, y=percentage, fill = rating )) +
  geom_bar(position = "fill", stat = "identity", color = "black", width = 0.9)+
  scale_y_continuous(labels = scales::percent)+
  #scale_fill_manual(values = c("#c23728","#7E6148FF","#1984c5"))+
  geom_text(aes(label = paste0(percentage*100,"%"),y=labelpos),size = 2) 

# ct day of the week vs rating
table2 <- table(dat$day_of_the_week, dat$rating)
table2

ptable2 <- data.frame(round(prop.table(table2,1),4))
colnames(ptable2) <- c("day_of_the_week","rating","percentage")

table2 <- data.frame(table2)
colnames(table2) <- c("day_of_the_week", "rating", "Freq")

p1 <- ggplot(data=table2, aes(x=day_of_the_week, y=rating))+
  geom_tile(aes(fill = Freq))+
  geom_text(aes(label = Freq),color = "black", fontface = "bold", size = 5)
#scale_fill_gradient("Freq",  low = "#c23728",high = "#1984c5")

# pt day of the week vs rating
ptable2$labelpos <- ifelse(ptable2$rating == 5, ptable2$percentage/2, ifelse(ptable2$rating ==4, 1-ptable2$percentage, 1-ptable2$percentage/2))

p2 <- ggplot(ptable2, aes(x = day_of_the_week, y=percentage, fill = rating )) +
  geom_bar(position = "fill", stat = "identity", color = "black", width = 0.9)+
  scale_y_continuous(labels = scales::percent)+
  #scale_fill_manual(values = c("#c23728","#7E6148FF","#1984c5"))+
  geom_text(aes(label = paste0(percentage*100,"%"),y=labelpos),size = 2) 

plot_grid(p1, p2, nrow = 1, ncol =2)

# box-plots
bp1 <- ggplot(dat, aes(x = rating, y = cost_of_the_order, fill = rating))+
  geom_boxplot(size = 0.75) 

bp2 <- ggplot(dat, aes(x = rating, y = food_preparation_time, fill = rating))+
  geom_boxplot(size = 0.75)

bp3 <- ggplot(dat, aes(x = rating, y = delivery_time, fill = rating))+
  geom_boxplot(size = 0.75) 

plot_grid(bp1, bp2,bp3, nrow = 1, ncol =3)

# chi-square test
chisq.test(dat$cuisine_type, dat$rating)
chisq.test(dat$day_of_the_week, dat$rating)

# proportional odds ratio model
m1 <- clm(rating~cuisine_type+cost_of_the_order+day_of_the_week+food_preparation_time+delivery_time, data = dat)
summary(m1)

anova(m1, type =3)

nominal_test(m1)


plot(Effect(focal.predictors = c("cuisine_type"), m1),
     rug = FALSE)

par(mfrow=c(2,2))
plot(Effect(focal.predictors = c("day_of_the_week"), m1),
     rug = FALSE)
plot(Effect(focal.predictors = c("cost_of_the_order"), m1),
     rug = FALSE)
plot(Effect(focal.predictors = c("food_preparation_time"), m1),
     rug = FALSE)
plot(Effect(focal.predictors = c("delivery_time"), m1),
     rug = FALSE)


#m2 <- polr(rating~cuisine_type+cost_of_the_order+day_of_the_week+food_preparation_time+delivery_time, data = dat,Hess=TRUE)
#summary(m2)
#parallel assumption checking
#brant(m2)
#Effect(focal.predictors = c("cuisine_type"), m2)

