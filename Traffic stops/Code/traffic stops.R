library(data.table)
library(tidyverse)
library(ggplot2)

getwd()
dat <- fread("./Traffic stops/Data/police_project.csv")
dat <- data.frame(dat)
dim(dat)
head(dat)
colnames(dat)

# Q1 Do men or women speed more often?

Q1 <- dat %>% 
  select(driver_gender, violation) %>%
  filter(violation == "Speeding") %>%
  group_by(driver_gender) %>%
  summarize(freq = n())

Q1 <- data.frame(Q1)
Q1

ggplot(data = Q1, aes(x = driver_gender, y = freq))+
  geom_bar(stat="identity", fill = "steelblue")+
  geom_text(aes(label = freq), vjust = 1.6)+
  theme_minimal()

# In the Rhode Island, from 2005 to 2013, we have 32979 speeding records for the man, while we have 15482 for the women.
# therefore, in this sample data, men speed more often.

#Does gender affect who gets searched during a stop?
## EDA
Q2 <- dat %>%
  select(driver_gender, search_conducted) %>%
  filter(driver_gender %in% c("F","M"))

Q2 <- data.frame(Q2)

### cross table
crt <- table(Q2$driver_gender, Q2$search_conducted)
crt

### percentage table
propt <- data.frame(round(prop.table(crt,1),4))
colnames(propt) <- c("driver_gender","search_conducted","percentage")
propt

### visualization
crt <- data.frame(crt)
colnames(crt) <- c("driver_gender","search_conducted","frequency")

ggplot(crt, aes(x = driver_gender, y = search_conducted))+
  geom_tile(aes(fill = frequency)) +
  geom_text(aes(label = frequency), color = "black", fontface = "bold", size = 5)

propt$labelpos <- ifelse(propt$search_conducted == "TRUE", propt$percentage/2, 1- propt$percentage/2)

ggplot(dat = propt, aes(x = driver_gender, y = percentage, fill = search_conducted))+
  geom_bar(position = "fill", stat = "identity", color = "black", width = 0.9)+
  scale_y_continuous(label = scales::percent)+
  geom_text(aes(label = paste0(percentage*100, "%"), y = labelpos),size =2)

# We have 23511 females, 62895 males in the sample of traffic stops. 471 out of 23511 (around 2 %) females were searched 
# during stop.; 2725 out of 62895 (around 4.33 %) males were searched during the stop
# To see is there signifcant assocations between gender and search conductions, we need to use chi-square test
chisq.test(Q2$driver_gender, Q2$search_conducted)

# p value (2.23-16) less than significance level (0.05) indicates that there is a significant assocaition
# between driver's gender and search conduction. In other words, gender affects who get searched during a stop.
# We can conduct linear regression if you interest how gender affect who get searched during a stop. 
# For model accuracy, we can also consider other covariates like stop time, driver gender ect.




