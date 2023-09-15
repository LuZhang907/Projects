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

# During a search, how often is the driver frisked?

Q3 <- dat %>%
  filter(grepl('Frisk', search_type))

Q3 <- data.frame(Q3)

table(Q3$search_type)

frisk_count = length(Q3$search_type)
total_count = length(dat$search_type)
frisk_ratio = frisk_count/total_count 

frisk_ratio

# During a search, driver has around 0.3% probabilities been frisked. In other words, if we have 1000 search during 
# a stop, around 3 drivers will be frisked.

# Which year had the least number of stops?
dat$year <- format(dat$stop_date, "%Y")

Q4 <- dat %>%
  group_by(year) %>%
  summarise(freq = n())

Q4 <- data.frame(Q4)
Q4_ordered <- Q4[order(Q4$freq),]
Q4_ordered

bar_plot <- barplot(Q4$freq,names.arg = Q4$year, xlab = "year", ylab = "number of stops",
                    ylim = c(0,15000),
                    col = "#1984c5")
text(bar_plot, Q4$freq+200, Q4$freq,cex = 1) 

# 2005 had the least number of stops

# How does drug activity change by time of day?
colnames(dat)
typeof(dat$stop_time)
Hour <- format(strptime(dat$stop_time, format = "%H:%M"), "%H")
head(Hour)

dat$hour <- Hour

Q5 <- dat %>%
  select(hour, drugs_related_stop) %>%
  filter(drugs_related_stop == "TRUE") %>%
  group_by(hour) %>%
  summarise(freq = n())

Q5 <- data.frame(Q5)
Q5

bar_plot2 <- barplot(Q5$freq,names.arg = Q5$hour, xlab = "time", ylab = "number of drug related stops",
                    ylim = c(0,150),
                    col = "#1984c5")
text(bar_plot2, Q5$freq+5, Q5$freq,cex = 1) 

# 0i represents 1 hour interval 0i:00 - 0(i):59, i = 0, 1,..., 23

# Do most stops occur at night?

Q6 <- dat %>%
  select(hour) %>%
  group_by(hour) %>%
  summarise(freq_stops = n())

Q6 <- data.frame(Q6)
Q6

bar_plot3 <- barplot(Q6$freq_stops, names.arg = Q6$hour,
                     xlab = "time", ylab = "number of stops",
                     ylim = c(0,8000),
                     col = "#1984c5")


text(bar_plot3, Q6$freq_stops+80, Q6$freq_stops, cex=1)


# save as new dataset for future Tableau
fwrite(dat, "./Traffic stops/Data/police_project_edited.csv")
# nope, according to the frequency bar chart, most stops occur at 10:00-11:00 AM. 
# there are three time intervals more likely to have stops: 23:00 PM -01:00 AM, 9:00 AM - 12:00 PM,
# and aroud 14:00 PM - 15:00 PM.








