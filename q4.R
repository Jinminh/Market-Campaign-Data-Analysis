#import 
library(ggplot2)
library(ggthemes)
library(plyr)

#clear memory
rm(list=ls())

get_spend <- function(colname){
  #linear regression of recency
  return(subset(direct_marketing, conversion == 1)[,c(colname,"spend")])
}

factor_to_num <- function(fac_item){
  fac_item = as.numeric(levels(fac_item))[fac_item]
}


#read data from direct_marketing.csv
direct_marketing <- read.csv2("direct_marketing.csv",fileEncoding="UTF-16LE", sep="\t")

#shared cols
visit_col = direct_marketing$visit
conversion_col = direct_marketing$conversion
spend_col = direct_marketing$spend


#Conversion according to recency
conversion_data <- ddply(direct_marketing, .(recency, conversion), nrow)
conversion_data

conversion_data$conversion <- replace(conversion_data$conversion,conversion_data$conversion == 1, "Shopped")
conversion_data$conversion <- replace(conversion_data$conversion,conversion_data$conversion == 0, "Not Shopped")
conversion_data

#how many people shopped
conversion_counts <- subset(conversion_data, conversion == "Shopped")[,"V1"]
conversion_counts


#Spend according to recency
spend_value <- as.numeric(as.character(direct_marketing$spend))
spend_value

spend_dt <- data.frame(month = direct_marketing$recency, spend = spend_value)
spend_dt

spend_dt_subset = subset(spend_dt, spend != 0)
spend_dt_subset

total_spend_month = aggregate(spend ~ month, spend_dt, sum)
total_spend_month

ggplot(total_spend_month, aes(x = month, y = spend)) + 
  geom_bar(stat = "identity", color="red", fill="lightblue") +
  scale_x_continuous(breaks=seq(1,12,by=1)) +
  geom_text(aes(label = spend, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Total spend within 3 months after being contacted according to recency", x="Month", y="Ponds")

mean= round(total_spend_month$spend/conversion_counts, 3)
mean

spend_mean_dt = data.frame(month = c(1:12), mean_val = mean)
spend_mean_dt

ggplot(spend_mean_dt, aes(x = month, y = mean_val)) + 
  geom_bar(stat = "identity", color="red", fill="lightblue") +
  scale_x_continuous(breaks=seq(1,12,by=1)) +
  geom_text(aes(label = mean_val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Average spend for shopped customers within 3 months after being contacted according to recency", x="Month", y="Ponds")


#linear regression of recency
recency_spend = get_spend("recency")

recency_spend$spend = as.numeric(levels(recency_spend$spend))[recency_spend$spend]

ggplot(recency_spend, aes(x=recency, y=spend)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(0,500,by=100))+
  labs(title = "Linear regression of recency against spend")



#history segment

#Conversion according to history segment
dt_hist_seg_conversion <- ddply(direct_marketing, .(history_segment, conversion), nrow)
dt_hist_seg_conversion

hist_seg_conversion_counts <- subset(dt_hist_seg_conversion, conversion == 1)[,"V1"]
hist_seg_conversion_counts


#Spend according to history segment
spend_value <- as.numeric(as.character(direct_marketing$spend))
spend_value

#for every customer, their spend and hist_seg
hist_seg_spend_dt <- data.frame(hist_seg = direct_marketing$history_segment, spend = spend_value)
hist_seg_spend_dt

#for every shopped customer, their spend and hist_seg
hist_seg_spend_dt_subset = subset(hist_seg_spend_dt, spend != 0)
hist_seg_spend_dt_subset

#sum of spend according to hist_seg
total_spend_hist_seg = aggregate(spend ~ hist_seg, hist_seg_spend_dt, sum)
total_spend_hist_seg

#hist, sum of spend according to hist_reg
ggplot(total_spend_hist_seg, aes(x = c(1:7), y = spend)) + 
  geom_bar(stat = "identity", color="gray41", fill="violetred1") +
  scale_x_continuous(breaks=seq(1,7,by=1)) +
  geom_text(aes(label = spend, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Total spend within 3 months after being contacted according to history segment", x="History Segment", y="Ponds") +
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))

hist_seg_mean= round(total_spend_hist_seg$spend/hist_seg_conversion_counts, 3)
hist_seg_mean

hist_seg_spend_mean_dt = data.frame(month = c(1:7), mean_val = hist_seg_mean)
hist_seg_spend_mean_dt

ggplot(hist_seg_spend_mean_dt, aes(x = c(1:7), y = mean_val)) + 
  geom_bar(stat = "identity", color="gray41", fill="violetred1") +
  #scale_x_continuous(breaks=seq(1,7,by=1)) +
  geom_text(aes(label = mean_val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Average spend for shopped customers within 3 months after being contacted according to history segment", x="History Segment", y="Ponds") +
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))




#linear regression of history_segment
his_seg_spend = get_spend("history_segment")
# as.character(his_seg_spend$history_segment)

levels(his_seg_spend$history_segment) <- 1:7

his_seg_spend$spend = as.numeric(levels(his_seg_spend$spend))[his_seg_spend$spend]
his_seg_spend$history_segment = as.numeric(levels(his_seg_spend$history_segment))[his_seg_spend$history_segment]


ggplot(his_seg_spend, aes(x=history_segment, y=spend)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))+
  scale_y_continuous(breaks=seq(0,600,by=100))+
  labs(title = "Linear regression of history segment against spend")



#linear regression of history
hist_spend = get_spend("history")

head(hist_spend, 20)

# hist_spend <- subset(hist_spend,(as.numeric(as.character(history)) > 29.99) & (as.numeric(as.character((history)) <715.96)))

hist_spend$spend = as.numeric(levels(hist_spend$spend))[hist_spend$spend]
hist_spend$history = as.numeric(levels(hist_spend$history))[hist_spend$history]

hist_spend <- subset(hist_spend, history < 715.96 & history > 29.99)
hist_spend

tail(sort(hist_spend$spend),15)
max(hist_spend$spend)

ggplot(hist_spend, aes(x=history, y=spend)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+
  scale_y_continuous(breaks=seq(0,600,by=100))+
  labs(title = "Linear regression of history against spend")

lm_hist_seg <- lm(hist_spend$spend~1+hist_spend$history) 
summary(lm_hist_seg)



