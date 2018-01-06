#import 
library(ggplot2)
library(ggthemes)
library(plyr)
library(gridExtra)
library(grid)


#clear memory
rm(list=ls())

#read data from direct_marketing.csv
direct_marketing <- read.csv2("direct_marketing.csv",fileEncoding="UTF-16LE", sep="\t")

#shared cols
visit_col = direct_marketing$visit
conversion_col = direct_marketing$conversion
spend_col = direct_marketing$spend

#Visit according to recency
counts <- ddply(direct_marketing, .(recency, visit), nrow)
visited_counts <- subset(counts, visit == 1)[,"V1"]
visited_counts

counts$visit <- replace(counts$visit,counts$visit == 1, "Visited")
counts$visit <- replace(counts$visit,counts$visit == 0, "Not Visited")
counts

total_customers = aggregate(V1 ~ recency, counts, sum)[,"V1"]
total_customers




#Conversion according to recency
conversion_data <- ddply(direct_marketing, .(recency, conversion), nrow)
conversion_data

conversion_data$conversion <- replace(conversion_data$conversion,conversion_data$conversion == 1, "Shopped")
conversion_data$conversion <- replace(conversion_data$conversion,conversion_data$conversion == 0, "Not Shopped")
conversion_data

#how many people shopped
conversion_counts <- subset(conversion_data, conversion == "Shopped")[,"V1"]
conversion_counts


total_customers_col <- append(total_customers, sum(total_customers))

conversion_counts_col <- append(conversion_counts, sum(conversion_counts))

not_conversion_counts <- total_customers - conversion_counts
not_conversion_counts_col <- append(not_conversion_counts, sum(not_conversion_counts)) 

recency_table <- rbind(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "Row Sum"), conversion_counts_col, not_conversion_counts_col, total_customers_col)
rownames(recency_table) <- c("Recency","Shopped", "Not Shopped", "Col Sum")

# grid.table(recency_table)

recency_chi_table = rbind(conversion_counts, not_conversion_counts)
recency_chi_table

chisq.test(recency_chi_table)




coversion_dt <- data.frame(obj = c(1:12), val = conversion_counts)

ggplot(coversion_dt, aes(x = obj, y = val)) + 
  geom_bar(stat = "identity", color="red", fill="lightblue") +
  scale_x_continuous(breaks=seq(1,12,by=1)) +
  geom_text(aes(label = val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Volume of customers who shopped on website within 3 months after being contacted according to recency", x="Month", y="Population")


conversion_pct <-round(conversion_counts/total_customers*100, 3)

conversion_pct

conversion_pct_dt <- data.frame(obj = c(1:12), val = conversion_pct)

ggplot(conversion_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="red") + 
  geom_point(shape = 23,size = 3, fill = "blue") +
  scale_x_continuous(breaks=seq(1,12,by=1)) +
  scale_y_continuous(breaks=seq(0,5,by=0.2)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Percentage of Customers who shopped on website within 3 months after being contacted according to recency", x="Month", y="Percentage")



#history segment
#Visit according to history segment
history_seg = direct_marketing$history_segment
history_seg

dt_hist_seg_and_visit = data.frame(hist_seg = history_seg, visited = visit_col)

dt_hist_seg_and_visit$visited <- replace(dt_hist_seg_and_visit$visited, dt_hist_seg_and_visit$visited == 1, "Visited")
dt_hist_seg_and_visit$visited <- replace(dt_hist_seg_and_visit$visited, dt_hist_seg_and_visit$visited == 0, "Not Visited")

dt_hist_seg_and_visit

dt_hist_seg_and_visit <- ddply(dt_hist_seg_and_visit, .(hist_seg, visited), nrow)
dt_hist_seg_and_visit

dt_hist_seg_and_visit$hist_seg
counts$recency

dt_hist_seg_and_visit["fun"] = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)

dt_hist_seg_and_visit

hist_seg_total_customers = aggregate(V1 ~ fun, dt_hist_seg_and_visit, sum)[,"V1"]
hist_seg_total_customers

#Conversion according to history segment
dt_hist_seg_conversion <- ddply(direct_marketing, .(history_segment, conversion), nrow)
dt_hist_seg_conversion

hist_seg_conversion_counts <- subset(dt_hist_seg_conversion, conversion == 1)[,"V1"]
hist_seg_conversion_counts

hist_seg_not_conversion_counts = hist_seg_total_customers - hist_seg_conversion_counts


hist_seg_total_col <- append(hist_seg_total_customers, sum(hist_seg_total_customers))
hist_seg_conversion_col <- append(hist_seg_conversion_counts, sum(hist_seg_conversion_counts))
hist_seg_not_conversion_col <- append(hist_seg_not_conversion_counts, sum(hist_seg_not_conversion_counts)) 

hist_seg_table <- rbind(c("$0~$100","$100~$200", "$200~$350","$350~$500","$500~$750","$750~$1000","$1000+","Row Sum"), 
                        hist_seg_conversion_col, hist_seg_not_conversion_col, hist_seg_total_col)
rownames(hist_seg_table) <- c("History Segment","Shopped", "Not Shopped", "Col Sum")

# grid.table(hist_seg_table)

hist_seg_chi_table = rbind(hist_seg_conversion_counts, hist_seg_not_conversion_counts)
hist_seg_chi_table

chisq.test(hist_seg_chi_table)



df_hist_seg_coversion <- data.frame(obj = c(1:7), val = hist_seg_conversion_counts)

ggplot(df_hist_seg_coversion, aes(x = obj, y = val)) + 
  geom_bar(stat = "identity", color="gray41", fill="violetred1") +
  scale_x_continuous(breaks=seq(1,7,by=1)) +
  geom_text(aes(label = val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Volume of customers who shopped on website within 3 months after being contacted according to history segment", x="History Segment", y="Population") +
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))


hist_seg_conversion_pct <-round(hist_seg_conversion_counts/hist_seg_total_customers*100, 3)

hist_seg_conversion_pct

hist_seg_conversion_pct_dt <- data.frame(obj = c(1:7), val = hist_seg_conversion_pct)

ggplot(hist_seg_conversion_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="red") + 
  geom_point(shape = 23,size = 3, fill = "blue") +
  scale_y_continuous(breaks=seq(0,5,by=0.2)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Percentage of Customers who shopped on website within 3 months after being contacted according to history segment", x="History Segment", y="Percentage") +
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))




#number of customers purchased only men and shopped
men_shopped_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$conversion==1 & direct_marketing$womens ==0, ])
men_total_num = nrow(direct_marketing[direct_marketing$men == 1 & direct_marketing$womens == 0, ])
men_not_shopped_num = men_total_num - men_shopped_num 

men_shopped_num
men_total_num

#number of customers purchased only women and shopped
women_shopped_num = nrow(direct_marketing[direct_marketing$mens == 0 & direct_marketing$conversion==1 & direct_marketing$womens ==1, ])
women_not_shopped_num = nrow(direct_marketing[direct_marketing$mens == 0 & direct_marketing$conversion==0 & direct_marketing$womens ==1, ])
women_total_num = nrow(direct_marketing[direct_marketing$men == 0 & direct_marketing$womens == 1, ])

women_shopped_num
women_not_shopped_num

#number of customers purchased both and visited
both_shopped_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$conversion ==1 & direct_marketing$womens ==1, ])
both_not_shopped_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$conversion ==0 & direct_marketing$womens ==1, ])
both_total_num = nrow(direct_marketing[direct_marketing$men == 1 & direct_marketing$womens == 1, ])




category_table <- rbind(c("Men","Women", "Both","Row Sum"), 
                        c(men_shopped_num, women_shopped_num, both_shopped_num, sum(men_shopped_num, women_shopped_num, both_shopped_num)),
                        c(men_not_shopped_num, women_not_shopped_num, both_not_shopped_num, sum(men_not_shopped_num, women_not_shopped_num, both_not_shopped_num)),
                        c(men_total_num, women_total_num, both_total_num, sum(men_total_num, women_total_num, both_total_num)))

rownames(category_table) <- c("Category","Shopped", "Not Shopped", "Col Sum")

# grid.table(category_table)

category_chi_table = rbind(c(men_shopped_num, women_shopped_num, both_shopped_num), 
                           c(men_not_shopped_num, women_not_shopped_num, both_not_shopped_num))

category_chi_table
chisq.test(category_chi_table)



df_gender_shopped = data.frame(gender = c(1,1,2,2,3,3), 
                               if_shopped=c("Shopped", "Not Shopped","Shopped", "Not Shopped", "Shopped", "Not Shopped"),
                               V1 = c(men_shopped_num, (men_total_num-men_shopped_num), women_shopped_num, women_not_shopped_num, both_shopped_num, both_not_shopped_num))


df_gender_shopped

ggplot(df_gender_shopped, aes(x = gender, y = V1, fill=if_shopped)) + 
  geom_bar(stat = "identity", width = 0.6) +
  scale_x_continuous(breaks=seq(1,3,by=1)) +
  geom_text(data = df_gender_shopped, aes(x = gender, y = V1,label = V1), size=4,vjust = 2) +
  labs(title = "Comparison of Customers who shopped on website within 3 months after being contacted according to Category", x="Category purchased before", y="Population") +
  scale_x_discrete(limits=c(1:3),labels=c("Men", "Women", "Both"))


gender_shopped_pct <-round(c(men_shopped_num, women_shopped_num, both_shopped_num)/c(men_total_num, women_total_num, both_total_num)*100,3)

gender_shopped_pct_dt <- data.frame(obj = c(1:3), val = gender_shopped_pct)

ggplot(gender_shopped_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="darkorchid1") + 
  geom_point(shape = 23,size = 3, fill = "darkorchid4") +
  scale_y_continuous(breaks=seq(0,5,by=1)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -1.1, hjust = 0.9)) + 
  labs(title = "Percentage of Customers who shopped on website within 3 months after being contacted according to category", x="Category purchased before", y="Percentage")+
  scale_x_discrete(limits=c(1:3),labels=c("Men", "Women", "Both"))



#Visit according to ZipCode
zip_code = direct_marketing$zip_code
zip_code

dt_zip_code_and_visit = data.frame(zip_c = zip_code, visited = visit_col)

dt_zip_code_and_visit$visited <- replace(dt_zip_code_and_visit$visited, dt_zip_code_and_visit$visited == 1, "Visited")
dt_zip_code_and_visit$visited <- replace(dt_zip_code_and_visit$visited, dt_zip_code_and_visit$visited == 0, "Not Visited")

dt_zip_code_and_visit

dt_zip_code_and_visit <- ddply(dt_zip_code_and_visit, .(zip_c, visited), nrow)
dt_zip_code_and_visit


dt_zip_code_and_visit["fun"] = c(1,1,2,2,3,3)

dt_zip_code_and_visit

zip_code_total_customers = aggregate(V1 ~ fun, dt_zip_code_and_visit, sum)[,"V1"]
zip_code_total_customers

#Conversion according to zip code
dt_zip_code_conversion <- ddply(direct_marketing, .(zip_code, conversion), nrow)
dt_zip_code_conversion

zip_code_conversion_counts <- subset(dt_zip_code_conversion, conversion == 1)[,"V1"]
zip_code_conversion_counts



zip_code_not_conversion_counts <- zip_code_total_customers - zip_code_conversion_counts

zip_code_total_col <- append(zip_code_total_customers, sum(zip_code_total_customers))
zip_code_conversion_col <- append(zip_code_conversion_counts, sum(zip_code_conversion_counts))
zip_code_not_conversion_col <- append(zip_code_not_conversion_counts, sum(zip_code_not_conversion_counts))

zip_code_table <- rbind(c("Rural", "Surburban", "Urban", "Row Sum"), zip_code_conversion_col, 
                        zip_code_not_conversion_col, zip_code_total_col)
rownames(zip_code_table) <- c("Zip code","Shopped", "Not Shopped", "Col Sum")

# grid.table(zip_code_table)

zip_chi_table = rbind(zip_code_conversion_counts, zip_code_not_conversion_counts)
zip_chi_table

chisq.test(zip_chi_table)



df_zip_code_coversion <- data.frame(obj = c(1:3), val = zip_code_conversion_counts)

ggplot(df_zip_code_coversion, aes(x = obj, y = val)) + 
  geom_bar(stat = "identity", color="mediumvioletred", fill="mediumorchid3", width = 0.6) +
  scale_x_continuous(breaks=seq(1,3,by=1)) +
  geom_text(aes(label = val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Volume of customers who shopped on website within 3 months after being contacted according to Zip Code", x="Zip Code", y="Population") +
  scale_x_discrete(limits=c(1:3),labels=c("Rural", "Surburban", "Urban"))


zip_code_conversion_pct <-round(zip_code_conversion_counts/zip_code_total_customers*100, 3)

zip_code_conversion_pct

zip_code_conversion_pct_dt <- data.frame(obj = c(1:3), val = zip_code_conversion_pct)

ggplot(zip_code_conversion_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="red") + 
  geom_point(shape = 23,size = 3, fill = "blue") +
  scale_y_continuous(breaks=seq(0,5,by=0.2)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Percentage of Customers who shoped website within 3 months after being contacted according to Zip Code", x="Zip Code", y="Percentage") +
  scale_x_discrete(limits=c(1:3),labels=c("Rural", "Surburban", "Urban"))






#Visit according to Newbie
newbie = direct_marketing$newbie
newbie

dt_newbie_and_visit = data.frame(nb = newbie, visited = visit_col)

dt_newbie_and_visit$visited <- replace(dt_newbie_and_visit$visited, dt_newbie_and_visit$visited == 1, "Visited")
dt_newbie_and_visit$visited <- replace(dt_newbie_and_visit$visited, dt_newbie_and_visit$visited == 0, "Not Visited")

dt_newbie_and_visit

dt_newbie_and_visit <- ddply(dt_newbie_and_visit, .(nb, visited), nrow)
dt_newbie_and_visit


dt_newbie_and_visit["fun"] = c(1,1,2,2)

dt_newbie_and_visit

newbie_total_customers = aggregate(V1 ~ fun, dt_newbie_and_visit, sum)[,"V1"]
newbie_total_customers

#Conversion according to newbie
dt_newbie_conversion <- ddply(direct_marketing, .(newbie, conversion), nrow)
dt_newbie_conversion

newbie_conversion_counts <- subset(dt_newbie_conversion, conversion == 1)[,"V1"]
newbie_conversion_counts



newbie_not_conversion_counts = newbie_total_customers - newbie_conversion_counts

newbie_total_col <- append(newbie_total_customers, sum(newbie_total_customers))
newbie_conversion_col <- append(newbie_conversion_counts, sum(newbie_conversion_counts))
newbie_not_conversion_col <- append(newbie_not_conversion_counts, sum(newbie_not_conversion_counts))

newbie_table <- rbind(c("Newbie", "Not Newbie", "Row Sum"), newbie_conversion_col, 
                      newbie_not_conversion_col, newbie_total_col)
rownames(newbie_table) <- c("Newbie","Shopped", "Not Shopped", "Col Sum")

# grid.table(newbie_table)

newbie_chi_table = rbind(newbie_conversion_counts, newbie_not_conversion_counts)
newbie_chi_table

chisq.test(newbie_chi_table)




df_newbie_coversion <- data.frame(obj = c(1:2), val = newbie_conversion_counts)

ggplot(df_newbie_coversion, aes(x = obj, y = val)) +
  geom_bar(stat = "identity", color="khaki2", fill="mediumturquoise", width=0.5) +
  scale_x_continuous(breaks=seq(1,2,by=1)) +
  geom_text(aes(label = val, vjust = -0.8, hjust = 0.5)) +
  labs(title = "Volume of customers who shopped on website within 3 months after being contacted according to Newbie", x="Newbie", y="Population") +
  scale_x_discrete(limits=c(1:2),labels=c("Not Newbie", "Newbie"))


newbie_conversion_pct <-round(newbie_conversion_counts/newbie_total_customers*100, 3)

newbie_conversion_pct

newbie_conversion_pct_dt <- data.frame(obj = c(1:2), val = newbie_conversion_pct)
newbie_conversion_pct_dt


ggplot(newbie_conversion_pct_dt, aes(x = obj, y = val), size = 5) +
  geom_bar(stat = "identity", color="ivory2", fill="mediumspringgreen", width=0.5) +
  scale_y_continuous(breaks=seq(0,5,by=0.2)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) +
  labs(title = "Percentage of Customers who shopped on website within 3 months after being contacted according to Newbie", x="Newbie", y="Percentage") +
  scale_x_discrete(limits=c(1:2),labels=c("Not Newbie", "Newbie"))





#Visit according to Channel
channel = direct_marketing$channel
channel

dt_channel_and_visit = data.frame(chn = channel, visited = visit_col)

dt_channel_and_visit$visited <- replace(dt_channel_and_visit$visited, dt_channel_and_visit$visited == 1, "Visited")
dt_channel_and_visit$visited <- replace(dt_channel_and_visit$visited, dt_channel_and_visit$visited == 0, "Not Visited")

dt_channel_and_visit

dt_channel_and_visit <- ddply(dt_channel_and_visit, .(chn, visited), nrow)
dt_channel_and_visit


dt_channel_and_visit["fun"] = c(1,1,2,2,3,3)

dt_channel_and_visit

channel_total_customers = aggregate(V1 ~ fun, dt_channel_and_visit, sum)[,"V1"]
channel_total_customers

#Conversion according to channels
dt_channel_conversion <- ddply(direct_marketing, .(channel, conversion), nrow)
dt_channel_conversion

channel_conversion_counts <- subset(dt_channel_conversion, conversion == 1)[,"V1"]
channel_conversion_counts



channel_not_conversion_counts = channel_total_customers - channel_conversion_counts

channel_total_col <- append(channel_total_customers, sum(channel_total_customers))
channel_conversion_col <- append(channel_conversion_counts, sum(channel_conversion_counts))
channel_not_conversion_col <- append(channel_not_conversion_counts, sum(channel_not_conversion_counts))

channel_table <- rbind(c("Multichannel", "Phone","Web", "Row Sum"), channel_conversion_col, 
                       channel_not_conversion_col, channel_total_col)
rownames(channel_table) <- c("Channel","Shopped", "Not Shopped", "Col Sum")

# grid.table(channel_table)

channel_chi_table = rbind(channel_conversion_counts, channel_not_conversion_counts)
channel_chi_table

chisq.test(channel_chi_table)





df_channel_coversion <- data.frame(obj = c(1:3), val = channel_conversion_counts)

ggplot(df_channel_coversion, aes(x = obj, y = val)) + 
  geom_bar(stat = "identity", color="dodgerblue", fill="goldenrod1", width = 0.6) +
  scale_x_continuous(breaks=seq(1,3,by=1)) +
  geom_text(aes(label = val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Volume of customers who shopped on website within 3 months after being contacted according to Channel", x="Channels", y="Population") +
  scale_x_discrete(limits=c(1:3),labels=c("Phone", "Web", "MultiChannel"))


channel_conversion_pct <-round(channel_conversion_counts/zip_code_total_customers*100, 3)

channel_conversion_pct

channel_conversion_pct_dt <- data.frame(obj = c(1:3), val = channel_conversion_pct)

ggplot(channel_conversion_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="red") + 
  geom_point(shape = 23,size = 3, fill = "blue") +
  scale_y_continuous(breaks=seq(0,5,by=0.2)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Percentage of Customers who shopped on website within 3 months after being contacted according to Channel", x="Channels", y="Percentage") +
  scale_x_discrete(limits=c(1:3),labels=c("Phone", "Web", "MultiChannel"))














