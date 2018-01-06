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

dt <- data.frame(obj = c(1:12), val = visited_counts)

#Draw histogram about how many customers visited website according to recency
ggplot(dt, aes(x = obj, y = val)) + 
  geom_bar(stat = "identity", color="red", fill="lightblue") +
  scale_x_continuous(breaks=seq(1,12,by=1)) +
  geom_text(aes(label = val, vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Volume of customers who visited website within 3 months after being contacted according to recency", x="Month", y="Population")


#Draw histogram to compare how many customers visited or not website according to recency
ggplot(counts, aes(x = recency, y = V1, fill=visit)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=seq(1,12,by=1)) +
  labs(title = "Comparison between Customers who visited website or not within 3 months after being contacted according to recency", x="Month", y="Population")


#Get total customers of each month 
total_customers = aggregate(V1 ~ recency, counts, sum)[,"V1"]
total_customers_col <- append(total_customers, sum(total_customers))

visited_counts_col <- append(visited_counts, sum(visited_counts))

not_visited_counts <- total_customers - visited_counts
not_visited_counts_col <- append(not_visited_counts, sum(not_visited_counts)) 

recency_table <- rbind(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "Row Sum"), visited_counts_col, not_visited_counts_col, total_customers_col)
rownames(recency_table) <- c("Recency","Visited", "Not Visited", "Col Sum")

grid.table(recency_table)

recency_chi_table = rbind(visited_counts, not_visited_counts)
recency_chi_table

chisq.test(recency_chi_table)

# #calculate the percentage of customers visited website according to month
# visited_pct <-round(visited_counts/total_customers*100)
# visited_pct
# visited_pct_dt <- data.frame(obj = c(1:12), val = visited_pct)
# 
# #Draw line graph about the percentage
# ggplot(visited_pct_dt, aes(x = obj, y = val), size = 5) + 
#   geom_line(color="red") + 
#   geom_point(shape = 23,size = 3, fill = "blue") +
#   scale_x_continuous(breaks=seq(1,12,by=1)) +
#   scale_y_continuous(breaks=seq(0,25,by=5)) +
#   expand_limits(x = 1, y = 0) +
#   geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
#   labs(title = "Percentage of Customers who visited website within 3 months after being contacted according to recency", x="Month", y="Percentage")





#history segment
#Visit according to history segment
history_seg = direct_marketing$history_segment
history_seg

#Create a matrix about how many customers visited website or not according to history segment   
dt_hist_seg_and_visit = data.frame(hist_seg = history_seg, visited = visit_col)
dt_hist_seg_and_visit$visited <- replace(dt_hist_seg_and_visit$visited, dt_hist_seg_and_visit$visited == 1, "Visited")
dt_hist_seg_and_visit$visited <- replace(dt_hist_seg_and_visit$visited, dt_hist_seg_and_visit$visited == 0, "Not Visited")
dt_hist_seg_and_visit <- ddply(dt_hist_seg_and_visit, .(hist_seg, visited), nrow)
dt_hist_seg_and_visit["fun"] = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)

#Print out matrix to see data
dt_hist_seg_and_visit

#Draw histogram to compare how many customers visited or not website according to history segment
ggplot(dt_hist_seg_and_visit, aes(x = fun , y = V1, fill=visited)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=seq(1,7,by=1)) +
  geom_text(data = dt_hist_seg_and_visit, aes(x = fun, y = V1,label = V1), size=4,vjust = 1) +
  labs(title = "Comparison between Customers who visited website or not within 3 months after being contacted according to history segment", x="History Segment", y="Population") +
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))

#Get total customers of each history segment
hist_seg_total_customers = aggregate(V1 ~ fun, dt_hist_seg_and_visit, sum)[,"V1"]
hist_seg_visited_counts <- subset(dt_hist_seg_and_visit, visited == "Visited")[,"V1"]
hist_seg_not_visited_counts = hist_seg_total_customers - hist_seg_visited_counts


hist_seg_total_col <- append(hist_seg_total_customers, sum(hist_seg_total_customers))
hist_seg_visited_col <- append(hist_seg_visited_counts, sum(hist_seg_visited_counts))
hist_seg_not_visited_col <- append(hist_seg_not_visited_counts, sum(hist_seg_not_visited_counts)) 

hist_seg_table <- rbind(c("$0~$100","$100~$200", "$200~$350","$350~$500","$500~$750","$750~$1000","$1000+","Row Sum"), 
                        hist_seg_visited_col, hist_seg_not_visited_col, hist_seg_total_col)
rownames(hist_seg_table) <- c("History Segment","Visited", "Not Visited", "Col Sum")

grid.table(hist_seg_table)

hist_seg_chi_table = rbind(hist_seg_visited_counts, hist_seg_not_visited_counts)
hist_seg_chi_table

chisq.test(hist_seg_chi_table)



#calculate the percentage of customers visited website according to history segment
hist_seg_visited_pct <-round(hist_seg_visited_counts/hist_seg_total_customers*100)
hist_seg_visited_pct_dt <- data.frame(obj = c(1:7), val = hist_seg_visited_pct)

#Draw line graph about the percentage
ggplot(hist_seg_visited_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="darkorchid1") + 
  geom_point(shape = 23,size = 3, fill = "darkorchid4") +
  scale_y_continuous(breaks=seq(0,25,by=5)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Percentage of Customers who visited website within 3 months after being contacted according to history segment", x="History Segment", y="Percentage")+
  scale_x_discrete(limits=c(1:7),labels=c("$0 - $100", "$100 - $200", "$200 - $350", "$350 - $500","$500 - $750", "$750 - $1.000", "$1.000 +"))



#Gender
#Visit according to gender

#number of customers purchased only men and visited
men_visited_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$visit==1 & direct_marketing$womens ==0, ])
men_not_visited_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$visit==0 & direct_marketing$womens ==0, ])
men_total_num = nrow(direct_marketing[direct_marketing$men == 1 & direct_marketing$womens == 0, ])

men_not_visited_num
men_total_num

#number of customers purchased only women and visited
women_visited_num = nrow(direct_marketing[direct_marketing$mens == 0 & direct_marketing$visit==1 & direct_marketing$womens ==1, ])
women_not_visited_num = nrow(direct_marketing[direct_marketing$mens == 0 & direct_marketing$visit==0 & direct_marketing$womens ==1, ])
women_total_num = nrow(direct_marketing[direct_marketing$men == 0 & direct_marketing$womens == 1, ])

#number of customers purchased both and visited
both_visited_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$visit==1 & direct_marketing$womens ==1, ])
both_not_visited_num = nrow(direct_marketing[direct_marketing$mens == 1 & direct_marketing$visit==0 & direct_marketing$womens ==1, ])
both_total_num = nrow(direct_marketing[direct_marketing$men == 1 & direct_marketing$womens == 1, ])


category_table <- rbind(c("Men","Women", "Both","Row Sum"), 
                        c(men_visited_num, women_visited_num, both_visited_num, sum(men_visited_num, women_visited_num, both_visited_num)),
                        c(men_not_visited_num, women_not_visited_num, both_not_visited_num, sum(men_not_visited_num, women_not_visited_num, both_not_visited_num)),
                        c(men_total_num, women_total_num, both_total_num, sum(men_total_num, women_total_num, both_total_num)))
                        
rownames(category_table) <- c("Category","Visited", "Not Visited", "Col Sum")

grid.table(category_table)

category_chi_table = rbind(c(men_visited_num, women_visited_num, both_visited_num), 
                           c(men_not_visited_num, women_not_visited_num, both_not_visited_num))

category_chi_table
chisq.test(category_chi_table)


df_gender_visited = data.frame(gender = c(1,1,2,2,3,3), 
                               visit=c("Visited", "Not Visited","Visited", "Not Visited","Visited", "Not Visited"),
                               V1 = c(men_visited_num, men_total_num-men_visited_num, women_visited_num, women_not_visited_num, both_visited_num, both_not_visited_num))


df_gender_visited

#Draw histogram about how many customers visited website according to category purchased before
ggplot(df_gender_visited, aes(x = gender, y = V1, fill=visit)) + 
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(data = df_gender_visited, aes(x = gender, y = V1,label = V1), size=4,vjust = 2) +
  labs(title = "Comparison between Customers who visited website or not within 3 months after being contacted according to category", x="Category purchased before", y="Population") +
  scale_x_discrete(limits=c(1:3),labels=c("Men", "Women", "Both"))

#calculate the percentage of customers visited website according to category purchased before
gender_visited_pct <-round(c(men_visited_num, women_visited_num, both_visited_num)/c(men_total_num, women_total_num, both_total_num)*100)
gender_visited_pct_dt <- data.frame(obj = c(1:3), val = gender_visited_pct)

#Draw line graph about the percentage
ggplot(gender_visited_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_line(color="darkorchid1") + 
  geom_point(shape = 23,size = 3, fill = "darkorchid4") +
  scale_y_continuous(breaks=seq(0,25,by=5)) +
  expand_limits(x = 1, y = 0) +
  geom_text(aes(label = paste(val, "%", sep=""), vjust = -0.8, hjust = 0.5)) + 
  labs(title = "Percentage of Customers who visited website within 3 months after being contacted according to category", x="History Segment", y="Percentage")+
  scale_x_discrete(limits=c(1:3),labels=c("Men", "Women", "Both"))



#Visit according to zip code
zip_code = direct_marketing$zip_code
zip_code

dt_zip_code_and_visit = data.frame(zip_c = zip_code, visited = visit_col)

dt_zip_code_and_visit$visited <- replace(dt_zip_code_and_visit$visited, dt_zip_code_and_visit$visited == 1, "Visited")
dt_zip_code_and_visit$visited <- replace(dt_zip_code_and_visit$visited, dt_zip_code_and_visit$visited == 0, "Not Visited")

dt_zip_code_and_visit

dt_zip_code_and_visit <- ddply(dt_zip_code_and_visit, .(zip_c, visited), nrow)
dt_zip_code_and_visit
# 
# dt_hist_seg_and_visit$hist_seg
# counts$recency

dt_zip_code_and_visit["fun"] = c(1,1,2,2,3,3)

dt_zip_code_and_visit

ggplot(dt_zip_code_and_visit, aes(x = fun , y = V1, fill=visited)) + 
  geom_bar(stat = "identity",width = 0.6) +
  scale_x_continuous(breaks=seq(1,3,by=1)) +
  geom_text(data = dt_zip_code_and_visit, aes(x = fun, y = V1,label = V1), size=4,vjust = 2) +
  labs(title = "Comparison between Customers who visited website or not within 3 months after being contacted according to Zip Code", x="Zip Code", y="Population") +
  scale_x_discrete(limits=c(1:3),labels=c("Rural", "Surburban", "Urban"))


zip_code_total_customers = aggregate(V1 ~ fun, dt_zip_code_and_visit, sum)[,"V1"]
zip_code_total_customers

zip_code_visited_counts <- subset(dt_zip_code_and_visit, visited == "Visited")[,"V1"]
zip_code_visited_counts

zip_code_not_visited_counts <- zip_code_total_customers - zip_code_visited_counts

zip_code_total_col <- append(zip_code_total_customers, sum(zip_code_total_customers))
zip_code_visited_col <- append(zip_code_visited_counts, sum(zip_code_visited_counts))
zip_code_not_visited_col <- append(zip_code_not_visited_counts, sum(zip_code_not_visited_counts))

zip_code_table <- rbind(c("Rural", "Surburban", "Urban", "Row Sum"), zip_code_visited_col, 
                        zip_code_not_visited_col, zip_code_total_col)
rownames(zip_code_table) <- c("Zip code","Visited", "Not Visited", "Col Sum")

grid.table(zip_code_table)

zip_chi_table = rbind(zip_code_visited_counts, zip_code_not_visited_counts)
zip_chi_table

chisq.test(zip_chi_table)



zip_code_visited_pct <-round(zip_code_visited_counts/zip_code_total_customers*100)

zip_code_visited_pct_dt <- data.frame(obj = c(1:3), val = zip_code_visited_pct)

ggplot(zip_code_visited_pct_dt, aes(x = obj, y = val), size = 5) + 
  geom_bar(stat = "identity",width = 0.6, fill = "darkorange") +
  scale_x_continuous(breaks=seq(1,3,by=1)) +
  geom_text(data = zip_code_visited_pct_dt, aes(x = obj, y =(val),label = paste0(val, "%")), size=4,vjust = -0.5) +
  labs(title = "Percentage of Customers who visited website within 3 months after being contacted according to Zip Code", x="Zip Code", y="Percentage") +
  scale_x_discrete(limits=c(1:3),labels=c("Rural", "Surburban", "Urban"))



#Visit according to newbie
newbie = direct_marketing$newbie
newbie

dt_newbie_and_visit = data.frame(nb = newbie, visited = visit_col)

dt_newbie_and_visit$visited <- replace(dt_newbie_and_visit$visited, dt_newbie_and_visit$visited == 1, "Visited")
dt_newbie_and_visit$visited <- replace(dt_newbie_and_visit$visited, dt_newbie_and_visit$visited == 0, "Not Visited")

dt_newbie_and_visit

dt_newbie_and_visit <- ddply(dt_newbie_and_visit, .(nb, visited), nrow)
dt_newbie_and_visit


ggplot(dt_newbie_and_visit, aes(x = nb , y = V1, fill=visited)) +
  geom_bar(stat = "identity",width = 0.6) +
  geom_text(data = dt_newbie_and_visit, aes(x = nb, y = V1,label = V1), size=4,vjust = 2) +
  labs(title = "Comparison between Customers who visited website or not within 3 months after being contacted according to Newbie", x="Whether Newbie", y="Population") +
  scale_x_discrete(limits=c(0:1),labels=c("Not Newbie","Newbie"))


newbie_total_customers = aggregate(V1 ~ nb, dt_newbie_and_visit, sum)[,"V1"]
newbie_total_customers

newbie_visited_counts <- subset(dt_newbie_and_visit, visited == "Visited")[,"V1"]
newbie_visited_counts

newbie_not_visited_counts = newbie_total_customers - newbie_visited_counts

newbie_total_col <- append(newbie_total_customers, sum(newbie_total_customers))
newbie_visited_col <- append(newbie_visited_counts, sum(newbie_visited_counts))
newbie_not_visited_col <- append(newbie_not_visited_counts, sum(newbie_not_visited_counts))

newbie_table <- rbind(c("Not Newbie", "Newbie", "Row Sum"), newbie_visited_col, 
                        newbie_not_visited_col, newbie_total_col)
rownames(newbie_table) <- c("Newbie","Visited", "Not Visited", "Col Sum")

grid.table(newbie_table)

newbie_chi_table = rbind(newbie_visited_counts, newbie_not_visited_counts)
newbie_chi_table

chisq.test(newbie_chi_table)






newbie_visited_pct <-round(newbie_visited_counts/newbie_total_customers*100)

newbie_visited_pct

newbie_visited_pct_dt <- data.frame(obj = c(0:1), val = newbie_visited_pct)

newbie_visited_pct_dt

ggplot(newbie_visited_pct_dt, aes(x = obj, y = val), size = 5) +
  geom_bar(stat = "identity",width = 0.4, fill = "green4") +
  scale_x_continuous(breaks=seq(1,2,by=1)) +
  geom_text(data = newbie_visited_pct_dt, aes(x = obj, y =(val),label = paste0(val, "%")), size=4,vjust = -0.5) +
  labs(title = "Percentage of Customers who visited website within 3 months after being contacted according to Newbie", x="Whether Newbie", y="Percentage") +
  scale_x_discrete(limits=c(0:1),labels=c("Not Newbie", "Newbie"))



#Visit according to channel
channel = direct_marketing$channel
channel

dt_channel_and_visit = data.frame(chn = channel, visited = visit_col)
dt_channel_and_visit$visited <- replace(dt_channel_and_visit$visited, dt_channel_and_visit$visited == 1, "Visited")
dt_channel_and_visit$visited <- replace(dt_channel_and_visit$visited, dt_channel_and_visit$visited == 0, "Not Visited")

head(dt_channel_and_visit)

dt_channel_and_visit <- ddply(dt_channel_and_visit, .(chn, visited), nrow)
dt_channel_and_visit

ggplot(dt_channel_and_visit, aes(x = chn , y = V1, fill=visited)) +
  geom_bar(stat = "identity",width = 0.6) +
  geom_text(data = dt_channel_and_visit, aes(x = chn, y = V1,label = V1), size=4,vjust = 1.5) +
  labs(title = "Comparison between Customers who visited website or not within 3 months after being contacted according to Channel", x="Channel types", y="Population")


channel_total_customers = aggregate(V1 ~ chn, dt_channel_and_visit, sum)[,"V1"]
channel_total_customers

channel_visited_counts <- subset(dt_channel_and_visit, visited == "Visited")[,"V1"]
channel_visited_counts


channel_not_visited_counts = channel_total_customers - channel_visited_counts

channel_total_col <- append(channel_total_customers, sum(channel_total_customers))
channel_visited_col <- append(channel_visited_counts, sum(channel_visited_counts))
channel_not_visited_col <- append(channel_not_visited_counts, sum(channel_not_visited_counts))

channel_table <- rbind(c("Multichannel", "Phone","Web", "Row Sum"), channel_visited_col, 
                      channel_not_visited_col, channel_total_col)
rownames(channel_table) <- c("Channel","Visited", "Not Visited", "Col Sum")

grid.table(channel_table)

channel_chi_table = rbind(channel_visited_counts, channel_not_visited_counts)
channel_chi_table

chisq.test(channel_chi_table)




channel_visited_pct <-round(channel_visited_counts/channel_total_customers*100)
channel_visited_pct_dt <- data.frame(obj = c(0:2), val = channel_visited_pct)

channel_visited_pct_dt

ggplot(channel_visited_pct_dt, aes(x = obj, y = val), size = 5) +
  geom_bar(stat = "identity",width = 0.6, fill = "bisque3") +
  scale_x_continuous(breaks=seq(1,3,by=1)) +
  geom_text(data = channel_visited_pct_dt, aes(x = obj, y =(val),label = paste0(val, "%")), size=4,vjust = -0.5) +
  labs(title = "Percentage of Customers who visited website within 3 months after being contacted according to Channel", x="Chanel types", y="Percentage") +
  scale_x_discrete(limits=c(0:2),labels=c("Multichannel", "Phone", "Web"))






