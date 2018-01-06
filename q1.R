
# clear memory
rm(list=ls())

table_draw_pie <- function(table, title){
	slices <- as.vector(table)
	lbls <- names(table)
	pct <- round(slices/sum(slices)*100)
	lbls <- paste(lbls, pct)
	lbls <- paste(lbls, "%", sep="")
	pie(slices, labels = lbls, main = title)
	
}

draw_pie <- function(slices, lbls, title){
	pct <- round(slices/sum(slices)*100)
	lbls <- paste(lbls, pct) 
	lbls <- paste(lbls,"%",sep="") 
	pie(slices,labels = lbls, main=title)
}

#read data from direct_marketing.csv
direct_marketing <- read.csv2("direct_marketing.csv",fileEncoding="UTF-16LE", sep="\t")

# I am still not sure about what are statistical properties. Just calcuted mean, variance, median and plotted few graphs 
# And something else that I think could help

#Recency
#mean variance and median(maybe useless for recency)
mean(direct_marketing$recency)
var(direct_marketing$recency)
median(direct_marketing$recency)


#Draw histogram for Recency
hist(direct_marketing$recency,breaks=seq(0,12,by=1), xlim = c(0,12), ylim = c(0, 7000), labels = TRUE, main = "Distribution of Months Since Last Purchase", xlab="Months Since Last Purchase", ylab = "Population", col=c("red3", "grey17"))


#Draw pie chart for history_segment
#1. get counts for history_segment
h_s <- table(direct_marketing$history_segment) 

#2. get slices and labels
slices <- as.vector(h_s)
lbls <- names(h_s)

h_s
slices
lbls

#3. get percentage
pct <- round(slices/sum(slices)*100)

#4. add percents to labels
lbls <- paste(lbls, pct) 

#5. add % to labels 
lbls <- paste(lbls,"%",sep="") 

#6. draw pie chart
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of history_segment")

#History
#Since history is a factor, need to convert history to number
history_value <- as.numeric(levels(direct_marketing$history))[direct_marketing$history]

#basic stats property for history
summary(history_value)


#In order to see statistical properties and outliers clearly, I believe boxplot is the best
#About boxplot (https://www.zhihu.com/question/36172806)
bxp <- boxplot(history_value, varwidth=TRUE, main="Boxplot of history")
text(x = col(bxp$stats) - .5, y = bxp$stats, labels = bxp$stats,cex  = 0.8)


#In order to see statistical properties and outliers clearly, I believe boxplot is the best
#About boxplot (https://www.zhihu.com/question/36172806)
bxp <- boxplot(history_value, varwidth=TRUE, main="Boxplot of history")
text(x = col(bxp$stats) - .5, y = bxp$stats, labels = bxp$stats,cex  = 0.8)


#Count how many customers only purchased "men" category during the previous 12 months before the marketing action
only_men <- subset(direct_marketing, direct_marketing$mens == 1 & direct_marketing$womens == 0)
only_men_count <- nrow(only_men)

#Count how many customers only purchased "women" category during the previous 12 months before the marketing action
only_women <- subset(direct_marketing, direct_marketing$mens == 0 & direct_marketing$womens == 1)
only_women_count <- nrow(only_women)

#Count how many customers purchased both "men" and "women" category during the previous 12 months before the marketing action
both <- subset(direct_marketing, direct_marketing$mens == 1 & direct_marketing$womens == 1)
both_count <- nrow(both)

#Pie chart for catagory purchasing
catg <- c(only_men_count, only_women_count, both_count)
catg_lbls <- c("Customers only purchased men catagory","Customers only purchased women catagory", "Customers purchased both men and women catagory")
draw_pie(catg, catg_lbls, "Pie chart of catagory purchasing")


#Pie chart for zip_code
table_draw_pie(table(direct_marketing$zip_code), "Pie chart for Zip Code")

#Pie chart for newbies
new_customers <- subset(direct_marketing, direct_marketing$newbie == 1)
new_customers_count <- nrow(new_customers)

old_customers <- subset(direct_marketing, direct_marketing$newbie == 0)
old_customers_count <- nrow(old_customers)

newbies <- c(new_customers_count, old_customers_count)
newbies_lbls <- c("New Customers", "Old Customers")
draw_pie(newbies, newbies_lbls, "Pie chart of Newbies")

#Pie chart for channel
table_draw_pie(table(direct_marketing$channel), "Pie chart for Channel")