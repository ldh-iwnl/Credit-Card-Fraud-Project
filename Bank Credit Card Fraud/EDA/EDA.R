#load libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(scales)
library(gridExtra)
#===============================================================================
#load data
credit_card_data <- read.csv(file="../card_transdata.csv")


#==============================================================================
#Exploratory Data Analysis

#print out the column names
print(colnames(credit_card_data))

#print out the number of observations
print(nrow(credit_card_data))

#print out the summary of each attribute
summary(credit_card_data)

#--------------------Outlier Analysis-------------------------------------------
null_values_table <- sapply(credit_card_data, function(x) sum(is.na(x)))
print("the number of null values of each attribute")
#The number of null values in each attribute
print(null_values_table)

# distance_from_home
box_homedistance <- ggplot(data = credit_card_data) +
  geom_boxplot(aes(y=distance_from_home)) +
  labs(y = "home distance") +
  ggtitle("Home Distance of transaction")
print(box_homedistance)
ggsave(filename = "box_homedistance.png", plot = box_homedistance,dpi=300, width = 1440, height=1440, units = "px")

# distance_from_last_transaction
box_lastdistance <- ggplot(data = credit_card_data) +
  geom_boxplot(aes(y=distance_from_last_transaction)) +
  labs(y = "last distance") +
  ggtitle("Last Distance of transaction")
print(box_lastdistance)
ggsave(filename = "box_lastdistance.png", plot = box_lastdistance,dpi=300, width = 1440, height=1440, units = "px")

# ratio_to_median_purchase_price
box_price <- ggplot(data = credit_card_data) +
  geom_boxplot(aes(y=ratio_to_median_purchase_price)) +
  labs(y = "ratio of price to median") +
  ggtitle("ratio to median purchase price")
print(box_price)
ggsave(filename = "box_price.png", plot = box_price, dpi=300, width = 1440, height=1440, units = "px")
#--------------------Univariate Analysis----------------------------------------
# distance_from_home
# hist_homedistance <- ggplot(data = credit_card_data, aes(x=distance_from_home)) +
#   geom_histogram(bins=100, fill="black") +
#   labs(x = "home distance") +
#   ggtitle("Home Distance of transaction")
# print(hist_homedistance)
# ggsave(filename = "box_homedistance.png", plot = box_homedistance)

# hist_homedistance_amplified <- ggplot(data = credit_card_data, aes(x=distance_from_home)) +
#   geom_histogram(bins=100, fill="#F8766D") +
#   xlim(0,300) +
#   coord_cartesian(ylim = c(0,300000)) +
#   labs(x = "home distance") +
#   ggtitle("Home Distance of transaction amplified") +
#   theme_bw() +
#   theme(plot.title = element_text(size=10, face="bold"))
# print(hist_homedistance_amplified)
# ggsave(filename = "box_homedistance_amplified.png", plot = box_homedistance_amplified)




# # distance_from_last_transaction
# hist_lastdistance <- ggplot(data = credit_card_data, aes(x=distance_from_last_transaction)) +
#   geom_histogram(bins=100, fill="black") +
#   labs(x = "last distance") +
#   ggtitle("Last Distance of transaction")
# print(hist_lastdistance)
# ggsave(filename = "box_lastdistance.png", plot = box_lastdistance)

# hist_lastdistance_amplified <- ggplot(data = credit_card_data, aes(x=distance_from_last_transaction)) +
#   geom_histogram(bins=100, fill="#F8766D") +
#   xlim(0,300) +
#   coord_cartesian(ylim = c(0,300000)) +
#   labs(x = "last distance") +
#   ggtitle("Last Distance of transaction amplified") +
#   theme_bw() +
#   theme(plot.title = element_text(size=10, face="bold"))
# print(hist_lastdistance_amplified)
# ggsave(filename = "box_lastdistance_amplified.png", plot = box_lastdistance_amplified)

# ratio_to_median_purchase_price
# dense_price <- ggplot(data = credit_card_data, aes(x=ratio_to_median_purchase_price)) +
#   geom_density(fill="#F8766D") +
#   xlim(0,20) +
#   labs(x = "ratio of price to median") +
#   ggtitle("ratio to median purchase price") +
#   geom_vline(xintercept=1, color="#00BFC4",size = 1.5) +
#   theme_bw() +
#   theme(plot.title = element_text(size=10, face="bold"))
# print(dense_price)
# ggsave(filename = "box_price.png", plot = dense_price)

# repeat_retailer
table_retailer <- data.frame(table(credit_card_data$repeat_retailer))
colnames(table_retailer) <- c("value","count")
table_retailer$percentage <- label_percent(accuracy = 0.01)(table_retailer$count / sum(table_retailer$count))

pie_repeat_retailer <- ggplot(data = table_retailer, aes(x="",y=count, fill=value)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label=percentage), position = position_stack(vjust=0.5)) +
  coord_polar("y", start=0) +
  labs(fill = "repeated\nretailer", x="repeated retailer: 881,536", y="not repeated retailer: 118,464") +
  ggtitle("Percentage of repeated retailer") +
  theme_void() +
  theme(axis.text = element_blank())
print(pie_repeat_retailer)
ggsave(filename = "pie_repeat_retailer.png", plot = pie_repeat_retailer,dpi=300, width = 1440, height=1440, units = "px")

# used_chip
table_chip <- data.frame(table(credit_card_data$used_chip))
colnames(table_chip) <- c("value","count")
table_chip$percentage <- label_percent(accuracy = 0.01)(table_chip$count / sum(table_chip$count))

pie_used_chip <- ggplot(data = table_chip, aes(x="",y=count, fill=value)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label=percentage), position = position_stack(vjust=0.5)) +
  coord_polar("y", start=0) +
  ggtitle("Percentage of a used chip") +
  theme_void()
print(pie_used_chip)
ggsave(filename = "pie_used_chip.png", plot = pie_used_chip,dpi=300, width = 1440, height=1440, units = "px")

# used_pin_number
table_pin <- data.frame(table(credit_card_data$used_pin_number))
colnames(table_pin) <- c("value","count")
table_pin$percentage <- label_percent(accuracy = 0.01)(table_pin$count / sum(table_pin$count))

pie_used_pin <- ggplot(data = table_pin, aes(x="",y=count, fill=value)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label=percentage), position = position_stack(vjust=0.5)) +
  coord_polar("y", start=0) +
  ggtitle("Percentage of a used pin") +
  theme_void()
print(pie_used_pin)
ggsave(filename = "pie_used_pin.png", plot = pie_used_pin,dpi=300, width = 1440, height=1440, units = "px")


# online_order
table_online <- data.frame(table(credit_card_data$online_order))
colnames(table_online) <- c("value","count")
table_online$percentage <- label_percent(accuracy = 0.01)(table_online$count / sum(table_online$count))

pie_online_order <- ggplot(data = table_online, aes(x="",y=count, fill=value)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label=percentage), position = position_stack(vjust=0.5)) +
  coord_polar("y", start=0) +
  ggtitle("Percentage of online order") +
  theme_void() +
  theme(axis.text = element_blank())
print(pie_online_order)
ggsave(filename = "pie_online_order.png", plot = pie_online_order,dpi=300, width = 1440, height=1440, units = "px")



# is fraud
table_fraud <- data.frame(table(credit_card_data$fraud))
colnames(table_fraud) <- c("value","count")
table_fraud$percentage <- label_percent(accuracy = 0.01)(table_fraud$count / sum(table_fraud$count))

pie_fraud <- ggplot(data = table_fraud, aes(x="",y=count, fill=value)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label=percentage), position = position_stack(vjust=0.5)) +
  coord_polar("y", start=0) +
  labs(fill = "is fraud", x="fraudulent cases: 87,403", y="not fraudulent cases: 912,597") +
  ggtitle("Percentage of a fraud") +
  theme_classic() +
  theme(axis.text = element_blank())
print(pie_fraud)
ggsave(filename = "pie_fraud.png", plot = pie_fraud, dpi=300, width = 1440, height=1440, units = "px")






# putting them together
univariate_analysis <- grid.arrange(pie_repeat_retailer, pie_used_chip, pie_used_pin, pie_online_order, ncol = 2)
ggsave(filename = "univariate_analysis.png", plot = univariate_analysis, dpi=300, width = 2580, height=1440, units = "px")
#---------------------Bivariate Analysis----------------------------------------
# distance_from_home + fraud
# plot the fraudulent transaction in distance_from_home

# hist_homedistance_fraud <- ggplot(data = credit_card_data, aes(x=distance_from_home, fill=factor(fraud))) +
#   geom_histogram(bins = 100) +
#   xlim(0,1000) +
#   labs(x = "Distance from home", y = "Number of transactions", fill="is fraud") +
#   ggtitle("Distribution of fraudulent transaction in distance from home") +
#   facet_grid(fraud ~ ., scales = "free_y")
# print(hist_homedistance_fraud)
# ggsave(filename = "hist_homedistance_fraud.png", plot = hist_homedistance_fraud)

hist_homedistance_fraud_merge <- ggplot(data = credit_card_data, aes(x=distance_from_home)) +
  geom_histogram(data = credit_card_data,
                 bins = 100,
                 binwidth = 5,
                 aes(x=distance_from_home),
                 fill = "#F8766D",
                 alpha = 0.7) +
  geom_histogram(data = credit_card_data[credit_card_data$fraud==1,],
                 bins = 100,
                 binwidth = 5,
                 aes(x=distance_from_home),
                 fill = "#00BFC4") +
  #xlim(0,1000) +
  coord_cartesian(ylim = c(0,300000), xlim=c(0,500)) +
  labs(x = "Distance from home", y = "Number of transactions") +
  ggtitle("fraud transaction in distance from home") +
  theme_bw() +
  theme(plot.title = element_text(size=10, face="bold"))
print(hist_homedistance_fraud_merge)
ggsave(filename = "hist_homedistance_fraud.png", plot = hist_homedistance_fraud_merge, dpi=300, width = 2560, height=1440, units = "px")



# distance_from_last_transaction + fraud
# plot the fraudulent transaction in distance_from_last_transaction

# hist_lastdistance_fraud <- ggplot(data = credit_card_data, aes(x=distance_from_last_transaction, fill=factor(fraud))) +
#   geom_histogram(bins = 100) +
#   xlim(0,1000) +
#   labs(x = "Distance from last transaction", y = "Number of transactions", fill="is fraud") +
#   ggtitle("fraud transaction in distance of last transaction") +
#   facet_grid(fraud ~ ., scales = "free_y")
# print(hist_lastdistance_fraud)
# ggsave(filename = "hist_lastdistance_fraud.png", plot = hist_lastdistance_fraud)

hist_lastdistance_fraud_merge <- ggplot(data = credit_card_data, aes(x=distance_from_last_transaction)) +
  geom_histogram(data = credit_card_data[credit_card_data$distance_from_last_transaction < 1000,],
                 bins = 100,
                 binwidth = 5,
                 aes(x=distance_from_last_transaction),
                 fill = "#F8766D",
                 alpha = 0.7) +
  geom_histogram(data = credit_card_data[credit_card_data$fraud==1,],
                 bins = 100,
                 binwidth = 5,
                 aes(x=distance_from_last_transaction),
                 fill = "#00BFC4") +
  #xlim(0,1000) +
  coord_cartesian(ylim = c(0,300000), xlim=c(0,500)) +
  labs(x = "Distance from last transaction", y = "Number of transactions") +
  ggtitle("fraud transaction in distance of last transaction") +
  theme_bw() +
  theme(plot.title = element_text(size=10, face="bold"))
print(hist_lastdistance_fraud_merge)
ggsave(filename = "hist_lastdistance_fraud.png", plot = hist_lastdistance_fraud)





# ratio_to_median_purchase_price + fraud
# plot the fraudulent transaction in ratio_to_median_purchase_price

hist_price_fraud_merge <- ggplot(data = credit_card_data, aes(x=ratio_to_median_purchase_price)) +
  geom_histogram(data = credit_card_data,
               aes(x=ratio_to_median_purchase_price),
               fill = "#F8766D",
               alpha = 0.7,
               bins = 100) +
  geom_histogram(data = credit_card_data[credit_card_data$fraud==1,],
               aes(x=ratio_to_median_purchase_price),
               fill = "#00BFC4",
               alpha = 1,
               bins = 100) +
  #geom_vline(xintercept=1, color="black",size = 1) +
  #geom_text(aes(x=2, label="x=1", y=-1), color="black") +
  xlim(0,50) +
  coord_cartesian(ylim = c(0,300000)) +
  labs(x = "ratio to median purchase price", y = "Number of transactions", fill="is fraud") +
  ggtitle("Number of transactions of ratio to median purchase price") +
  theme_bw() +
  theme(plot.title = element_text(size=10, face="bold"))
print(hist_price_fraud_merge)
ggsave(filename = "hist_price_fraud_merge.png", plot = hist_price_fraud_merge, dpi=300, width = 2560, height=1440, units = "px")





# repeat_retailer + fraud
t <- table(credit_card_data[credit_card_data$fraud==1,]$repeat_retailer)
t2 <- table(credit_card_data$repeat_retailer)
table_retailer_fraud <- data.frame(t)
colnames(table_retailer_fraud) <- c("repeat_retailer","count")
table_retailer_fraud$isfraud <- rep(1,nrow(table_retailer_fraud))
table_retailer_fraud$percentage <- label_percent(accuracy = 0.01)(as.numeric(t) / as.numeric(t2))
t2 <- data.frame(t2)
colnames(t2) <- c("repeat_retailer","count")
t2$isfraud <- rep(0,nrow(t2))
t2$percentage <- label_percent(accuracy = 0.01)(c(1,1))
table_retailer_fraud <- rbind(table_retailer_fraud, t2)

bar_retailer_fraud <- ggplot(data = table_retailer_fraud, 
                         aes(x=repeat_retailer,
                             y=count,
                             fill= factor(isfraud))) +
  geom_bar(width=0.5,
           position = "dodge",
           stat="identity") +
  geom_text(aes(x=repeat_retailer, y= count, label=percentage, group=isfraud),
            position = position_dodge(width = 1),
            vjust = -0.25, 
            size = 2) +
  labs(x = "Not a repeated retailer     /     a repeated retailer", y = "Num of transactions", fill="is fraud") +
  ggtitle("the ratio of fraudulent cases in repeated retailer") +
  theme_bw() +
  theme(plot.title = element_text(size=10, face="bold"))
print(bar_retailer_fraud)
ggsave(filename = "bar_retailer_fraud.png", plot = bar_retailer_fraud, dpi=300, width = 2560, height=1440, units = "px")







# used_chip + fraud
t <- table(credit_card_data[credit_card_data$fraud==1,]$used_chip)
t2 <- table(credit_card_data$used_chip)
table_chip_fraud <- data.frame(t)
colnames(table_chip_fraud) <- c("used_chip","count")
table_chip_fraud$isfraud <- rep(1,nrow(table_chip_fraud))
table_chip_fraud$percentage <- label_percent(accuracy = 0.01)(as.numeric(t) / as.numeric(t2))
t2 <- data.frame(t2)
colnames(t2) <- c("used_chip","count")
t2$isfraud <- rep(0,nrow(t2))
t2$percentage <- label_percent(accuracy = 0.01)(c(1,1))
table_chip_fraud <- rbind(table_chip_fraud, t2)

bar_chip_fraud <- ggplot(data = table_chip_fraud, 
                            aes(x=used_chip,
                                y=count,
                                fill= factor(isfraud))) +
  geom_bar(width=0.5,
           position = "dodge",
           stat="identity") +
  geom_text(aes(x=used_chip, y= count, label=percentage, group=isfraud),
            position = position_dodge(width = 1),
            vjust = -0.25, 
            size = 4) +
  theme_bw() +
  labs(x = "Not used a chip     /     used a chip", y = "Num of transactions", fill="is fraud") +
  ggtitle("the ratio of fraudulent cases in used/not used chip situation")
print(bar_chip_fraud)
ggsave(filename = "bar_chip_fraud.png", plot = bar_chip_fraud, dpi=300, width = 2560, height=1440, units = "px")






# used_pin_number + fraud
t <- table(credit_card_data[credit_card_data$fraud==1,]$used_pin_number)
t2 <- table(credit_card_data$used_pin_number)
table_pin_fraud <- data.frame(t)
colnames(table_pin_fraud) <- c("used_pin_number","count")
table_pin_fraud$isfraud <- rep(1,nrow(table_pin_fraud))
table_pin_fraud$percentage <- label_percent(accuracy = 0.01)(as.numeric(t) / as.numeric(t2))
t2 <- data.frame(t2)
colnames(t2) <- c("used_pin_number","count")
t2$isfraud <- rep(0,nrow(t2))
t2$percentage <- label_percent(accuracy = 0.01)(c(1,1))
table_pin_fraud <- rbind(table_pin_fraud, t2)

bar_pin_fraud <- ggplot(data = table_pin_fraud, 
                         aes(x=used_pin_number,
                             y=count,
                             fill= factor(isfraud))) +
  geom_bar(width=0.5,
           position = "dodge",
           stat="identity") +
  geom_text(aes(x=used_pin_number, y= count, label=percentage, group=isfraud),
            position = position_dodge(width = 1),
            vjust = -0.25, 
            size = 4) +
  theme_bw() +
  labs(x = "Not used a pin     /     used a pin", y = "Num of transactions", fill="is fraud") +
  ggtitle("the ratio of fraudulent cases in used/not used pin number situation")
print(bar_pin_fraud)
ggsave(filename = "bar_pin_fraud.png", plot = bar_pin_fraud, dpi=300, width = 2560, height=1440, units = "px")





# online_order + fraud
t <- table(credit_card_data[credit_card_data$fraud==1,]$online_order)
t2 <- table(credit_card_data$online_order)
table_online_fraud <- data.frame(t)
colnames(table_online_fraud) <- c("online_order","count")
table_online_fraud$isfraud <- rep(1,nrow(table_online_fraud))
table_online_fraud$percentage <- label_percent(accuracy = 0.01)(as.numeric(t) / as.numeric(t2))
t2 <- data.frame(t2)
colnames(t2) <- c("online_order","count")
t2$isfraud <- rep(0,nrow(t2))
t2$percentage <- label_percent(accuracy = 0.01)(c(1,1))
table_online_fraud <- rbind(table_online_fraud, t2)

bar_online_fraud <- ggplot(data = table_online_fraud, 
                        aes(x=online_order,
                            y=count,
                            fill= factor(isfraud))) +
  geom_bar(width=0.5,
           position = "dodge",
           stat="identity") +
  geom_text(aes(x=online_order, y= count, label=percentage, group=isfraud),
            position = position_dodge(width = 1),
            vjust = -0.25, 
            size = 2) +
  theme_bw() +
  labs(x = "Not a online order     /   is an online order", y = "Num of transactions", fill="is fraud") +
  ggtitle("the ratio of fraudulent cases in online/offline orders")
print(bar_online_fraud)
ggsave(filename = "bar_online_fraud.png", plot = bar_online_fraud, dpi=300, width = 2560, height=1440, units = "px")


# putting them together
multivariate_analysis <- grid.arrange(hist_homedistance_fraud_merge,hist_lastdistance_fraud_merge, hist_price_fraud_merge,bar_retailer_fraud, nrow=2)
print(multivariate_analysis)
ggsave(filename = "multivariate_analysis.png", plot = multivariate_analysis, dpi=300, width = 2560, height=1440, units = "px")

#correlation of attributes
corr <- corrplot(cor(credit_card_data), method = "number")

#----------------------Multivariate Analysis------------------------------------

#------------------------------not used in report-------------------------------

group_divide <- function(used_chip, used_pin, online_order) {
  if(used_pin == 1)
    return("used pin")
  
  if(used_chip == 0 & online_order==0)
    return("card not present offline order")
  
  if(used_chip == 0 & online_order==1)
    return("card not present online order")
  
  if(used_chip == 1 & online_order==0)
    return("card present offline order")
  
  if(used_chip == 1 & online_order==1)
    return("card present online order")
  
  return("others")
}

# divide each record into a group
grouped_credit_card_data <- credit_card_data
group <- mapply(group_divide, credit_card_data$used_chip, credit_card_data$used_pin_number, credit_card_data$online_order)
grouped_credit_card_data$group <- group

# group analysis
table_grouped_data <- table(grouped_credit_card_data$group)
table_grouped_data <- data.frame(table_grouped_data)
colnames(table_grouped_data) <- c("group_name","order_number")

# group number of order analysis
bar_table_group <- ggplot(data = table_grouped_data, 
                             aes(x=group_name,
                                 y=order_number,
                                 fill= factor(group_name))) +
  geom_bar(width=0.5,
           position = "dodge",
           stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "group name", y = "Num of transactions", fill="group") +
  ggtitle("the number of orders in each group")
print(bar_table_group)
ggsave(filename = "bar_table_group.png", plot = bar_table_group)



# group + home
hist_home_group <- ggplot(data = grouped_credit_card_data,
                          aes(x = distance_from_home, fill = factor(group)),
) +
  geom_histogram(bins = 100) +
  labs(x = "distance from home", y = "Number of transactions", fill = "group type") +
  ggtitle("number of transactions of distance from home by group") +
  facet_grid(group ~ ., scales = "free_y") +
  xlim(0,300) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
print(hist_home_group)
ggsave(filename = "hist_home_group.png", plot = hist_home_group)



# group + home + fraud
hist_group_home_fraud <- ggplot(data = grouped_credit_card_data[grouped_credit_card_data$fraud==1,],
                            aes(x = distance_from_home, fill = factor(group)),
) +
  geom_histogram(bins = 100, data = grouped_credit_card_data, alpha = 0.3, mapping = aes(x = distance_from_home, fill = "All transactions")) +
  geom_histogram(bins = 100) +
  labs(x = "distance from home", y = "Number of transactions", fill = "group type") +
  ggtitle("fraud transaction of distance from home by group") +
  facet_grid(group ~ ., scales = "free_y") +
  xlim(0,300) +
  coord_cartesian(ylim = c(0,3000)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
print(hist_group_home_fraud)
ggsave(filename = "hist_group_home_fraud.png", plot = hist_group_home_fraud,  dpi=300, width = 2560, height=1440, units = "px")







# group + last
hist_group_last <- ggplot(data = grouped_credit_card_data,
                                aes(x = distance_from_last_transaction, fill = factor(group)),
) +
  geom_histogram(bins = 100, data = grouped_credit_card_data, alpha = 0.3, mapping = aes(x = distance_from_home, fill = "All transactions")) +
  geom_histogram(bins = 100) +
  labs(x = "distance from last transaction", y = "Number of transactions", fill = "group type") +
  ggtitle("number of transactions of distance from last transaction by group") +
  facet_grid(group ~ ., scales = "free_y") +
  xlim(0,300) +
  coord_cartesian(ylim = c(0,3000)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
print(hist_group_last)
ggsave(filename = "hist_group_last.png", plot = hist_group_last, dpi=300, width = 2560, height=1440, units = "px")








# group + last + fraud
hist_group_last_fraud <- ggplot(data = grouped_credit_card_data[grouped_credit_card_data$fraud==1,],
                                aes(x = distance_from_last_transaction, fill = factor(group)),
) +
  geom_histogram(bins = 100, data = grouped_credit_card_data, alpha = 0.3, mapping = aes(x = distance_from_home, fill = "All transactions")) +
  geom_histogram(bins = 100) +
  labs(x = "distance from last transaction", y = "Number of transactions", fill = "group type") +
  ggtitle("fraud transaction of distance from last transaction by group") +
  facet_grid(group ~ ., scales = "free") +
  xlim(0,300) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
print(hist_group_last_fraud)
ggsave(filename = "hist_group_home_fraud.png", plot = hist_group_last_fraud, dpi=300, width = 2560, height=1440, units = "px")







# group + price
hist_group_price <- ggplot(data = grouped_credit_card_data,
                          aes(x = ratio_to_median_purchase_price, fill = factor(group)),
) +
  geom_histogram(bins = 200) +
  labs(x = "ratio to median purchase price", y = "Number of transactions", fill = "group type") +
  ggtitle("number of transactions of price ratio to median by group") +
  facet_grid(group ~ ., scales = "free_y") +
  #geom_histogram(bins = 100, data = grouped_credit_card_data, alpha = 0.3, mapping = aes(x = distance_from_home, fill = "All transactions")) +
  xlim(0,10) +
  #coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
print(hist_group_price)
ggsave(filename = "hist_group_price.png", plot = hist_group_price, dpi=300, width = 2560, height=1440, units = "px")



# group + last + fraud
hist_group_price_fraud <- ggplot(data = grouped_credit_card_data[grouped_credit_card_data$fraud==1,],
                                aes(x = distance_from_last_transaction, fill = factor(group)),
) +
  geom_histogram(bins=100,data = grouped_credit_card_data, alpha = 0.3, mapping = aes(x = ratio_to_median_purchase_price, fill = "All transactions")) +
  geom_histogram(bins=100) +
  labs(x = "ratio to median purchase price", y = "Number of transactions", fill = "group type") +
  ggtitle("fraud transactions of price ratio to median by group") +
  facet_grid(group ~ ., scales = "free") +
  xlim(0,10) +
  coord_cartesian(ylim = c(0,2000)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
print(hist_group_price_fraud)
ggsave(filename = "hist_group_price_fraud.png", plot = hist_group_price_fraud)




# table + group + retailer
t <- table(grouped_credit_card_data[grouped_credit_card_data$fraud==1,]$group, grouped_credit_card_data[grouped_credit_card_data$fraud==1,]$repeat_retailer)
t2 <- table(grouped_credit_card_data$group, grouped_credit_card_data$repeat_retailer)
table_group_retailer_fraud <- data.frame(t)
table_group_retailer <- data.frame(t2)
colnames(table_group_retailer_fraud) <- c("group","repeat_retailer","count")
colnames(table_group_retailer) <- c("group","repeat_retailer","count")
print(table_group_retailer_fraud)
table_group_retailer_fraud$percentage <- label_percent(accuracy = 0.01)(as.numeric(t) / as.numeric(t2))
table_group_retailer$percentage <- label_percent(accuracy = 0.01)(rep(1,nrow(t2)))
print(table_group_retailer)

# online_order + fraud


bar_group_retailer_fraud <- ggplot(data = table_group_retailer_fraud, 
                           aes(x=repeat_retailer,
                               y=count,
                               fill= factor(group))) +
  geom_bar(width=0.75,
           position = "dodge",
           stat="identity") +
  geom_text(aes(x=repeat_retailer, y= count, label=percentage),
            position = position_dodge(width = 0.75),
            vjust = -0.25, 
            size = 2) +
  geom_bar(data = table_group_retailer,
           width=0.75,
           alpha =0.3,
           position = "dodge",
           stat="identity") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "Not a repeated retailer / is a repeated retailer", y = "Num of transactions", fill="group type") +
  ggtitle("retailer status in each group")
print(bar_group_retailer_fraud)
ggsave(filename = "bar_group_retailer_fraud.png", plot = bar_group_retailer_fraud, dpi=300, width = 2560, height=1440, units = "px")




#group + fraud

# group analysis
table_grouped_data <- table(grouped_credit_card_data$group)
table_grouped_data_fraud <- table(grouped_credit_card_data[grouped_credit_card_data$fraud==1,]$group)
table_grouped_data <- data.frame(table_grouped_data)
table_grouped_data_fraud <- data.frame(table_grouped_data_fraud)
colnames(table_grouped_data) <- c("group_name","order_number")
colnames(table_grouped_data_fraud) <- c("group_name","order_number")
table_grouped_data_fraud$percentage <- label_percent(accuracy = 0.01)(table_grouped_data_fraud$order_number / table_grouped_data$order_number)

# group number of order analysis
bar_table_group <- ggplot(data = table_grouped_data, 
                          aes(x=group_name,
                              y=order_number,
                              fill= factor(group_name))) +
  geom_bar(width=0.75,
           position = "dodge",
           stat="identity",
           alpha = 0.3) +
  geom_bar(data = table_grouped_data_fraud,
           width=0.75,
           position = "dodge",
           stat="identity") +
  geom_text(data = table_grouped_data_fraud,
            aes(x=group_name, y= order_number, label=percentage),
            position = position_dodge(width = 0.75),
            vjust = -0.25, 
            size = 3) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "groups", y = "Num of transactions", fill="group") +
  ggtitle("the number of orders in each group")
print(bar_table_group)
ggsave(filename = "bar_table_group.png", plot = bar_table_group, dpi=300, width = 2560, height=1440, units = "px")

#===============================================================================
