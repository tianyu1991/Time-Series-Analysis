library(ggplot2)
library(datasets)
library(dplyr)

initial<-read.csv("cost.csv",nrows=100,header=TRUE,stringsAsFactors=FALSE)
classes<-sapply(initial,class)
cost<-read.csv("cost.csv",header=TRUE,stringsAsFactors=FALSE,colClasses=classes)

colnames(cost) <-c("Date","Open","High","Low","Close","Volumn")
cost$Date<-as.Date(cost$Date,format="%Y-%m-%d")
cost$Average<-rowMeans(subset(cost, select = c(High, Low)), na.rm = TRUE)
Volumn<-cost$Volumn

cost <-cost[order(cost$Date),] 
#plot
png(file="HistoricalPrices5y.png",width=680,height=480)
ggplot(cost, aes(cost$Date,cost$Close))+
geom_line(colour="#ffa07a")+
ggtitle("Historical Prices for COST Stock in Five Year")+
xlab("Date")+ylab("price($)")
dev.off()

cost1y<-cost[1005:1256,]
png(file="HistoricalPrices1y.png",width=680,height=480)
ggplot(cost[1005:1256,], aes(cost$Date[1005:1256],cost$Close[1005:1256]))+
geom_line(colour="#ffa07a")+
ggtitle("Historical Prices for COST Stock in One Year")+
xlab("Date")+ylab("price($)")
dev.off()
