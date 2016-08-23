library(ggplot2)
library(datasets)
library(dplyr)

initial<-read.csv("cost.csv",nrows=100,header=TRUE,stringsAsFactors=FALSE)
classes<-sapply(initial,class)
cost<-read.csv("cost.csv",header=TRUE,stringsAsFactors=FALSE,colClasses=classes)

colnames(cost) <-c("Date","Open","High","Low","Close","Volumn")
cost$Date<-as.Date(cost$Date,format="%Y-%m-%d")

cost <-cost[order(cost$Date),] 
#plot
png(file="HistoricalPrices5y.png",width=580,height=480)
ggplot(cost, aes(cost$Date,cost$Close))+
geom_line(colour="#ffa07a")+
ggtitle("Historical Prices for COST Stock in Five Year")+
xlab("Date")+ylab("price($)")
dev.off()

cost1y<-cost[1005:1256,]
png(file="HistoricalPrices1y.png",width=580,height=480)
ggplot(cost[1005:1256,], aes(cost$Date[1005:1256],cost$Close[1005:1256]))+
geom_line(colour="#ffa07a")+
ggtitle("Historical Prices for COST Stock in One Year")+
xlab("Date")+ylab("price($)")
dev.off()


p<-ggplot(cost, aes(x=cost$Date,y=cost$Close))+
geom_line(colour="#ffa07a")+
geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+
ggtitle("Linear Regression Model")+
xlab("Date")+ylab("price($)")


lm_eqn <- function(df){
    m <- lm(Close ~ Date, cost);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
}

png(file="LinearRegressionModel.png",width=580,height=480)
p + geom_text(x = 15670, y = 150, label = lm_eqn(df), parse = TRUE)
dev.off()

summary(m)

##difference log model
dif<-diff(log(cost$Close))
png(file="HistogramDifferenceLog(Closing Price).png",width=580,height=480)
qplot(dif,xlab="Difference",ylab="Count",main="Histogram of Difference in Log(Close Price)",fill=I("#ffa07a"),binwidth=0.003)
dev.off()

png(file="DifferenceLog(Closing Price).png",width=580,height=480)
ggplot(cost[-1,], aes(x=cost$Date[-1],y=dif))+
geom_line(colour="#ffa07a")+
ggtitle("Difference in Log(Close Price)")+
xlab("Date")+ylab("Difference")
dev.off()

ks.test(dif,"pnorm",mean(dif),sd(dif))
shapiro.test(dif)
