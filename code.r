initial<-read.csv("cost.csv",nrows=100,header=TRUE,stringsAsFactors=FALSE)
classes<-sapply(initial,class)
cost<-read.csv("cost.csv",header=TRUE,stringsAsFactors=FALSE,colClasses=classes)

colnames(cost) <-c("Date","Open","High","Low","Close","Volumn")
cost$Date<-as.Date(cost$Date,format="%Y-%m-%d")
cost$Average<-rowMeans(subset(cost, select = c(High, Low)), na.rm = TRUE)
