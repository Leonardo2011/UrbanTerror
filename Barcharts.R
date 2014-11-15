PreGTD <- read.csv("PreAnalysis/pregtd.csv", header=TRUE)

ggplot(PreGTD, aes(CUC.dist.km, iyear)) +geom_point()

qplot(CUC.dist.km, data=PreGTD, geom = "freqpoly", ylab= "sum of attacks", xlab= "year",
 main="Number of Attacks with increasing distances from the nearest Urban Center")

ggplot(PreGTD, aes(x=CUC.dist.km, y=count)) +geom_histogram()

ggplot(PreGTD, aes("iyear">=2000)) +geom_histogram(aes(x=CUC.dist.km), binwidth = 0.5, stat="bin", width=2) + scale_x_continuous(limits=c(0,1000)) + scale_y_log10()

ggplot(PreGTD) 
+geom_histogram(aes(x=CUC.dist.km), binwidth = 10, stat="bin", width=2, colour="blue", fill="white") 
+ scale_x_continuous(limits=c(0,1000)) 
+ scale_y_log10()
+ labels(x="Distance to closest Urban Center", y="Number of Attacks", title="Number of attacks with increasing distance to CUC" )
