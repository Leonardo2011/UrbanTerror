# take a look at the structure of our data
str(PreGTD)

# some quick counts of in urban center, part of urban centers, and in both
count(PreGTD, "in.urban.centers.environment")
count(PreGTD, "part.of.urban.center")
count(PreGTD, c("in.urban.centers.environment", "part.of.urban.center"))

# make a table of attacks in capital cities per year
cap.table0<-with(PreGTD, table(iyear, capital))
cap.table <- subset(PreGTD, capital==1, select=1:30)

#now just year and capital
cap.table<-with(cap.table, table(iyear, capital))
cap.table

# what percentage of attacks took place in capitals since 1970?
(xtabs(~capital, data = PreGTD))
barplot(xtabs(~capital, data = PreGTD))
summary(xtabs(~capital, data = PreGTD))
count(PreGTD$capital)
percattacksincap<-round(22036/92057, digits=2)*100

# Grouped Bar Plot
counts <- table(cap.table$iyear, cap.table$capital)
barplot(counts, main="Attacks in Capitals By Year",
        xlab="Year", col=c("darkblue","red"), beside=TRUE)
