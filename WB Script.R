install.packages("ggplot2")
library("ggplot2")


vesselData <- read.csv(file.choose(), header = TRUE)

summary(vesselData)

names(vesselData)

factor(vesselData$isremarkable) # has only 1 level, cannot be used for prediction
factor(vesselData$hasnohamis) # empty, cannot be used


# see relation between deadweight tonnes and total load quantity
sumLoad <- vesselData$load1 + vesselData$load2 + vesselData$load3 + vesselData$load4

vesselDataModif <- cbind(vesselData, sumLoad)

ggplot(vesselDataModif, aes(x = vesselDataModif$vesseldwt, y = vesselDataModif$sumLoad))+
  geom_point()


# see relation between deadweight tonnes and total discharge quantity
sumDis <- vesselData$discharge1 + vesselData$discharge2 + vesselData$discharge3 + vesselData$discharge4

vesselDataModif <- cbind(vesselDataModif, sumDis)

ggplot(vesselDataModif, aes(x = vesselDataModif$vesseldwt, y = vesselDataModif$sumDis))+
  geom_point()

# as expected, above plots reveal that vessels with larger capacity load/ discharge more
# given load/ discharge takes place, the total quantity can be fit using a linear model


# see relation between type and deadweight
ggplot(vesselDataModif, aes(y = vesselDataModif$vesseldwt, x = vesselDataModif$vesseltype))+
  geom_point()
# type and deadweight are not correlated


# relation between load1 and vesseltype (also vesselID as Id has only one type)
# with respect to travel type
ggplot(vesselDataModif, aes(y = vesselData$load1, x = vesselDataModif$traveltype))+
  geom_point()+ 
  facet_wrap(~vesselDataModif$vesseltype)
# vessels of type 2 and 3 can have load1, type 2 carries higher quantities

ggplot(vesselDataModif, aes(y = vesselData$load2, x = vesselDataModif$traveltype))+
  geom_point()+ 
  facet_wrap(~vesselDataModif$vesseltype)
# vessels of type 3 is likely to have load2


# check which load / discharges can be together
checkload <- vesselDataModif$load1 > 0 & vesselDataModif$load2 > 0
sum (checkload) # load1 and 2 are never together

checkload <- vesselDataModif$load1 > 0 & vesselDataModif$load3 > 0 
sum (checkload) # load1 and 3 are never together

# compare all pairs
for ( i in 6:12){
  for ( k in (i+1):13){
    checkload <- vesselDataModif[,i] > 0 & vesselDataModif[,k] > 0
    if(sum(checkload) > 0){
      print(paste(i, " ", k, " ", sum(checkload)))
    }
  }
}
# column 12 (discharge) and column 13 (load4) are likely to be together (79 instances)
# column 10 (discharge3) and column 13 (load4) can be together (13 instances)

