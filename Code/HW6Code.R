##Code for the report.

#Load data
leadData <- read.csv("~/Documents/GitHub/Kebe_Project_01/DataRaw/lead-iq-01.csv")

#Calculate averages
avg_iq_near <- mean(leadData[leadData$Smelter == "Near",]$IQ)
avg_iq_far <- mean(leadData[leadData$Smelter == "Far",]$IQ)


#data visualization:
boxplot(IQ~Smelter, data = leadData)


##Other methods for data visualization that were tried and uneffective/unasthetic
p <- ggplot(leadData, aes(x=Smelter, y=IQ)) + 
  geom_dotplot(binaxis='y', stackdir='center')

# Use geom_errorbar()
p + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="errorbar", color="red", width=0.5) +
  stat_summary(fun.y=mean, geom="point", color="red")


means <- c(avg_iq_far,avg_iq_near)
stdevs <- c(leadData[leadData$Smelter == "Far",]$IQ,sd(leadData[leadData$Smelter == "Near",]$IQ))

p<- ggplot(leadData, aes(x=Smelter, y=IQ) +
             geom_point() +
             geom_errorbar(aes(ymin=means-stdevs, ymax=means+stdevs), width=.2)
           print(p)
           hist(leadData[leadData$Smelter == "Near",]$IQ)
           hist(leadData[leadData$Smelter == "Far",]$IQ)
           
           
           plot(x = leadData$Smelter, y=leadData$IQ)
           
           ggplot(leadData, aes(x=Smelter_Binary,y = leadData$IQ, color = leadData$Smelter)) +
             geom_point()
           
           smelter_bin <- as.numeric(leadData$Smelter == "Near")
           leadData <- c(leadData, smelter_bin)
           