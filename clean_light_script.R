### R script to process a batch of files from HoBo light loggers, remove outliers, and calculate basic descriptors
### Code by G.Rocher-Ros, developed for research purposes and internal use, so no responsability 
# over third party use is taken, for further details, requests and questions contact: gerardrocher@gmail.com
# 
#Updated version, sample file and other information can be found at https://github.com/gmrocher/outliers_HoBolight_loggers

###LOADING PACKAGES NEEDED

library(foreign)
library(xts)


##LOAD FUNCTIONS 

### function to read the files and give column names
readfiles <- function(files){ 
 data<- read.csv(file =files)
  colnames(data) <- c("nr","day","time","temp","lux")
  data$paar <- data$lux*0.019    #this is to convert from lux to PAR
  data$time<- gsub("fm", "AM", data$time)  #to convert from swedish date format to standard one
  data$time <- gsub("em", "PM", data$time)
  data$date.time <- paste(data$day, data$time, sep=" ")
  data$date.time <- as.POSIXct(strptime(data$date.time, "%m/%d/%Y %I:%M:%S %p"))
  
return(data)
}

### function to make a plot of the data to compare corrected and uncorrected data
PlotPar <- function(data, name1){
  pdf(file = paste(name1, ".pdf", sep="")) #this is to export the plot as a pdf
  plot(data$nr, data$paar, type="l", col="darkorange", xlab="time", ylab="Par", main =  name1 )
  lines( data$nr, data$newpar, type="l", col="black") #plot to visually check the change
  legend("topright", lty = 1, col = c("darkorange", "black"), legend=c("Uncorrected", "corrected"))
  graphics.off() 
  
  
}

### function to remove outliers and replace them with the average of the next closest values
Paroutliers <-function(data){ 
  data$newpar= data$paar 
  thres= quantile(data$paar, c(.99))  #determine the threshold for outliers as the 0.01 quantile
  for(i in 1:length(data$paar)){
    if( data$paar[i]>thres){  #if one value is above the threshold... 
      data$newpar[i] = (data$paar[i-1]+data$paar[i+1])/2    #subtitute it with the average of the previous and the next value 
    }
  } 
  return(data)
}


###  READING ALL FILES ###
### 
###  ADD YOUR FILE NAMES HERE AND THE FILE PATH BELOW 
filelist <- c("L1_processed.csv","L2_B3_june.csv", "L1_B4_june.csv", "L2_B4_june.csv") #vector of files to read
f <- file.path("C:/Users/gero0008/Box Sync/PhD/R/Lightdata", filelist) #create filepaths for each file

dataset <- lapply(f, readfiles) #read all the files and place them in a list
names(dataset) <- gsub(".*/(.*)\\..*", "\\1", f)  #takes the names: searches for the last / and the "." and copys the text in it

sumary <- data.frame(names= names(dataset) ) #new dataframe with the summary values



#### MAIN LOOP OF THE SCRIPT:
 # 1. Remove outliers
 # 2. Sum the total PAR
 # 3. Save some key Variables for each site
 # 4. Make a plot of the before/after as a quality check
####


for(i in 1:length(dataset)){  #for each logger in the dataset
  
 dataset[[i]] <- Paroutliers(dataset[[i]]) #applies the function to remove outliers

 sumary$parTotal <- sum(dataset[[i]]$newpar) #PAR accumulated: sum all the PAR values 
 sumary$n_obs <- length(dataset[[i]]$newpar) #count the number of observations
 sumary$parMax <- max(dataset[[i]]$newpar) #maximum PAR value 
 sumary$parMean <- mean(dataset[[i]]$newpar) #mean PAR
 sumary$parMedian <- median(dataset[[i]]$newpar) #median PAR
 
 

 ####THIS IS TO MAKE  THE AVERAGE OF PAR OF A DEFINED PERIOD OF THE DAY, IT OUTPUTS A DAILY VALUE
  x <- as.xts(dataset[[i]]$paar, dataset[[i]]$date.time) #construct an xts object with the variable we want
  Par_midday <- x["T10:00/T16:00"]  #take only the desired period
  ep <- endpoints(Par_midday,'days') #make this object to select by "days", it can also be "weeks", or "month"
  Par_midday_daily <- period.apply(Par_midday,ep,mean) #obtain the mean (or any other function for each day
  
  ### Only saves the mean for the midday periods. It cn be changed to export an individual file for each site
  sumary$parMean_midday <- mean(Par_midday_daily[,1])
  sumary$parLost <- sum(dataset[[i]]$paar - dataset[[i]]$newpar)/sum(dataset[[i]]$paar) #in the summary it makes the sum of the PAR removed, as a quality check
  
  PlotPar(dataset[[i]], sumary[i,1] ) #Make a plot for each file and save is a pdf. see function above
 
}
  
write.csv(x=sumary, file ="Light_data_processed.csv")
