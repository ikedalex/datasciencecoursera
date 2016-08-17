pollutantmean <- function(directory, pollutant, id = 1:332) {
        root <- getwd()
        names <- list.files(directory) ## Creates a char vector with all names of the .csv files
        setwd(directory)
        files <- sample(10,length(id), replace = TRUE)
        
        for(i in 1:length(id)){
                files[i] <- names[id[i]]
        }
        ## Makes a big data frame out of the specified files
        bigdata <- read.csv(files[1])
        if(length(id) == 1){
                if(pollutant == "sulfate"){
                        bigmean <- mean(bigdata$sulfate, na.rm = TRUE)
                }
                if(pollutant == "nitrate"){
                        bigmean <- mean(bigdata$nitrate, na.rm = TRUE)
                }
                setwd(root)
                bigmean
        }
        else{
                for(i in 2:length(id)){
                        data <- read.csv(files[i], header = TRUE)
                        bigdata <- rbind(bigdata,data)
                }
                if(pollutant == "sulfate"){
                        bigmean <- mean(bigdata$sulfate, na.rm = TRUE)
                }
                if(pollutant == "nitrate"){
                        bigmean <- mean(bigdata$nitrate, na.rm = TRUE)
                }        
        }
        setwd(root)
        bigmean
}