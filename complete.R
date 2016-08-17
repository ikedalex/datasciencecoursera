complete <- function(directory, id = 1:332) {
        
        ## Save original working directory and change it to the specified directory
        root <- getwd()
        names <- list.files(directory)
        setwd(directory)
        
        ## Build a big data frame with the files containing the specified IDs
        files <- sample(10, length(id), replace = TRUE)
        for(i in 1:length(id)) {
                files[i] <- names[id[i]]
        }
        bigdata <- read.csv(files[1])
        ## Check how many entries have full observations in both columns 2 and 3 simultaneously.
        ## For each ID number, count the total amount of full observations
        if(length(id) == 1) {
                sulfatena <- !is.na(bigdata$sulfate)
                nitratena <- !is.na(bigdata$nitrate)
                nobs <- sum((sulfatena & nitratena))
        }
        else {  
                nobs <- sample(length(id), length(id), replace = TRUE)
                ## Keep on reading the .csv files
                for(i in 2:length(id)) {
                        data <- read.csv(files[i], header = TRUE)
                        bigdata <- rbind(bigdata,data)
                }
                ## For each ID number, subset the data frame, test for NAs, then count the number of observations
                for(i in 1:length(id)) {
                        newdata <- subset(bigdata, ID == id[i], select = c(Date, sulfate, nitrate, ID))
                        sulfatena <- !is.na(newdata$sulfate)
                        nitratena <- !is.na(newdata$nitrate)
                        nobs[i] <- sum((sulfatena&nitratena))
                }
        }
        ## Set up a data frame containing two columns: ID and n.obs
        results <- data.frame(id, nobs)
        setwd(root)
        results
}