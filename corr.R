corr <- function(directory, threshold = 0) {
        ## Read the file containing the results of the complete(...) function run over all 332 .csv files
        results <- read.csv("completeobs.csv", header = TRUE, sep = ",")
        ## Set up working directory
        root <- getwd()
        names <- list.files(directory)
        setwd(directory)
        
        ## Eliminate all non-qualifying entries accordingly to the specified threshold
        newresults <- subset(results, nobs > threshold, select = c(id, nobs))
        if(nrow(newresults) == 0){
                cr <- 0
                return(cr)
        }
        
        ## Pack up a vector that stores all IDs above the threshold. This will be an input for the pollutantcorr(...) function
        newid <- newresults$id
        
        ## Define the local pollutantcorr(...) function
        pollutantcorr <- function(id = 1:332){
                
                ## Create the files vector
                files <- sample(10,length(id), replace = TRUE)
                for(i in 1:length(id)){
                        files[i] <- names[id[i]]
                }
                ## For each file that meets the threshold, read the raw data and calculate the correlation
                cr <- sample(1, length(id), replace = TRUE)
                for(i in 1:length(id)){
                        monitor <- read.csv(files[i], header = TRUE)
                        monitor <- subset(monitor, !is.na(monitor$sulfate) == TRUE, select = Date:ID)
                        monitor <- subset(monitor, !is.na(monitor$nitrate) == TRUE, select = Date:ID)
                        cr[i] <- cor(monitor$sulfate, monitor$nitrate)
                }
                cr <- subset(cr, !is.na(cr) == TRUE)
        }
        ## Run the pollutantcorr(...) function with the input vector that satisfies the threshold
        crresults <- pollutantcorr(newid)
        setwd(root)
        crresults
}