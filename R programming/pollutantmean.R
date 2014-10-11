pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        filenames <- c(sprintf("%03d.csv", c(id)))
        
        allfiles <- file.path(directory, c(filenames))
        
        numbers <- matrix(data=NA,nrow=1,ncol=4,dimnames=list(c(),c("Date","sulfate","nitrate","ID")))
        
        for (file in allfiles) {
                raw_dat <- read.csv(file)
                numbers <- rbind(numbers,raw_dat)
        }
            
        pollutmean <- mean(numbers[pollutant][!is.na(numbers[pollutant])])       
        pollutmean
}