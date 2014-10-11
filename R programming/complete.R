complete <- function(directory, id = 1:332) {
        
        count = 1
        
        results <- data.frame(id=NA,nobs=NA)
                #nrow=length(id),ncol=2,dimnames=list(c(),c("id","nobs")))        
                
        filenames <- c(sprintf("%03d.csv", c(id)))
        
        allfiles <- file.path(directory, c(filenames))

        for (file in allfiles) {
                raw_dat <- read.csv(file)
                results[count,1] <- raw_dat[1,4]# or raw_dat["id"]
                results[count,2] <- NROW(na.omit(raw_dat))
                count <- count + 1
        }
        print(results)        
}