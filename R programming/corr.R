corr <- function(directory, threshold = 0) {
        
        allfiles <- file.path(directory, c(list.files(directory)))
        
        results <- c()
        
        for (file in allfiles) {
                raw_dat <- read.csv(file)
                test <- NROW(na.omit(raw_dat))
                if (test > threshold) {
                        results <- append(results,cor(raw_dat$sulfate,raw_dat$nitrate,use="pairwise.complete.obs"))
                }
        }
        print(results)        
}