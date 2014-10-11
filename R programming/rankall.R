rankall <- function(outcome, num = "best") {
        
        data <- read.csv("~/datasciencecoursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
                stop("invalid outcome")
        
        switcher <- grep(outcome,c("heart attack","heart failure","pneumonia"))
        if (switcher == 1)
                switcher = switcher + 10
        if (switcher == 2)
                switcher = switcher + 15
        if (switcher == 3)
                switcher = switcher + 20        
                
        results <- matrix(data=NA,nrow=54,ncol=2,dimnames=list(c(),c("hospital","state")))
        
        hospital <- split(data,data["State"])
        states <- unique(data$State)
        states <- sort(states)
        
        count = 1
        if (num == "best")
                counter <- 1
        if (num == "worst")
                counter <- 0
        else 
                counter <- num
        
        for (i in 1:54) {
                x <- as.character(states[i])
                y<-as.data.frame(hospital[x])
                newhospital <- y[order(as.numeric(y[,switcher]),y[,2],na.last=NA),]
                
                if (num == "worst")
                        counter <- nrow(newhospital)
                
                
                results[count,1] <- newhospital[counter,2]
                results[count,2] <- x
                count <- count + 1
                
        }
        print(data.frame(results))
}