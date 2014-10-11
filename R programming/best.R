best <- function(state, outcome) {
        
        data <- read.csv("~/datasciencecoursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        states <- unique(data$State)
        if (!(state %in% states)) 
                stop("invalid state")
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) 
                stop("invalid outcome")
        switcher <- grep(outcome,c("heart attack","heart failure","pneumonia"))
        if (switcher == 1)
                switcher = switcher + 10
        if (switcher == 2)
                switcher = switcher + 15
        if (switcher == 3)
                switcher = switcher + 20        
        
        hospital <- as.data.frame(split(data,data["State"])[state])
        newhospital <- hospital[order(as.numeric(hospital[,switcher])),] 
        
        count <- 1
        
        lowhospital <- as.data.frame(matrix(data=NA,nrow=1,ncol=46,dimnames=list(c(),c(names(hospital)))))
        
        lowest <- newhospital[1,switcher]
        while (newhospital[count,switcher] == lowest) {
                lowhospital[count,] <- newhospital[count,]
                count = count + 1
        }
        alphabetical <- lowhospital[order(lowhospital[,2]),]
        print(alphabetical[1,2])        
}