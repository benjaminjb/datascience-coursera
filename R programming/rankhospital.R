rankhospital <- function(state, outcome, num = "best") {
        
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
        newhospital <- hospital[order(as.numeric(hospital[,switcher]),hospital[,2],na.last=NA),]
        
        if (num == "best")
                num <- 1
        if (num == "worst")
                num <- nrow(newhospital)
                        
        print(newhospital[num,2])
}