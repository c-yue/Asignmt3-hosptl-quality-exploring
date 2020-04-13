#coding 1

best <- function(state = "TX", outcome = "heart attack")
{
        ## Read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data$State <- na.omit(data$State)
        
        
        ## Check that state and outcome are valid
        
        if(!state %in% data$State) {return("Error")}
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                                   {return("Error")}
        
        
        ## Return hospital name in that state with lowest 30-day death rate
        if (outcome == "heart attack")
        {
                data[, 11] <- as.numeric(data[, 11])
                data[, 11] <- na.omit(data[, 11])
                grbySheet <- sqldf("SELECT State, Hospital Name, MIN(Hospital 30-Day Death (Mortality) Rates from Heart Attack)
                FROM data GROUP BY State, Hospital Name")
        }
        
        grbySheet
        
}
