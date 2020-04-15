rankhospital <- function(state = "TX", outcome = "heart attack", num = 4)
{
        ## Read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[,11] <- as.numeric(data[,11])
        data[,17] <- as.numeric(data[,17])
        data[,23] <- as.numeric(data[,23])
        datatbl <- tbl_df(data)
        
        
        ## Check that state and outcome are valid 
        
        if(outcome == "heart attack") {
                ## Return hospital name in that state with lowest 30-day death rate
                grbySheet <- 
                        datatbl %>%
                        select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name) %>%
                        filter(State == state) %>%
                        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name) }
        
        else if (outcome == "heart failure") {
                ## Return hospital name in that state with lowest 30-day death rate
                grbySheet <- 
                        datatbl %>%
                        select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
                        filter(State == state) %>%
                        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name) }
        
        else if (outcome == "pneumonia") {
                ## Return hospital name in that state with lowest 30-day death rate
                grbySheet <- 
                        datatbl %>%
                        select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
                        filter(State == state) %>%
                        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) }
        
        else {return("Error")}
        
        
        grbySheet[num,]
        
}
