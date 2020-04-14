best <- function(state = "TX", outcome = "heart attack")
{
        ## Read data
        data <- read.csv("outcome-of-care-measures.csv")
        datatbl <- tbl_df(data)
        
        
        ## Check that state and outcome are valid 
        
        
        if(outcome == "heart attack") {
                ## Return hospital name in that state with lowest 30-day death rate
                grbySheet <- 
                        datatbl %>%
                        select(7, 2, 11) %>%
                        filter(State == state) %>%
                        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name) }
        
        else if (outcome == "heart failure") {
                ## Return hospital name in that state with lowest 30-day death rate
                grbySheet <- 
                        datatbl %>%
                        select(7, 2, 17) %>%
                        filter(State == state) %>%
                        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name) }
        
        else if (outcome == "pneumonia") {
                ## Return hospital name in that state with lowest 30-day death rate
                grbySheet <- 
                        datatbl %>%
                        select(7, 2, 23) %>%
                        filter(State == state) %>%
                        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) }
        
        else {return("Error")}
        
        
        grbySheet[1,2]
        
}