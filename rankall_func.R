rankall <- function(outcome = "heart attack", num = 1)
{
        ## Read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[,11] <- as.numeric(data[,11])
        data[,17] <- as.numeric(data[,17])
        data[,23] <- as.numeric(data[,23])
        datatbl <- tbl_df(data)
        
        if (num == "best") {num <- 1}
        
        
        ## Check that state and outcome are valid 
        
        if(outcome == "heart attack") 
        {
                ## Return hospital name in that state with lowest 30-day death rate
                
                outTable <- data.frame()
                outTable <- tbl_df(outTable)
                
                grList <- 
                        datatbl %>%
                        select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
                        group_by(State) %>%
                        group_split()
                
                for (tbl in grList) 
                {
                        tbl <- 
                                tbl%>%
                                select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
                                arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
                        outTable <- rbind(outTable, tbl[num,])
                        outTable <- arrange(outTable, State, Hospital.Name)
                }
                
        }
        
        
        else if (outcome == "heart failure") 
                {
                
                ## Return hospital name in that state with lowest 30-day death rate
                
                outTable <- data.frame()
                outTable <- tbl_df(outTable)
                
                grList <- 
                        datatbl %>%
                        select(State, Hospital.Name, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
                        group_by(State) %>%
                        group_split()
                
                for (tbl in grList) 
                {
                        tbl <- 
                                tbl%>%
                                select(State, Hospital.Name, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
                                arrange(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
                        outTable <- rbind(outTable, tbl[num,])
                        outTable <- arrange(outTable, State, Hospital.Name)
                }
                
                }
        
        
        else if (outcome == "pneumonia") 
                {
                
                ## Return hospital name in that state with lowest 30-day death rate
                
                outTable <- data.frame()
                outTable <- tbl_df(outTable)
                
                grList <- 
                        datatbl %>%
                        select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
                        group_by(State) %>%
                        group_split()
                
                for (tbl in grList) 
                {
                        tbl <- 
                                tbl%>%
                                select(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
                                arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)
                        outTable <- rbind(outTable, tbl[num,])
                        outTable <- arrange(outTable, State, Hospital.Name)
                }
                
                }
        
        else {return("Error")}
        
        
        outTable
        
}
