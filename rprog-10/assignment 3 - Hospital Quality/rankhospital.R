rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	state_data <- subset(data, State == state)

	rank_state <- function(state_data, column, num){
		suppressWarnings(state_data[, column] <- as.numeric(state_data[, column]))
		##sorted_state_data <- rank(state_data, na.last=NA) 
		cleaned_state_data <- state_data[complete.cases(state_data[column]), ]

		if (nrow(state_data) == 0) {
        	        stop("invalid state")
	        }

		if (num == "best") { line <- 1 }
		else if (num == "worst") { line <- nrow(cleaned_state_data) }
		else if (num > 1 && num < nrow(cleaned_state_data)) { line <- num }
		else { stop("invalid num") }

		sorted_cleaned_state_data <- cleaned_state_data[order(cleaned_state_data[[column]]) , ]
		result <- sorted_cleaned_state_data[line, ]$Hospital.Name
		##result <- sorted_cleaned_state_data[[2]]
		return(result)
	}

	## Return hospital name in that state with the given rank
	## 30-day death rate
	        if (outcome == "heart attack") {
                ##[11] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                result <- rank_state(state_data, 11, num)
        }
        else if (outcome == "heart failure"){
                ##[17] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                result <- rank_state(state_data, 17, num)
        }
        else if (outcome == "pneumonia"){
                ##[23] Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                result <- rank_state(state_data, 23, num)
        }
        else{
        	stop("invalid outcome")
	}
	
	return(result)
}
