best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


	## Check that state and outcome are valid
	check_state <- function(state_data, column){
		suppressWarnings(state_data[, column] <- as.numeric(state_data[, column]))
		state_minimum <- min(state_data[, column], na.rm=TRUE)
                hospital_list <- subset(state_data, state_data[[column]] == state_minimum)
                return(hospital_list[[2]])
	}

	state_data <- subset(data, State == state) 

	if (nrow(state_data) == 0) {
		stop("invalid state")
	}

	if (outcome == "heart attack") {
		##[11] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
		result <- check_state(state_data, 11)
	}
	else if (outcome == "heart failure"){
		##[17] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
		result <- check_state(state_data, 17)
	}
	else if (outcome == "pneumonia"){
		##[23] Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
		result <- check_state(state_data, 23)
	}
	else{
		stop("invalid outcome")
	} 

	## Return hospital name in that state with lowest 30-day death
	## rate
	return(result)


}
