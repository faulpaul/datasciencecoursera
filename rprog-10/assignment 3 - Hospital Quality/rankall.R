rankall <- function(outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name

	rank_hospital <- function(state_data, num) {
		ordered_state_data <- order(state_data[3], state_data$Hospital.Name, na.last=NA)
		if (num == "best") {
			state_data$Hospital.Name[ordered_state_data[1]]
		} else if (num == "worst") {
			state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
		} else if (is.numeric(num)) {
			state_data$Hospital.Name[ordered_state_data[num]]
		} else {
			stop("invalid num")
		}
	}

	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	if (outcome == "heart attack") {
		##[11] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
		column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	}
	else if (outcome == "heart failure"){
		##[17] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
		column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	}
	else if (outcome == "pneumonia"){
		##[23] Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
		column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}
	else{
		stop("invalid outcome")
	}

	suppressWarnings(data[, column] <- as.numeric(data[, column]))

	##data_by_state <- split(data[, c("Hospital.Name", "State", column)], data$State)
	##data_by_state <- split(data[, c("Hospital.Name", "State", column)], data$State)
	data_by_state <- split(data[, c("Hospital.Name", "State", column)], data$State)

	result <- lapply(data_by_state, rank_hospital, num)
	result <- data.frame(hospital = unlist(result), state = names(result), row.names = names(result))

	return(result)
}
rankall("heart attack", 20)
