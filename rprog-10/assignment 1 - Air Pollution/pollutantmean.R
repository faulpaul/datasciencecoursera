pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

		poll_data <- c()
		files <- list.files(directory, full.names=TRUE)
		for (i in id) {
			file <- read.csv(files[i], header=TRUE, sep=",")
			na_removed <- file[!is.na(file[, pollutant]), pollutant]
			poll_data <- c(poll_data, na_removed)
		}
		return(round(mean(poll_data),3))
}

##pollutantmean("D:/Users/goessingerp/Downloads/specdata", "nitrate", 23)  == 1.281
##pollutantmean("D:/Users/goessingerp/Downloads/specdata", "sulfate", 1:10) == 4.064
##pollutantmean("D:/Users/goessingerp/Downloads/specdata", "nitrate", 70:72) == 1.706