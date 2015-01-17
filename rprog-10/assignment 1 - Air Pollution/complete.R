complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

		sumnobs <- function(id) {
			file <- file.path(directory, paste(sprintf("%03d", as.numeric(id)), ".csv", sep=""))
			return (sum(complete.cases(read.csv(file))))
		}

		return(data.frame(id=id, nobs=sapply(id,sumnobs)))
}

##complete("D:/Users/goessingerp/Downloads/specdata", c(2, 4, 8, 10, 12))
##complete("D:/Users/goessingerp/Downloads/specdata", 30:25)
##complete("D:/Users/goessingerp/Downloads/specdata", 3)