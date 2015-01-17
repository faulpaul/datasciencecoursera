corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

		cr <- c()
		files <- list.files(directory, full.names=TRUE)  

		for (i in 1:length(files)) {
			data <- read.csv(files[i])
			data <- data[complete.cases(data),]
			if ( nrow(data) > threshold ) {
				cr <- c(cr, cor(data$sulfate, data$nitrate) )
			}
		}

		return(cr)
}

##cr <- corr("D:/Users/goessingerp/Downloads/specdata", 150)
##head(cr)
##summary(cr)

##cr <- corr("specdata", 400)
##head(cr)
##summary(cr)
