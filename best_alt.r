best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
	
	## Check that state is valid
	list_state <- unique(data[,7]) ##take the unique states and put it into another list
	if (!(state %in% list_state)) { ## check if state given is in the list
		stop ("invalid state") ##stop running if it finds that the state is invalid
	}
	
	
	## Check that outcome is valid
	list_outcome <- c("heart attack", "heart failure", "pneumonia")
	if (!(outcome %in% list_outcome)) {
		stop ("invalid outcome")                 
	}
	
	## assign col num according to outcome
	if (outcome == "heart attack") {
		outcol <- 11
		cname <- "heartattack"
	}
	else if (outcome == "heart failure") {
		outcol <- 17
		cname <- "heartfailure"
	}
	else {
		outcol <- 23
		cname <- "pneumonia"
	}
	
	## extract the relevant outcome column and save it into another DF
	
	subdata <- data[data$State == state,c(2,7,outcol)]
	colnames(subdata) <- c("hospitalname" ,"state", cname )
	finaldata <- na.omit(subdata)
	finaldata[,cname] <- as.double(finaldata[,cname])
	
	## Return hospital name in that state with lowest 30-day death
	findata <- finaldata[order(finaldata[,cname],finaldata[,"hospitalname"]),]
	findata[,4] <- c(1:nrow(findata))
	colnames(findata) <- c("hospitalname" ,"state", cname, "Rank")
	dhospitals <- findata[1,1]
	
	
	## rate
	dhospitals
}
