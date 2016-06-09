rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
	
	list_state <- unique(data[,7]) ##take the unique states and put it into another list
	
	
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
	
	hospital = data.frame(x = character(), y = character(), stringsAsFactors = FALSE)
	
	## For each state, find the hospital of the given rank
	
	for (i in 1:length(list_state)) {
		subdata <- data[data$State == list_state[i],c(2,7,outcol)]
		colnames(subdata) <- c("hospitalname" ,"state", cname)
		finaldata <- na.omit(subdata)
		finaldata[,cname] <- as.double(finaldata[,3])
		
		## Return hospital name in that state with the given rank
		findata <- finaldata[order(finaldata[,cname],finaldata[,"hospitalname"]),]
		findata[,4] <- c(1:nrow(findata))
		colnames(findata) <- c("hospitalname" ,"state", cname, "Rank")
		if (num == "best") {
			hospital[i,1:2] <- findata[1,1:2]
			hospital[i,2] <- list_state[i]
		}
		else if (num == "worst") {
			hospital[i,1:2] <- findata[nrow(findata),1:2]
			hospital[i,2] <- list_state[i]
		}
		else 
			hospital[i,1:2] <- findata[num,1:2]
			hospital[i,2] <- list_state[i]
	}
	
	rankhospital <- hospital[order(hospital[,2]),]
	colnames(rankhospital) <- c("hospital", "state")
	rankhospital
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
}