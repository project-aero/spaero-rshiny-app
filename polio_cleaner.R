#Clean Pej's Polio data so that it is in Project Tycho format:
polioCleaner <- function(filename="./data/poli5180_modified.csv"){

	rd <- read.csv(filename, sep = ",", comment.char="#")
	names(rd) <- c("Year", "Month", "Cases", "State")
	year_range <- min(rd$Year):max(rd$Year)

	states <- unique(rd$State)
	data <- as.data.frame(matrix("-", ncol = 2+ length(states), nrow = 12*length(year_range)))
	names(data) <- c(c("Year","Month"), as.character(states))


	for(name in names(data)){
	data[name] <- rep("-",length(year_range))
	}

	data$Month <- rep(1:12,length(year_range))
	data$Year <- sort(rep(year_range, 12))


#Not efficient at all. Redo by looping over rows in rd or something
	for(state in as.character(states)){
		for(month in 1:12){
			for(year in year_range){
				temp <- rd[which(rd$State == state & rd$Year == year & rd$Month == month),]$Cases
				if(length(temp) ==1){
					data[which(data$Year ==year & data$Month == month), state] <- temp
				}
			}
		}
	}
	#Fudge:
	names(data) <- toupper(names(data))
	return(data.matrix(data))
}

#Takes as an input the data.matrix generated using polioCleaner
polioCleanedOutput <- function(data,filename){
writeLines( paste("\"Weekly Polio Cases, ",min(data[,"YEAR"]),"-",max(data[,"YEAR"]),"\"\n\"Data provided by ***, Data Version 1.0.0, released ***.\"" , sep=""), filename)
write.table(data, filename, row.names=FALSE, sep = ",", na="-", append = TRUE)
}

