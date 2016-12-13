rankhospital <- function(state, outcome, rank = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  source("Best.R")
  if(rank == "best"||rank == "Best"||rank == 1){return(best(state, outcome))}
  if(outcome == "heart attack"||outcome == "Heart Attack"){outcome = 11}
  if(outcome == "heart failure"||outcome == "Heart Failure"){outcome = 17}
  if(outcome == "pneumonia"||outcome == "Pneumonia"){outcome = 23}
  if(!is.numeric(outcome)){stop("Invalid Outcome!")}
  if((rank == "worst")||(rank == "Worst")){
    maxpos <- NA
    for(rows in 1:nrow(data)){
      if(data[rows, "State"] == state){
        if(is.na(maxpos)){maxpos <- rows}
        if(data[rows, outcome] != "Not Available"){
          if((as.numeric(data[rows, outcome]) > as.numeric(data[maxpos, outcome]))){maxpos = rows}
        }
      }
    }
    if(is.na(maxpos)){stop("Invalid State!")}
    return(data[maxpos, "Hospital.Name"])
  }
  counter = 1
  while(counter < rank){
    minpos <- NA
    for(rows in 1:nrow(data)){
      if(data[rows, "State"] == state){
        if(is.na(minpos)){minpos <- rows}
        if(data[rows, outcome] != "Not Available"){
          if(as.numeric(data[rows, outcome]) < as.numeric(data[minpos, outcome])){
            minpos = rows
          }
        }
      }
    }
    if(is.na(minpos)){return(NA)}
    output = data[minpos, "Hospital.Name"]
    data <- data[-minpos, ]
    counter <- counter + 1
  }
  return(output)
}