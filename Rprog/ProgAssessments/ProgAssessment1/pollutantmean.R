pollutantmean <- function(directory = "./specdata", pollutant, id = 1:332) {
  originalDirectory = getwd()
  setwd(directory)
  temp <- list.files()[id]
  myfiles <- lapply(temp, read.csv)
  myDataSet <- do.call("rbind", myfiles)
  print(mean(myDataSet[, pollutant], na.rm = T))
  setwd(originalDirectory)
}