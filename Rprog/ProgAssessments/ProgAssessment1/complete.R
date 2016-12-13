#The complete.R function accepts a directory containing the specdata, and a numeric vector of ids that the user wants the program to consider. It outputs a data frame containing
#each id as well as the number of complete cases in each file.
complete <- function(directory = "./specdata", id = 1:332) {
  originalDirectory = getwd()
  setwd(directory)
  temp <- list.files()[id]
  myfiles <- lapply(temp, read.csv)
  CompleteCases <- lapply(myfiles, complete.cases)
  print(data.frame(id <- id, nobs = unlist(lapply(CompleteCases, sum))))
  setwd(originalDirectory)
}