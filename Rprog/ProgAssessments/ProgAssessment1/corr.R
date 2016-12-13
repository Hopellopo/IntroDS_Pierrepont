#The corr function accepts a directory that contains specdata and a threshold of the miniumum number of complete cases that you wish to consider. Its output is a numeric vector
#containing the correlation of sulfate and nitrate according to the cor function of each specdata file that meets the minimum threshold.
corr <- function(directory = "./specdata", threshold = 0){
  originalDirectory = getwd() 
  setwd(directory)
  temp <- list.files()[1:332]
  myfiles <- lapply(temp, read.csv)
  l <- 1
  output = numeric(332)
  while(l < 332) {
    if (sum(complete.cases(myfiles[[l]])) > threshold){
      output[l] = cor(myfiles[[l]]["sulfate"], myfiles[[l]]["nitrate"], use = "complete.obs")
    }
    l = l + 1
  }
  setwd(originalDirectory)
  return(output[output != 0])
}
