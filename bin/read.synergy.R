################################################################################
# Growth Curve Data Input From Synergy MX                                      #
#                                                                              #
#	Written by M. Muscarella                                                     #
#                                                                              #
#	Last update: 1/17/13                                                         #
#                                                                              #
# Features:                                                                    #
#   Reads in the data table from the symergy mx machine                         #
#   Selects only the matrix of plate data                                      #
#   Converts time to minutes                                                   #
#   Checks the type of all values & changes to numeric if needed               #
#   Outputs data matrix {Time, Temp, Well...                                   #
################################################################################

read.synergy <- function(input = " ", sep = ","){
  temp <- readLines(input, warn = F)
  t0 <- which(grepl("0:00:00", temp))[1]
  tf <- which(grepl("Results", temp)) - 2
  data.in <- read.csv2(input, sep = ",", header = F)
  names <- as.character(data.in[t0 - 1, ])
  names[2] <- "Temp"
  data.out <- data.in[t0:tf, ]
  colnames(data.out) <- names
  data.out$Time <- as.character(data.out$Time)
  t.h <- as.numeric(lapply(strsplit(data.out$Time, "\\:"), "[", 1))
  t.m <- as.numeric(lapply(strsplit(data.out$Time, "\\:"), "[", 2))
  data.out$Time <- round(t.h + t.m/60, 2)
  for (i in 1:dim(data.out)[2]){
    if (is.numeric(data.out[,i]) == FALSE){data.out[,i] = as.numeric(data.out[,i])}}
  return(data.out)
  }
