corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  files <- list.files(directory, full.names=TRUE);
  comp_dat <- complete(directory);
  id <- comp_dat$id[comp_dat$nobs > threshold];
  
  dat <- vector(mode="numeric", length=0);
  
  for(i in id) {
    files_dat <- read.csv(files[i]);
    
    dat <- c(dat, cor(files_dat$sulfate, files_dat$nitrate, use="complete.obs"))
  }
  
  return(dat);
}