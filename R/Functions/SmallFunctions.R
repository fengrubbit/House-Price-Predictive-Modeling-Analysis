####
# Some useful function might be used in the analysis
####

# Create the function such that replace the NA in the factor column by "Other"
NAReplace_factor <- function(x){
  x <- factor(x, levels = c(levels(x), "Other"))
  x[is.na(x)]<-"Other"
  return(x)
}

# Create the function such that replace the NA in the character column by "Other"
NAReplace_Char <- function(x){
  x[is.na(x)]<-"Other"
  return(x)
}
