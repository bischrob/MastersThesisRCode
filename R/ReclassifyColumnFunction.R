# function to convert specified columns to specified class type
# df <- newdb1
# c <- c(2:138)
# i = 1
# type = "integer"
reclass <- function(df,c, type){
  # df is a dataframe with columns to reclassify
  # c is a vector of column numbers
  # type is the class to change to
    # only works for character, numeric, and integer
  df <- data.frame(df)
  if(type == "numeric"){
    for (i in c){
      class(df[,i]) <- 'numeric'
    }    
  } else if (type == "integer"){
    for (i in c){
      class(df[,i]) <- 'integer'
    }  
  } else if (type == "character"){
    for (i in c){
      class(df[,i]) <- 'character'
    }  
  } else {
    stop("Only works if type is integer, character, or numeric")
  }
  df <- as.tibble(df)
  return(df)
}


