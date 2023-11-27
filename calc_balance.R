


calc_balance <-function(LA=NULL, params=NULL){
  
  # need an LA to calculate the balance of
  if(is.null(LA)){
    stop("There is no locating array provided")
  }
  # need params to calculate balance, I'm assuming the person using this
  # has access to parameters if they have the array to avoid computing this
  if(is.null(params)){
    stop("There are no parameters specified")
  }
  
  #suppressing warning here because the file is not "proper" table
  suppressWarnings({params <- read.table(file=params, sep="\t",header=FALSE)})
  # read in the factor levels and convert to a vector of numbers to generate full factorial
  num_params <- as.numeric(params$V1[1])
  row2_string <- as.character(params[2, 1])
  values_list <- unlist(strsplit(row2_string, "\\s+"))

  # read in number of factors
  numeric_values <- as.numeric(values_list)
  param_vals <- data.frame(params_values = numeric_values)
  
  # read in the LA
  design = read.table(file=LA, sep="\t",header=FALSE)
  design <- design[, -ncol(design)]

  # change column/row names to match gen.factorial() method name scheme
  new_column_names <- paste("X", 1:ncol(design), sep="")
  colnames(design) <- new_column_names
  
  # set up list to get counts for parameter values
  balance_count <- list()
  
  for(i in values_list){
    size <- round(as.numeric(i))
    row <- rep(0, size)
    balance_count <- c(balance_count, list(row))
  }
  
  # get occurrence counts for parameter values
  for(i in 1:nrow(design)){
    for(j in 1:ncol(design)){
      balance_count[[j]][design[i, j] + 1] <- balance_count[[j]][design[i, j] + 1] + 1
    }
  }
  
  # calculate the balance
  total <- 0.0
  count = 0
  min = 2147483647
  for(list in balance_count){
    for(num in list){
      total <- total + num
      count <- count + 1
      if(num < min){
        min <- num
      }
    }
  }

  avg <- total/count
  bal <- round(min/avg, digits=5)
  return(bal)
}


calc_balance("/home/michael/Desktop/function/TWC_LA.tsv","/home/michael/Desktop/function/TWC.tsv")

