remove_file <- function(filename=NULL){
  if(is.null(filename)){
    stop("No file was given.")
  }
  else{
    path = getwd()
    del = paste(path,"/",filename,sep="")
    command <- paste("rm", del,sep=" ")
    system(command, intern=TRUE)
  }
}

format_file <- function(LA=NULL, design_params=NULL){
  
  # the locating array has to be provide in order to check it
  if(is.null(LA)){
    stop("The design file must be provided.")
  }
  
  # I am assuming that if you don't pass parameters to generate a design that it's
  # already in the proper format to be checked (since I will need these parameters
  # to fix the format)
  if(is.null(design_params)){
    command <- paste("./check -sv <", LA, "> formatted_LA.txt",sep=" ")
    print(command)
    system(command, intern=TRUE)
  }
  
  else{
    #suppressing warning here because the file is not "proper" table
    suppressWarnings({params <- read.table(file=design_params, sep="\t",header=FALSE)})
    # read in the factor levels and convert to a vector of numbers to generate full factorial
    num_params <- as.numeric(params$V1[1])
    row2_string <- as.character(params[2, 1])
    values_list <- unlist(strsplit(row2_string, "\\s+"))
    
    # read in number of factors
    numeric_values <- as.numeric(values_list)
    
    # read in the LA
    design = read.table(file=LA, sep="\t",header=FALSE)
    design <- design[, -ncol(design)]
    
    # change column/row names to match gen.factorial() method name scheme
    new_column_names <- paste("X", 1:ncol(design), sep="")
    colnames(design) <- new_column_names
    
    l2 <- paste(nrow(design), ncol(design), sep="\t")
    
    # getting array output file/ header set up
    fname <- "formatted_LA.txt"
    
    write("v2.0", fname)
    write(l2, fname, append=TRUE)
    p= ""
    for(x in numeric_values){
      # print(x)
      p <- paste(p, x, sep="\t")
    }
    p <- substring(p, 2)
    write(p, fname, append=TRUE)
    
    for(i in 1:(ncol(design)+1)){
      write(0, fname, append=TRUE)
    }
    
    # appending the locating
    file_conn <- file(fname, open = "a")
    
    # Loop through each row and write to the file
    for (i in 1:nrow(design)) {
      row_values <- design[i, ]
      row_values <- paste(row_values, collapse = "\t")  # Concatenate values with tabs
      cat(row_values, file = file_conn, "\n")
    }
    
    # Close the file
    close(file_conn)
  }
}

calc_separation <- function(LA=NULL, params=NULL, t=2){
  
  # the locating array has to be provide in order to check it
  if(is.null(LA)){
    stop("The design file must be provided.")
  }
  
  # I am assuming that if you don't pass parameters to generate a design that it's
  # already in the proper format to be checked (since I will need these parameters
  # to fix the format)
  
  sep <- 0
  
  if(!is.null(params)){
    format_file(LA, params)
    command <- paste("./check -sv 1", t,  "< formatted_LA.txt > check_output.txt", sep=" ")
    # print(command)
    system(command, intern=TRUE)
    command <- "python3 calc_separation.py check_output.txt"
    # print(command)
    sep <- system2("python3", c("calc_separation.py", "check_output.txt"), stdout=TRUE)
    remove_file("formatted_LA.txt")
    remove_file("check_output.txt")
  }
  else{
    command <- paste("./check -sv 1", t,  "<", LA, "> check_output.txt", sep=" ")
    # print(command)
    system(command, intern=TRUE)
    sep <- system2("python3", c("calc_separation.py", "check_output.txt"), stdout=TRUE)
    remove_file("check_output.txt")
  }
  return(sep)
}

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


calc_statistics <- function(LA=NULL, design_params=NULL, t=2){
  # include necessary packages
  library(AlgDesign)
  library(dplyr)
  
  # read in design that's given
  design = read.table(file=LA, sep="\t",header=FALSE)
  design <- design[, -ncol(design)]
  
  # change column/row names to match gen.factorial() method name scheme
  new_column_names <- paste("X", 1:ncol(design), sep="")
  colnames(design) <- new_column_names
  
  # map all the LA values to factorial
  for(i in 1:nrow(design)){
    for(j in 1:ncol(design)){
      design[i,j] <- design[i,j] + 1
    }
  }
  # print(design)
  
  #suppressing warning here because the file is not "proper" table
  suppressWarnings({params <- read.table(file=design_params, sep="\t",header=FALSE)})
  
  # read in the factor levels and convert to a vector of numbers to generate full factorial
  num_params <- as.numeric(params$V1[1])               
  row2_string <- as.character(params[2, 1]) 
  values_list <- unlist(strsplit(row2_string, "\\s+"))
  
  # read in number of factors
  numeric_values <- as.numeric(values_list)
  
  # generate factorial design
  factorial <- gen.factorial(levels=numeric_values, nVars=num_params[1], center=FALSE)
  
  separ <- calc_separation(LA, design_params, t)
  bal <- calc_balance(LA, design_params)
  print(bal)
  print(separ)
  
  design_rows <- nrow(design)
  
  # augment using D criteria
  opt <- optFederov(data = design, nTrials=design_rows, augment=TRUE, criterion = "D", rows=1:design_rows)
  # print(opt)
  
  des <- round(opt$D, digits=5)
  print(des)
  
  stats <- paste("Balance = ", bal, ", Separation = ", separ, ", D-Optimal Criteria = ", des, sep="")
  print(stats)
}

calc_statistics("/home/michael/Desktop/function/TWC_LA.tsv","/home/michael/Desktop/function/TWC.tsv")

