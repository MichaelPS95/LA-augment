
# ./generate 1 2 1 -v Sample-Input/TWC.tsv out.tsv

generate_array <- function(array_executable=NULL, params=NULL, outfile=NULL, d=1, t=2, delta=1){
  if(is.null(array_executable)){
    array_executable <- paste(getwd(), "/generate", sep="")
  }
  if(is.null(params)){
    stop("There was no input file given.")
  }
  
  library(Rcpp)
  if(is.null(outfile)){
    outfile <- paste(getwd(), "/out.tsv", sep="")
  }
  
  command <- paste(array_executable, params, outfile, d, t, delta, sep=" ")
  # print(command)
  system(command, intern=TRUE)
  design = read.table(file=outfile, sep="\t",header=FALSE)
  
  # Isaacs code generates TSV where each line is ended with a tab, 
  # which creates N/A values so remove that column from design
  design <- design[, -ncol(design)]
  print(design)
  return(design)
}

generate_array("/home/michael/Desktop/function/generate", "/home/michael/Desktop/function/TWC.tsv")

current_directory <- getwd()
current_directory



augment_design <- function(design_path=NULL, design_params=NULL, outfile="out.tsv", flag=0, aug_percent=0.25, aug_fix=0.5, d=1, t=2, delta=1){
  # flag = 0 will augment by randomly selecting some # rows based on aug_percent
  # flag = 1 will augment by adding D-Optimal rows based on aug_perecent
  # TODO :: flag = 2 will add rows based on separation in LA design
  
  # include necessary packages
  library(AlgDesign)
  library(dplyr)
  
  design <- data.frame()
  
  # check if both design and parameters are missing
  if(is.null(design_path) && is.null(design_params)){
    stop("No design and no design parameters given.")
  }

  # check if parameters are missing
  if(is.null(design_params)){
    stop("No design parameters given.")
  }
  
  if(!is.null(design_path)){
    # read in design that's given
    design = read.table(file=design_path, sep="\t",header=FALSE)
    design <- design[, -ncol(design)]
  }
  else{
    # generate the design
    generate_path = paste(getwd(),"/generate", sep="")
    design <- generate_array(generate_path, design_params, outfile, d, t, delta)
  }
  
  
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
  
  # add % of base design rows randomly selected from factorial to LA
  # check for overflow error edge case and reset augment amount
  rows_to_add <-as.integer(nrow(design) * aug_percent)
  if((rows_to_add + nrow(design)) > nrow(factorial)){
    rows_to_add = as.integer((nrow(factorial)-nrow(design))* aug_fix)
  }
  
  if(flag == 0){

    for (i in 1:rows_to_add) {
      rand_index = sample(nrow(factorial), 1)
      while(anyDuplicated(rbind(design, factorial[rand_index, ]))){
        rand_index = sample(nrow(factorial), 1)
      }
      # print(rand_index)
      design <- rbind(design, factorial[rand_index,])
    }
    # this just renames the rows, they assign weird numbers if they choose
    # say row 24 from factorial where row 24 (the name) already exists in design
    rownames(design) <- 1:nrow(design)
    return(design)
    
  }
  
  else if(flag == 1){
    
    design_rows <- nrow(design)
    
    #get number of trials for optFederov function
    trials <- rows_to_add + nrow(design)
    
    # ran into an issue where appending factorial would sometimes repeat rows
    # so I filter the factorial to remove the base design rows
    filtered_factorial <- suppressMessages(anti_join(factorial, design))
    factorial <- rbind(design, filtered_factorial)
    
    # augment using D criteria
    opt <- optFederov(data = factorial, nTrials=trials, augment=TRUE, criterion = "D", rows=1:design_rows)
    # print(opt)
    
    design <- opt$design
    rownames(design) <- 1:nrow(design)
    
    return(design)
    
  }
  
}

augment_design(design_params="/home/michael/Desktop/function/TWC.tsv", aug_percent =  .25)

augment_design(design_params="/home/michael/Desktop/function/TWC.tsv", flag=1)

augment_design(design_path="/home/michael/Desktop/function/out.tsv", design_params="/home/michael/Desktop/function/TWC.tsv")

augment_design(design_path="/home/michael/Desktop/function/out.tsv", design_params="/home/michael/Desktop/function/TWC.tsv", flag=1)

augment_design(design_path="/home/michael/Desktop/function/out.tsv", flag=1)

augment_design(flag=1)

augment_design(design_params="/home/michael/Desktop/function/TWC.tsv", flag=1, d=1, t=2, delta=2)

augment_design(design_params="/home/michael/Desktop/function/trivial.tsv", flag=1, d=1, t=2, delta=2)
