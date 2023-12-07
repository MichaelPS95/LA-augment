
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

format_file <- function(LA=NULL, params=NULL){
  
  # the locating array has to be provide in order to check it
  if(is.null(LA)){
    stop("The design file must be provided.")
  }
  
  # # I am assuming that if you don't pass parameters to generate a design that it's
  # # already in the proper format to be checked (since I will need these parameters
  # # to fix the format)
  # if(is.null(params)){
  #   command <- paste("./check -sv <", LA, "> check_output.txt",sep=" ")
  #   print(command)
  #   system(command, intern=TRUE)
  # }
  # 
  # else{
    #suppressing warning here because the file is not "proper" table
    suppressWarnings({params <- read.table(file=params, sep="\t",header=FALSE)})
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
    cat(file=file_conn, "\n")
    # Close the file
    close(file_conn)

}


add_separation_row <- function(LA=NULL, rows_to_add=1, t=2, append_path=""){
  if(is.null(LA)){
    stop("No design specified.")
  }
  
  else{
    command <- paste("./check -sv < formatted_LA.txt > check_output.txt",sep=" ")
    system(command, intern = TRUE)
    command <- paste("python3 add_row.py", LA, "check_output.txt", rows_to_add, t, append_path, sep=" ")
    print(command)
    system(command, intern=TRUE)
  }
}

augment_design <- function(design_path=NULL, design_params=NULL, outfile="out.tsv", flag=0, aug_num=1, aug_fix=0.5, d=1, t=2, delta=1){
  # flag = 0 will augment by randomly selecting some # rows based on aug_num
  # flag = 1 will augment by adding D-Optimal rows based on aug_num
  # flag = 2 will add rows based on separation in LA design
  
  # include necessary packages
  library(AlgDesign)
  library(dplyr)
  
  design <- data.frame()
  
  # check if both design and parameters are missing
  if(is.null(design_path) && is.null(design_params)){
    stop("No design and no design parameters given.")
  }
  # check design is not NULL
  if(is.null(design_path)){
    stop("No design given.")
  }
  # check if parameters are missing
  if(is.null(design_params)){
    stop("No design parameters given.")
  }
  
  # read in design that's given
  design = read.table(file=design_path, sep="\t",header=FALSE)
  design <- design[, -ncol(design)]

  # NOT SURE I WANT TO ALLOW THIS, I FEEL LIKE WHY ARE YOU GOING TO AUGMENT A DESIGN YOU JUST MADE
  # else{
  #   # generate the design
  #   generate_path = paste(getwd(),"/generate", sep="")
  #   design <- generate_array(generate_path, design_params, outfile, d, t, delta)
  # }
  
  
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
  rows_to_add <-as.integer(aug_num)
  if((rows_to_add + nrow(design)) > nrow(factorial)){
    rows_to_add = as.integer((nrow(factorial)-nrow(design)) * aug_fix)
  }
  print("rows to add")
  print(rows_to_add)
  if(flag == 0){
    print("flag == 0")
    start_row <- nrow(design) + 1
    print("design")
    print(design)
    
    print("factorial")
    print(factorial)
    for (i in 1:rows_to_add) {
      rand_index = sample(nrow(factorial), 1)
      while(!any(duplicated(rbind(design, factorial[rand_index, ])))){
        print(rbind(design, factorial[rand_index, ]))
        rand_index = sample(nrow(factorial), 1)
        # print(rand_index)
      }
      # print(rand_index)
      design <- rbind(design, factorial[rand_index,])
    }
    # this just renames the rows, they assign weird numbers if they choose
    # say row 24 from factorial where row 24 (the name) already exists in design
    rownames(design) <- 1:nrow(design)
    
    # Map them back to LA values
    for(i in 1:nrow(design)){
      for(j in 1:ncol(design)){
        design[i,j] <- design[i,j] - 1
      }
    }
    write.table(design[start_row:nrow(design), ], file = design_path, append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t", eol = "\t\n")
    return(design)
    
  }
  else if(flag == 1){
    
    design_rows <- nrow(design)
    start_row <- nrow(design) + 1
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
    for(i in 1:nrow(design)){
      for(j in 1:ncol(design)){
        design[i,j] <- design[i,j] - 1
      }
    }
    write.table(design[start_row:nrow(design), ], file = design_path, append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t", eol = "\t\n")
    return(design)
  }
  
  else if(flag == 2){
    format_file(design_path, design_params)
    add_separation_row("formatted_LA.txt", append_path=design_path)
  }
  
}

# function(design_path=NULL, design_params=NULL, outfile="out.tsv", flag=0, aug_num=1, aug_fix=0.5, d=1, t=2, delta=1)
# flag = 0 will augment by randomly selecting some # rows based on aug_num
# flag = 1 will augment by adding D-Optimal rows based on aug_num
# flag = 2 will add rows based on separation in LA design


augment_design("/home/michael/Desktop/Designs/2_2_2_2_interaction/random/2_2_2_2_random.tsv","/home/michael/Desktop/Designs/2_2_2_2_interaction/2_2_2_2_params.tsv")

catch

library(AlgDesign)
fac <- gen.factorial(c(2,2,2,2),nVars=4,center=FALSE)
fac
opt <- optFederov(data=fac,center=FALSE)
opt
