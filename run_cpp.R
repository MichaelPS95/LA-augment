

# ./generate 1 2 1 -v Sample-Input/TWC.tsv out.tsv

generate_array <- function(design_path=NULL, infile=NULL, outfile="output/out.tsv", d=1, t=2, delta=1){
  if(is.null(design_path) || is.null(infile)){
    stop("There was no executable and no input file given.")
  }
  if(is.null(infile)){
    stop("There was no input file given.")
  }
  
  library(Rcpp)
  library(RcppArmadillo)
  
  command <- paste(design_path, d, t, delta, infile, outfile,  sep=" ")
  print(command)
  system(command, intern=TRUE)
  
}

generate_array("/home/michael/Desktop/function/generate", "/home/michael/Desktop/function/TWC.tsv", "/home/michael/Desktop/function/out_test.tsv")

current_directory <- getwd()
current_directory
