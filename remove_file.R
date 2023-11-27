
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

remove_file("remove.txt")
