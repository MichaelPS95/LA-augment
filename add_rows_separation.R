
add_row <- function(LA=NULL, separation_data=NULL, rows_to_add=1, t=2){
  if(is.null(separation_data) && is.null(LA)){
    stop("There are no files provided.")
  }
  else if(is.null(LA)){
    stop("No design specified.")
  }
  else if(is.null(separation_data)){
    stop("No parameters specified.")
  }
  
  else{
    command <- paste("python3 add_row.py",LA, separation_data, rows_to_add, t,sep=" ")
    print(command)
    system(command, intern=TRUE)
  }
}

add_row("/home/michael/Desktop/function/Sample-Input/Colbourn1.tsv","/home/michael/Desktop/function/t_way.txt")

add_row("/home/michael/Desktop/function/Sample-Input/Colbourn1.tsv","/home/michael/Desktop/function/t_way.txt", rows_to_add=2)

add_row("/home/michael/Desktop/function/Sample-Input/Colbourn1.tsv")
