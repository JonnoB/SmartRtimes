SaveManyRDS <- function(FileName, FolderPath, Chunksize=1e5, CallFunction = NULL){
  #Breaks up a large CSV into RDS files without crashing the computer
  #FileName : The name of the CSV to be broken up 
  #FolderPath :  The path and name of the Folder which will contain the smaller files
  #Chunksize :  The Maximum number of rows of the chunks which will make up the smaller files
  #CallFunction : If additional functions are to be called they are 
  #               included here as quoted or character strings
     dir.create(FolderPath)
  
  g <- function(z,pos){
    x<- z
    
    if (!is.null(CallFunction)){
      x <- eval(parse(text = CallFunction))
    }
    saveRDS(x, file.path(FolderPath, paste("RDS",pos, sep="_")))
  }
  read_csv_chunked(FileName, SideEffectChunkCallback$new(g), chunk_size = Chunksize)
  
}