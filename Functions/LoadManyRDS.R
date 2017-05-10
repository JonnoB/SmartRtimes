LoadManyRDS <- function(FolderPath,  CallFunction = NULL){
 #Loads a string of RDS files created by the Save many RDS Function
  #FolderPath : The path of the Folder containing the files
  #CallFunction : If additional functions are to be called they are 
  #               included here as quoted or character strings
  
  g <- function(z){
    print(z)
    x<- readRDS(file.path(FolderPath, z))
    
    if (!is.null(CallFunction)){
      x <- eval(parse(text = CallFunction))
    }
   return(x) 
  }
  
  
   list.files(FolderPath)[list.files(FolderPath) %>% 
                            sub("RDS_", "",.) %>% 
                            as.numeric(.) %>% order()]  %>% 
     map_df(~g(.x))
}