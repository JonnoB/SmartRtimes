LoadManyRDS <- function(FolderPath,  CallFunction = NULL){
 #Loads a string of RDS files created by the Save many RDS Function
  #FolderPath : The path of the Folder containing the files
  #CallFunction : If additional functions are to be called they are 
  #               included here as quoted or character strings
  
  g <- function(z){
    print(z)
    x<- read.csv(file.path(FolderPath, z), 
                 colClasses = "character",
                 check.names = FALSE)

    if (!is.null(CallFunction)){
      x <- eval(CallFunction) 
    }
   return(x) 
  }
  
  LoadOrder <- list.files(FolderPath) %>% 
    sub("CSV_", "",.) %>%
    sub("_.csv", "",.) %>%
    as.numeric(.) %>% order()
  
  
    # LoadOrder <- list.files(FolderPath) %>% 
  #   sub("RDS_", "",.) %>% 
  #   as.numeric(.) %>% order()

   list.files(FolderPath)[LoadOrder]  %>% 
     map_df(~g(.x))
}