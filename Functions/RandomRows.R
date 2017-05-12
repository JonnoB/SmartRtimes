RandomRows <- function(File, MaxRow, ReturnedRows, TotalColumns, Header = TRUE){
  #Similar to the function JustTheseRows. This function extractions a random sample of rows from a file
  #
  #Inputs
  #File : The file path of the target file
  #MaxRow : The Maximum Row you wish to sample upto
  #ReturnedRows : The total number of rows sampled
  #Columns :  the number of columns inside the file. This is necessary to make a coherent Dataframe
  #Header : If the file has a header row this will be imported and used as the names of the columns
  
  SkipHead<-0
  if(Header==TRUE){
   SkipHead = 1
  }  
  
      Out <- sample(1:MaxRow,  ReturnedRows, replace = TRUE) %>%
        map(~scan(File, 
              what="raw",sep=',', 
              skip = .x + SkipHead,
              nlines = 1,
              quiet = TRUE) )%>%  
    unlist %>% 
    matrix(., ncol= TotalColumns, byrow = TRUE) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  
  if(Header==TRUE){
    names(Out)<- scan(File, what="raw",sep=',', skip = 0, nlines = 1, quiet = TRUE)
  }
  return(Out)
}
