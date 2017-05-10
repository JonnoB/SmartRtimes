JustTheseRows <- function(File, Start, Stop, Columns=1, Header = TRUE ){
  #Returns the named rows from inside a file used for getting a known sample from a large csv
  #
  #Inputs
  #File : The file path of the target file
  #Start : The Start row to begin data extraction
  #Stop : The Stop row where extraction ends
  #Columns :  the number of columns inside the file. This is necessary to make a coherent Dataframe
  #Header : If the file has a header row this will be imported and used as the names of the columns
  
  Skipped <- Start
  
  if(Header==TRUE){
    Start<- Start+1
    Stop = Stop+1
    Skipped = Start+1
  }  
  
  Out <- scan(File, what="raw",sep=',', skip = Skipped-1,
              nlines = Stop-Start+1,
              quiet = TRUE) %>% 
    matrix(., ncol= Columns, byrow = TRUE) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  if(Header==TRUE){
    names(Out)<- scan(File, what="raw",sep=',', skip = 0, nlines = 1, quiet = TRUE)
  }
  return(Out)
  
}