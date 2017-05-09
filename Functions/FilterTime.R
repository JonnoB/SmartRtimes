#This function filters the Hours to include only the periods you are interesteding
#Requires the timezone of the data being collected.

#The operation is chunked as the conversion of the date time column into one that R 
#recognises as date time can cause the program to crash if the you try it on a dataframe.

#Inputs
#df : A data frame of at least 1 column where that column is of class datetime in the format dmy_hms
#DateTime :  The name of the column that contains the date time
#TimeZone : The time zone that the data is being collected in, this is required in case the
#           clocks change during the data collection period.
#Start : The Time of day the period of interest starts from
#End : The time of day the period of interest ends in
#segments : Breaks the function into sections to prevent running out of memory

FilterTime <-function(df, Date.Time, TimeZone, Start=16, End=21, segments=1000){

  #create vector of rows to use so that it doesn't crash
  rowIDs <-rep(1:ceiling(nrow(df)/segments), each = segments)[1:nrow(df)]
  
  names(df)[grep(Date.Time, names(df))] <- "DateTime"
  
  df<-lapply(unique(rowIDs), function(n){
    print(n)

      df %>% filter(rowIDs == n) %>%
      mutate(DateTime = dmy_hms(DateTime, tz=TimeZone), 
             hour = hour(DateTime)) %>%
      filter(hour >= Start, hour<=End)

  }
  ) %>% bind_rows(.)
  
  names(df)[grep("DateTime", names(df))] <- Date.Time 
  
  return(df)
}
