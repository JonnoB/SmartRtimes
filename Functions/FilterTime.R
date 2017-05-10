FilterTime <-function(df, Date.Time, TimeZone, Start=0, End=24){
  #This function filters the Hours to include only the periods you are interesteding
  #Requires the timezone of the data being collected.
  
  #Inputs
  #df : A data frame of at least 1 column where that column is of class datetime in the format dmy_hms
  #DateTime :  The name of the column that contains the date time
  #TimeZone : The time zone that the data is being collected in, this is required in case the
  #           clocks change during the data collection period.
  #Start : The Time of day the period of interest starts from
  #End : The time of day the period of interest ends in
    
  #create vector of rows to use so that it doesn't crash
  
  names(df)[grep(Date.Time, names(df))] <- "DateTime"
  
  df<- df %>%
    mutate(DateTime = dmy_hms(DateTime, tz=TimeZone), 
           hour = hour(DateTime)) %>%
    filter(hour >= Start, hour<=End)
  
  names(df)[grep("DateTime", names(df))] <- Date.Time 
  
  return(df)
}