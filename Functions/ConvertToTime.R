ConvertToTime <- function(x, TimeZoneCol = "TimeZoneCol", ){
  #This function deals with multiple time zones in the data.
  names(x)[grep(TimeZoneCol, names(x))] <- "TimeZoneCol"
  
  x <- x %>%
    split(.$TimeZoneCol) %>%
    map_df(~FilterTime(., "Date and Time of capture", unique(.x$TimeZoneCol)))
  
  names(x)[grep("TimeZoneCol", names(x))] <- TimeZoneCol 
  
  return(x)
}
