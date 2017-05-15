ConvertToTime <- function(x, Date.Time, TimeZoneCol = "TimeZoneCol" ){
  #This function deals with multiple time zones in the data.
  NamesReplace <- c(grep(TimeZoneCol, names(x)), grep(Date.Time, names(x)))
  names(x)[NamesReplace] <- c("TimeZoneCol", "DateTime")

  x <- x %>%
    split(.$TimeZoneCol) %>%
    map_df(~ .x %>%
             mutate(DateTime = dmy_hms(DateTime, 
                                       tz=unique(.x$TimeZoneCol))
             )
    )
  
  names(x)[NamesReplace] <- c(TimeZoneCol, Date.Time) 
  
  return(x)
}
