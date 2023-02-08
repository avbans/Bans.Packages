#' Determine Water Year
#'
#'This function determines the water year based on a date given.
#' @param date The date in the form of date or datetime.
#' @param yearstart The month your water year starts on, defaults as Oct 1st (10).
#'
#' @return  The water year the date belongs to.

#'
#' @examples get_wateryear(lubridate::today(),9)
#' @examples df<- dplyr::mutate(df, wateryear = get_wateryear(datetime))
#' @export
get_wateryear<-function(date,yearstart){
  yearstart = 10
  x<-as.character(ifelse(lubridate::month(date) < yearstart,lubridate::year(date), lubridate::year(date)+1))
  x<-paste0((as.numeric(stringr::str_sub(x,1))-1),"-", stringr::str_sub(x, 1))
}

#' Parse storm events from precipitation data
#'
#' This function parses storm events from tidy precipitation data.
#` This function also uses the get_wateryear() function from this package
#' @param df This is a dataframe that contains datetime columns and a rainfall
#'  columns labeled as "depth_in" or "depth_mm".
#' @param intervals_per_hr How many recordings per hour. 1 means hourly,
#`  12 = 5 minute intervals, 60 = 1 minute intervals. Default is hourly.
#' @param interevent_period_hr The dry period needed in between storm events, in hours
#' @param storm_size_minimum What is the minimum storm size being evaluated?
#`  can be in either in or mm.
#'
#' @return A new data frame with storm_id, water year,eventstart, eventend,
#'  total_depth, and duration_hr
#'
#' @examples df<- parse_storms (df = df,intervals_per_hr = 1, interevent_period_hr = 12,storm_size_minimum = 0.02)
#' @export
parse_storms<-function(df,
                       intervals_per_hr,
                       interevent_period_hr,
                       storm_size_minimum){
  intervals_per_hr <- 1
  interevent_period_hr <-intervals_per_hr *interevent_period_hr

  df_units <-c(depth = "depth_in", depth = "depth_mm")
  df2<-rename(df,any_of(df_units))

  storm_summary<-df2%>%
    arrange(datetime)%>%
    mutate(log =row_number())%>%
    mutate(rainflag = ifelse(depth > 0,"rain","no rain"))%>%
    mutate(rainflag_length = sequence(rle(rainflag)$lengths))%>%
    mutate(eventflag = ifelse(
      rainflag == "rain",
      "event",
      ifelse(
        rainflag == "no rain" & rainflag_length < interevent_period_hr,
        "event",
        "no event"

      )
    )
    )%>%
    filter(eventflag == "event")%>%
    mutate(storm_id= cumsum(c(1, abs(log[-length(log)]-log[-1])>1)))

  storm_summary<-storm_summary%>%
    group_by(storm_id)%>%
    summarize(
      eventstart = first(datetime),
      eventend = last(datetime),
      total_depth= sum(depth))%>%
    mutate(duration_hr = as.numeric(difftime(eventend,
                                             eventstart,
                                             units='h')))%>%
    mutate(wateryear=get_wateryear(eventend))%>%
    filter(total_depth > storm_size_minimum) %>%
    mutate(storm_id = row_number())%>%
    select(storm_id,
           wateryear,
           eventstart,
           eventend,
           duration_hr,
           total_depth)


  if("depth_in" %in% colnames(df)){
    storm_summary<- storm_summary%>%
      rename("total_depth_in" = "total_depth")
  } else if("depth_mm" %in% colnames(df)){
    storm_summary<- storm_summary%>%
      rename("total_depth_mm" = "total_depth")
  }
}

#' Identify what storms an observation belongs to
#'
#' This function determines what storm event an observation is associated with.
#' This can be used a wide range of circumstances (sampling, rain, flow, etc.).
#' WARNING: This function does not work on extremely large datasets for now.
#' @param df The dataframe to manipulate. You need datetime at minimum.
#' @param df2 The dataframe output from parse_storms() function.
#'
#' @return Returns updated dataframe where each observation has an associated
#`  storm id. `
#'
#' @examples df<- parse(df, df2)
#' @export
parse_dates<- function(df,df2){
  df2<-df2%>%
    select(storm_id,eventstart,eventend)

  crossing(df,df2)%>%
    filter(datetime >= eventstart,
           datetime < eventend)%>%
    select(-c(eventstart,eventend))
}

