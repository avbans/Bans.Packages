#THIS FUNCTION WILL DETERMINE THE WATER YEAR BASED ON THE DATE WITH THE ,
#ASSUMPTION THAT THE NEW YEAR STARTS ON OCT 1ST.
#USE IN CONJUNTION WITH DPLYR::MUTATE

get_wateryear<-function(date,yearstart){
  yearstart = 10
  x<-as.character(ifelse(lubridate::month(date) < yearstart,lubridate::year(date), lubridate::year(date)+1))
  x<-paste0((as.numeric(stringr::str_sub(x,1))-1),"-", stringr::str_sub(x, 1))
}
##############################################################################
#THIS FUNCTIONS PARSES STORM EVENTS FROM TIDY PRECIPTATION DATA
#THIS FUNCTION WORKS OFF ANY DATEFRAME (df), THAT HAS THE COLUMNS LABELED AS
#datetime,
#INTERVALS_PER_HR IS THE INTERVAL OF THE DATASET WHICH GETS MULTIPLIED BY
#INTEREVENT_PERIOD_HR
#AN INTEREVENT_PERIOD_HR in the form of hours
#STORM_SIZE_MINIMUM IS THE MINIMUM STORM SIZE YOU WILL BE LOOKING AT
#THIS FUNCTION WORKS FOR BOTH METRIC AND IMPERIAL AS LONG AS UNITS ARE "depth_in"
#OR "depth_mm"
#THIS FUNCTION USES MY GET_WATERYEAR() FUNCTION AS WELL

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

################################################################################
#THIS FUNCTION USES CROSS-JOINING TO IDENTIFIY WHICH DATES OCCUR UNDER SPECIFIC
#INTERVALS, FOR INSTANCE IDENTIFYING IF A DATE AND TIME OCCUR UNDER A SPECIFIC
#STORM EVENT
#THIS FUNCTION BUILDS ON TOP THE PARSE_STORMS() FUNCTION AND TAKES A TIDY DF that
#NEEDS datetime, AND DF CREATED BY PARSE_STORMS() FUNCTION

parse_dates<- function(df,df2){
  df2<-df2%>%
    select(storm_id,eventstart,eventend)

  crossing(df,df2)%>%
    filter(datetime >= eventstart,
           datetime < eventend)%>%
    select(-c(eventstart,eventend))
}
