# Hospitlisation plot 


Hosp_plot<- function( daysbefore = 7, daysafter = 0 ) {
  
  
  # connect to baord 
  board <- board_rsconnect()
  
  # read pin that has already been previously created 
  vicnissData <- pin_read( board=board
                           , name='dennis.wollersheim@health.vic.gov.au/vicniss_daily_location_report')
  
  # constants
  ageLevels <- factor( c('0-18', '19-39', '40-64', '65+'), ordered=TRUE )
  
  nAgeGroup <- length( levels( ageLevels ) )
  
  # popAgeData
  popData2020 <- pin_read( board=board
                           ,name='craig.savage@health.vic.gov.au/pop2020')
  popAgeData <- popData2020 %>%
    mutate( AgeGroup=cut( age_abs
                          , breaks=c(0,18,39,64,Inf)
                          , labels=ageLevels
                          , right=TRUE
                          , include.lowest=TRUE
                          , ordered_result=TRUE )
    ) %>%
    group_by( AgeGroup ) %>%
    summarise( Pop=sum( population ))
  
  # Create data set that shows  hospitalization cases of COVID-19 starting from 2022 and hospitalization cases for the lastest week and latest two weeks
  hospdata <- vicnissData %>%
    mutate( DoB = PatientDOB
            , Age=floor( as.numeric(DailyLogDate-DoB)/365.25 ) 
            , AgeGroup=cut( Age
                            , c(0, 18, 39, 64, Inf)
                            , ageLevels
                            , right=TRUE
                            , include.lowest=TRUE
                            , ordered_result=TRUE )
            , ytd=DailyLogDate >= ymd( 20220101 )
            , lastWeek=between( DailyLogDate
                                , today(tzone="Australia/Melbourne") - days(17)
                                , today(tzone="Australia/Melbourne") - days(11))
            , thisWeek=between( DailyLogDate
                                , today(tzone="Australia/Melbourne") - days(10)
                                , today(tzone="Australia/Melbourne")-days(4) )
    ) %>%
    filter( DailyLogDate <= today(tzone="Australia/Melbourne")-days(4))
  
  
  
  hospPlotData <- hospdata %>%
    filter( DailyLogDate >= ymd( 20220101 ) ) %>%
    group_by( DailyLogDate ) %>%
    summarise( N = n_distinct( Patient_URNo ) ) %>%
    mutate( Weekly=slide_dbl( N
                              , .f=mean
                              , .before= daysbefore
                              , .after= daysafter ) )
  
  lastUpdated <- max(hospdata$DailyLogDate)
  
  plotheight <- max(hospPlotData$N)
  
  hospPlot <- 
    ggplot( hospPlotData, aes( x=DailyLogDate) ) +
    geom_point(aes( y=N ) ) +
    geom_line( aes( y=Weekly ), size=1.2) +
    ylim(0, plotheight) +
    labs( x='Date', y='People in hospital',
          title='Total daily COVID-19 hospitalisations',
          subtitle='2022') +
    theme_mae()
  
  
  return(hospPlot) }

Hosp_plot()


