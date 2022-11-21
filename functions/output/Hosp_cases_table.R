# Hospitalization cases table 

Hosp_cases_table <- function() {
  
  
  # format function
  formatFT <- function( inputFlextable ){
    outputFlextable <- inputFlextable %>%
      theme_zebra( odd_body='grey95' ) %>%
      bg( bg='#091540', part='header' ) %>%
      color( color='white', part='header' ) %>%
      font( fontname='Arial', part='all' ) %>%
      align( align="center", part="all") %>%
      set_table_properties( layout="autofit")
    return( outputFlextable )
  }
  
  set_flextable_defaults( digits=2 )
  
  # constants
  ageLevels <- factor( c('0-18', '19-39', '40-64', '65+'), ordered=TRUE )
  
  # connect to baord 
  board <- board_rsconnect()
  
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
  
  
  # read pin that has already been previously created 
  vicnissData <- pin_read( board=board,
                           name='dennis.wollersheim@health.vic.gov.au/vicniss_daily_location_report')
  
  # Create dataset that shows  hospitlisation cases of COVID-19 starting from 2022 and hospitalization cases for the lastest week and latest two weeks
  hospData <- vicnissData %>%
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
  
  
  hospTableData <- hospData %>%
    filter( !is.na( AgeGroup ) ) %>%
    filter( DailyLogDate >= ymd( 20220101 ) ) %>%
    group_by( AgeGroup ) %>%
    summarise( ytd=n_distinct( Patient_URNo )
               , lastWeekDailyAve=round(sum( lastWeek )/7, 1 ) # Daily average
               , thisWeekDailyAve=round(sum( thisWeek)/7 , 1 ) ) %>%
    inner_join( popAgeData, by='AgeGroup' ) %>%
    adorn_totals() %>%
    mutate( percentIncrease=((thisWeekDailyAve-lastWeekDailyAve)/lastWeekDailyAve) %>%
              percent( accuracy=0.1 )
            , thisweekHospPer100k=round(thisWeekDailyAve/(Pop/100e3),1)
            , ytdPer100k=round(ytd/(Pop/100e3),1) )
  
  hospTableData<-   hospTableData %>%
    select(AgeGroup, 
           ytd, 
           lastWeekDailyAve, 
           thisWeekDailyAve,
           percentIncrease, 
           thisweekHospPer100k, 
           ytdPer100k) 
  
  hospFT <- hospTableData %>%
    flextable() %>%
    formatFT()
  
  return(hospFT)
  
}



