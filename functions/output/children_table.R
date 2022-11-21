# Children table 

children_table<- function ( ) {
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # Function
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
  
  # Read in Population data pin 
  popData2020 <- pin_read( board = board
                           ,name = 'craig.savage@health.vic.gov.au/pop2020')
  
  popAgeDataChildren <- popData2020 %>%
    mutate( ChildAgeGroups = cut(age_abs
                                 , breaks=c(0,4,11,18,Inf)
                                 , labels=c('0-4', '5-11','12-18','19+')
                                 , right=TRUE
                                 , include.lowest=TRUE
                                 , ordered=TRUE ) ) %>%
    group_by( ChildAgeGroups ) %>%
    summarise( Pop = sum( population ))
  
  # read cases pin 
  child_case_data <- pin_read(board,"qin.xu@health.vic.gov.au/epicases_linelist_read") %>%
    mutate( ChildAgeGroups=cut(AgeAtOnset
                               , breaks=c(0,4,11,18,Inf)
                               , labels=c('0-4', '5-11','12-18','19+')
                               , right=TRUE
                               , include.lowest=TRUE
                               , ordered=TRUE )) %>%
    select(-c (AgeAtOnset, AgeGroup)) %>%
    mutate(  thisWeek=between( DiagnosisDate
                               , today(tzone="Australia/Melbourne")-days(7)
                               , today(tzone="Australia/Melbourne") - days(1) )
             , lastWeek=between( DiagnosisDate
                                 , today(tzone="Australia/Melbourne")-days(14)
                                 , today(tzone="Australia/Melbourne")-days(8)))
  # child case table data
  child_case_table_data <- child_case_data %>%
    group_by( ChildAgeGroups ) %>%
    summarise( YTD = sum( n_case ),
               lastweek = sum(ifelse( lastWeek == "TRUE", n_case, NA), na.rm = T),
               thisweek = sum(ifelse( thisWeek == "TRUE", n_case, NA), na.rm = T)) %>%
    drop_na() %>%
    inner_join(popAgeDataChildren, by='ChildAgeGroups' ) %>%
    adorn_totals()  %>%
    mutate( percentIncrease=((thisweek-lastweek)/lastweek) %>%
              percent( accuracy=0.1) ,
            thisWeekCasePer100k=round(thisweek/(Pop/100e3),1),
            ytdPer100k=round(YTD/(Pop/100e3),0) ) %>%
    select( ChildAgeGroups,
            YTD,
            thisweek,
            lastweek, 
            percentIncrease, 
            thisWeekCasePer100k, 
            ytdPer100k )
  
  childFT <-   child_case_table_data %>%
    flextable() %>%
    formatFT()
  
  childFT
  
  return (childFT)
  
  
}
