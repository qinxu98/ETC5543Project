# Case table function 

trevi_case_table <- function() {
  
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
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  #agelevels
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
  
  
  # read in case 
  df <- pin_read(board,"qin.xu@health.vic.gov.au/epicases_linelist_read")
  
  df<- df %>%
    mutate( ytd =DiagnosisDate >= ymd( 20220101 )
            , thisWeek=between( DiagnosisDate
                                , today(tzone="Australia/Melbourne")-days(7)
                                , today(tzone="Australia/Melbourne") - days(1) )
            , lastWeek=between( DiagnosisDate
                                , today(tzone="Australia/Melbourne")-days(14)
                                , today(tzone="Australia/Melbourne")-days(8) ))
  
  case_table<- df %>%
    group_by(AgeGroup) %>%
    summarise( ytd  = sum( n_case))
  
  case_table_thisweek<- df %>% 
    filter(thisWeek =="TRUE") %>%
    group_by(AgeGroup) %>%
    summarise(thisweek = sum( n_case)) 
  
  case_table_lastweek <- df %>%
    filter(lastWeek == "TRUE") %>%
    group_by(AgeGroup) %>%
    summarise( lastweek = sum(n_case))
  
  case_table<- case_table %>%
    left_join(case_table_thisweek, by = 'AgeGroup') %>%
    left_join(case_table_lastweek, by = 'AgeGroup')  %>% 
    drop_na() %>%
    inner_join( popAgeData, by='AgeGroup' ) %>%
    adorn_totals() %>%
    mutate(percentIncrease =((thisweek -  lastweek)/ lastweek) %>%
             percent( accuracy=0.1)
           , ThisWeekPer100k=round(thisweek /(Pop/100e3),1)
           , ytdPer100k=round(ytd/(Pop/100e3),0)) 
  
  case_table <- case_table %>% 
    select(c(AgeGroup,
             ytd,
             thisweek,
             lastweek,
             percentIncrease, 
             ThisWeekPer100k, 
             ytdPer100k))
  
  caseFT <- case_table %>%
    flextable() %>%
    formatFT()
  
  return(caseFT)
  
}


