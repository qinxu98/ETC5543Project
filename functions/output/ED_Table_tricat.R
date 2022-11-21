#ED cases grouped by triage category 

ED_Table_tricat<- function() {
  
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
  
  nAgeGroup <- length( levels( ageLevels ) )
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
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
  
  # read in ED_cases_read
  edData <-  pin_read( board=board
                       ,name='qin.xu@health.vic.gov.au/epied_cases_read')
  
  edTableData2 <- edData %>%
    filter( !is.na( tricat ) & tricat < 6 ) %>%
    mutate( tricat = ifelse( tricat == 1, "1 - Resuscitation", tricat)) %>%
    mutate( tricat = ifelse( tricat == 2, "2 - Emergency", tricat)) %>%
    mutate( tricat = ifelse( tricat == 3, "3 - Urgent", tricat)) %>%
    mutate( tricat = ifelse( tricat == 4, "4 - Semi urgent", tricat)) %>%
    mutate( tricat = ifelse( tricat == 5, "5 - Non urgent", tricat)) 
  
  edTableData_ytd<-    edTableData2 %>%
    group_by( tricat ) %>%
    summarise( ytd = sum ( ytd))
  
  ed_lastweek1<- edTableData2 %>%
    filter(adm_lastWeek == "TRUE") %>%
    group_by( tricat) %>%
    summarise(lastWeekDailyAve=sum(ytd)/7) 
  
  ed_thisweek1<- edTableData2 %>%
    filter(adm_thisWeek == "TRUE") %>%
    group_by(tricat) %>%
    summarise(thisWeekDailyAve=sum(ytd)/7)
  
  edTableData2 <- edTableData_ytd %>% 
    left_join(ed_lastweek1,by = "tricat") %>%
    left_join(ed_thisweek1, by = "tricat") %>% 
    mutate_if(is.numeric, ~floor(.)) %>% 
    adorn_totals() %>%
    mutate( percentIncrease=((thisWeekDailyAve-lastWeekDailyAve)/lastWeekDailyAve) %>%
              percent( accuracy=0.1 ) ) %>%
    select( tricat, ytd, thisWeekDailyAve, lastWeekDailyAve, percentIncrease )
  
  edFT2 <- edTableData2 %>%
    flextable() %>%
    formatFT()
  
  return(edFT2) 
}


