# ED Table by Age group 
# format function
ED_Table_Age<- function() {
  
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
  
  # ED TableData1 
  edTableData1 <- edData  %>% 
    group_by(AgeGroup) %>%
    summarise( ytd = sum(ytd))
  
  
  ed_lastweek<- edData %>%
    filter(adm_lastWeek == "TRUE") %>%
    group_by(AgeGroup) %>%
    summarise(lastWeekDailyAve=sum(ytd)/7) 
  
  ed_thisweek<- edData %>%
    filter(adm_thisWeek == "TRUE") %>%
    group_by(AgeGroup) %>%
    summarise(thisWeekDailyAve=sum(ytd)/7)
  
  edTableData1 <- edTableData1 %>% 
    left_join(ed_lastweek,by = "AgeGroup") %>%
    left_join(ed_thisweek, by = "AgeGroup") %>% 
    mutate_if(is.numeric, ~floor(.)) %>% 
    inner_join( popAgeData, by ='AgeGroup') %>%
    adorn_totals() %>%
    mutate( percentIncrease =((thisWeekDailyAve-lastWeekDailyAve)/lastWeekDailyAve) %>%
              percent( accuracy = 0.1 ),
            thisweekEDDailyAvePer100k = round(thisWeekDailyAve/(Pop/100e3),1), # this weekly average out of per 100K 
            ytdPer100k=round(ytd/(Pop/100e3),0)) %>%  # all ED cases average out of per 100K 
    select( AgeGroup, 
            ytd, 
            lastWeekDailyAve,
            thisWeekDailyAve,
            percentIncrease, 
            thisweekEDDailyAvePer100k,
            ytdPer100k)
  
  edFT1 <- edTableData1 %>%
    flextable() %>%
    formatFT
  
  return (edFT1)
}

