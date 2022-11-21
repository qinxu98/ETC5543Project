# Death Table 
Death_table <- function(){
  
  ## Constants ----
  ageLevels <- factor( c('0-18', '19-39', '40-64', '65+'), ordered=TRUE )
  
  nAgeGroup <- length( levels( ageLevels ) )
  
  emptyDeathTable <- tibble( AgeGroup=ageLevels
                             , YTD=rep( 0, nAgeGroup )
                             , last2Week=rep( 0, nAgeGroup )
                             , lastWeek=rep( 0, nAgeGroup )
                             , percentIncrease=rep( NA_character_, nAgeGroup ))
  
  ## Functions -----
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
  
  # read in deathcase pin
  df <- pin_read(board,"qin.xu@health.vic.gov.au/epideath_case_read") %>%
    mutate(last2Week=between( DateOfDeath
                              , today(tzone = "Australia/Melbourne") - days(21)
                              , today(tzone = "Australia/Melbourne") - days(15)),
           lastWeek=between( DateOfDeath
                             , today(tzone = "Australia/Melbourne") - days(14)
                             , today(tzone = "Australia/Melbourne") - days(8))
    )
  
  # Deathdata
  Deathdata<- df %>%
    group_by( AgeGroup ) %>%
    summarise( YTD = sum( n_death))
  
  death_last2week <-  df%>% 
    filter( last2Week =="TRUE") %>%
    group_by(AgeGroup) %>%
    summarise( last2Week = sum(n_death))
  
  death_lastweek <-  df%>% 
    filter( lastWeek =="TRUE") %>%
    group_by(AgeGroup) %>%
    summarise( lastWeek = sum(n_death))
  
  Deathdata <-  Deathdata %>%
    left_join(death_last2week, by = "AgeGroup") %>%
    left_join(death_lastweek, by = "AgeGroup") %>%
    replace(is.na(.), 0) %>%
    adorn_totals() %>%
    mutate( percentIncrease = ((lastWeek-last2Week)/last2Week) %>%
              scales::percent( accuracy=0.1 ))
  
  
  # Fill in the deaths table
  finalDeathTable <- Deathdata %>%
    bind_rows( anti_join( emptyDeathTable, Deathdata
                          , by='AgeGroup' ) ) %>%
    arrange( AgeGroup ) 
  
  deathFT <- finalDeathTable %>%
    flextable() %>%
    formatFT() 
  
  return(deathFT) }



