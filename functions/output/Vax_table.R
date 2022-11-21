## Vaccination Table 

Vax_table <- function() {
  
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
  
  # Connect to board 
  board <- board_rsconnect()
  
  # Read in Population 
  popData2020 <- pin_read( board=board
                           ,name='craig.savage@health.vic.gov.au/pop2020')
  # PopVaxdata
  popVaxData <- popData2020 %>%
    filter( age_abs >= 5 ) %>%
    mutate( AgeGroup=cut(age_abs
                         , breaks=c(5,11,15,64,Inf)
                         , labels=c('05-11','12-15','16-64', '65+')
                         , right=TRUE
                         , include.lowest=TRUE
                         , ordered=TRUE ) ) %>%
    group_by( AgeGroup ) %>%
    summarise( Pop = sum( population ) )
  # PopVaxdata2
  popVaxData2 <- popData2020 %>%
    filter( age_abs >= 5 ) %>%
    mutate( VaxAgeGroup=cut(age_abs
                            , breaks = c(5,11,15,29,49,64,Inf)
                            , labels = c('05-11','12-15','16-29','30-49','50-64','65+')
                            , right = TRUE
                            , include.lowest = TRUE
                            , ordered = TRUE ) ) %>%
    group_by( VaxAgeGroup ) %>%
    summarise( Pop=sum( population ))
  
  # Read in vax data pin 
  VaxData <- pin_read(board,"qin.xu@health.vic.gov.au/epi_vax_data_read")  %>%
    mutate(VaxAgeGroup = as.character(VaxAgeGroup))
  
  VaxDataTotal2 <- VaxData %>%
    filter( DoseNumber <= 4 ) %>%
    group_by( VaxAgeGroup, DoseNumber ) %>%
    summarise( N = sum( n_vax)) %>%
    pivot_wider( names_from = DoseNumber, values_from = N, names_prefix = 'Dose_' ) %>%
    inner_join( popVaxData2, by = "VaxAgeGroup" ) %>%
    mutate( across( Dose_1:Dose_4, ~./Pop ))  %>%
    select( -Pop )
  
  VaxDataMonth2 <- VaxData %>%
    filter( DoseNumber <= 4 ) %>%
    filter( EncounterDate >= today(tzone="Australia/Melbourne") - days(30) ) %>%
    group_by( VaxAgeGroup, DoseNumber ) %>%
    summarise( N = sum( n_vax)) %>%
    pivot_wider( names_from = DoseNumber, values_from = N, names_prefix = 'Dose_' ) %>%
    inner_join( popVaxData2, by="VaxAgeGroup") %>%
    mutate( across( Dose_1:Dose_4, ~./Pop ) ) %>%
    select( -Pop )
  
  makeVaxCell <- function( doseTotal, doseMonth ){
    vaxCell <- paste0(percent( doseTotal, accuracy=1 )
                      , ' (+'
                      , percent( doseMonth
                                 , accuracy=0.01 )
                      , ' in past 30 days)' ) 
    return( vaxCell )
  }
  
  VaxTable2 <- VaxDataTotal2 %>%
    inner_join( VaxDataMonth2, by='VaxAgeGroup'
                , suffix=c('.total', '.month') ) %>%
    mutate( Dose_1=makeVaxCell( Dose_1.total, Dose_1.month ) 
            , Dose_2=makeVaxCell( Dose_2.total, Dose_2.month ) 
            , Dose_3=makeVaxCell( Dose_3.total, Dose_3.month ) 
            , Dose_4=makeVaxCell( Dose_4.total, Dose_4.month ) ) %>%
    select( VaxAgeGroup, Dose_1:Dose_4 )
  
  VaxFT <- VaxTable2 %>%
    flextable() %>%
    formatFT()
  
  
  return(VaxFT) }


