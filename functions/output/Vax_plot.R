# Vax Plot 

Vax_plot<- function() {
  
  # Connect to board 
  board <- board_rsconnect()
  
  # Read in Population 
  popData2020 <- pin_read( board=board
                           ,name='craig.savage@health.vic.gov.au/pop2020')
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
  
  
  # Reading in Pins 
  VaxData <- pin_read(board,"qin.xu@health.vic.gov.au/epi_vax_data_read")  %>%
    mutate(VaxAgeGroup = as.character(VaxAgeGroup),
           EncounterDate = as.Date( EncounterDate )) %>%
    filter( DoseNumber <= 4 ) 
  
  VaxPlotData2 <- VaxData %>%
    group_by( VaxAgeGroup, 
              DoseNumber,
              EncounterDate ) %>%
    summarise( nDose = sum(n_vax),
               .groups='drop_last' ) %>%
    arrange( EncounterDate, .by_group = TRUE ) %>%
    mutate( cumDose = cumsum( nDose ) ) %>%
    inner_join( popVaxData2, by='VaxAgeGroup' ) %>%
    mutate( vaxRate=cumDose/Pop ) %>%
    mutate( DoseNumber = paste0("Dose ", DoseNumber)) 
  
  months <- VaxPlotData2 %>%
    mutate(EncDateMonth = floor_date(EncounterDate, "month")) %>% 
    arrange(desc(EncDateMonth)) %>%
    group_by(EncDateMonth) %>% 
    summarise(n = n()) %>% 
    arrange(desc(EncDateMonth)) %>% 
    mutate(rn=1:n())
  
  thismonth <- months %>% filter(rn==1)
  
  thismonth <- thismonth$EncDateMonth
  
  sixmonths <- months %>% filter(rn == 6)
  
  sixmonths <- sixmonths$EncDateMonth
  
  twelvemonths <- months %>% filter(rn == 12)
  
  twelvemonths <- twelvemonths$EncDateMonth
  
  eighteenmonths <- months %>% filter(rn==18)
  
  eighteenmonths <- eighteenmonths$EncDateMonth
  
  # twentyfourmonths <- months %>% filter(rn==24)
  
  datebreaks <- c(as.Date(eighteenmonths), as.Date(twelvemonths), as.Date(sixmonths), as.Date(thismonth))
  
  # VaxPlot
  VaxPlot2 <- ggplot( VaxPlotData2 
                      , aes( x = EncounterDate,
                             y = vaxRate ) ) +
    geom_line( aes( colour = VaxAgeGroup ),
               size = 1.2 ) +
    facet_wrap( ~DoseNumber,
                nrow=1 ) +
    theme_mae() +
    theme( legend.position ='bottom' ) +
    theme( axis.text.x = element_text(hjust = 0.8, angle=90) ) +
    scale_x_date( breaks = datebreaks
                  , labels = date_format("%b %Y")
                  , limits = c(as.Date("2021-02-01"), today(tzone="Australia/Melbourne")-1)
                  , expand = c(0,0) ) +
    theme( strip.text = element_text(hjust=0.5, family="VIC")
           , strip.background = element_rect( fill="grey90" )) +
    labs( x = "Date", y = "Vaccination rate",
          title = "Vaccination coverage" 
          , subtitle = "By dose number and age group")
  
  
  return(VaxPlot2)
}
