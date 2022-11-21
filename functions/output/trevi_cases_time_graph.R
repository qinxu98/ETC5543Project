# cases over time plot
trevi_cases_time_graph <- function( daysbefore = 7 , daysafter = 0) {
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read data in:
  df <- pin_read(board,"qin.xu@health.vic.gov.au/epicases_linelist_read")
  
  case_plot <- df %>%
    group_by(DiagnosisDate) %>%
    summarise(n = sum (n_case))%>%
    mutate( weeklyAverage = slide_dbl( .x = n
                                       , .f=mean
                                       , .before= daysbefore
                                       , .after= daysafter )) %>% 
    ggplot() + 
    geom_point( aes( x=DiagnosisDate, 
                     y=n,
    ),size = 1.5) +
    geom_line( aes( x=DiagnosisDate, y=weeklyAverage ), size=0.8) +
    labs( x='Diagnosis Date', y='Cases', 
          title='Daily cases reported to DH, 2022',subtitle =  paste0('Line shows rolling average over ', daysbefore, ' days')) +
    scale_x_date(date_breaks = "3 months",
                 labels = date_format("%b"),
                 expand=c(0,0), limits = c(as.Date("2022-01-01"), 
                                           as.Date(today(tzone="Australia/Melbourne")))) +
    theme_mae()
  
  return(case_plot)
  
} 