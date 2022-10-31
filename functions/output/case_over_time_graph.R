# cases over time plot

cases_over_time_graph <- function(days_before = 7) {
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read data in:
  df <- pin_read(board,"qin.xu@health.vic.gov.au/epicases_linelist_read")
  
  case_plot <- df %>% 
    filter( DiagnosisDate >= ymd( 20220101 )
            , DiagnosisDate < today(tzone="Australia/Melbourne")) %>%
    count(DiagnosisDate ) %>%
    mutate( weeklyAverage = slide_dbl( .x = n,
                                       .f = mean,
                                       .before = days_before,
                                       .after = 0L  )) %>% 
    ggplot() + 
    geom_point( aes( x=DiagnosisDate, 
                     y=n,
    ),size = 1.5) +
    geom_line( aes( x=DiagnosisDate, y=weeklyAverage ), size=0.8) +
    labs( x='Diagnosis Date', y='Cases', 
          title='Daily cases reported to DH ', subtitle= paste0('Line shows rolling average over ', x, ' days')) +
    scale_x_date(date_breaks = "3 months",
                 labels = date_format("%b"),
                 expand=c(0,0), limits = c(as.Date("2022-01-01"), 
                                           as.Date(today(tzone="Australia/Melbourne")))) +
    theme_mae()
  
  return(case_plot)
  
} 

