# Coded by Jose.Canevar
trevi_case_hosp_graph <- function(tstart, tend) {
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read data in:
  df <- pin_read(board, "jose.canevari@health.vic.gov.au/epi_trevi_case_hosp_age_sex")
  
  df %>% 
    filter(!is.na(DiagnosisDate) & DiagnosisDate < today()) %>% # investigate!!
    group_by(DiagnosisDate, Admitted) %>% 
    summarise(count = sum(Count)) %>% 
    pivot_wider(names_from = Admitted, values_from = count) %>% 
    rename('hospitalised' = '1', 'not_hospitalised' =  '0') %>% 
    ungroup() %>% 
    pad() %>% 
    mutate_at(c('hospitalised', 'not_hospitalised'), ~replace_na(.,0)) %>% 
    arrange(DiagnosisDate) %>% 
    mutate(
      h_roll_sum = roll_sum(hospitalised, 7*4, align = "right", fill = NA),
      nh_roll_sum = roll_sum(not_hospitalised, 7*4, align = "right", fill = NA)) %>% 
    subset(DiagnosisDate >= tstart, DiagnosisDate <= tend) %>% 
    bind_cols(
      round(epi.conf(cbind(.$h_roll_sum, .$nh_roll_sum), ctype = "prop.single"), digits = 5)
    ) %>% 
    
    ggplot() +
    geom_ribbon(aes(x = DiagnosisDate, ymax = upper*1000, ymin = lower*1000),
                fill=mae_blue, alpha = 0.3) +
    geom_line(aes(x = DiagnosisDate, y = est*1000), size = 1, colour =mae_blue ) +
    xlab('Diagnosis date') + ylab ('Hospitalisation risk (per 1,000 cases)') + 
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle ('Covid-19 rolling 4-week case hospitalisation risk') +
    theme_mae()  
  
}

