#Coded by Jose.canevari
trevi_hosp_mortality_graph <- function(tstart, tend) {
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read data in:
  df <- pin_read(board, "jose.canevari@health.vic.gov.au/trevi_hosp_linelist")
  
  # monthly rolling sum of admissions/rolling sum of deaths for those admissions
  df %>% 
    mutate(Status = if_else(is.na(Status) | Status == 'Alive', 'Alive', 'Dead')) %>% 
    count(MinAdmissionDate, Status) %>% 
    pivot_wider(names_from = Status, values_from = n) %>% 
    pad() %>% mutate_at(c('Alive', 'Dead'), ~replace_na(.,0)) %>% 
    mutate(
      a_roll_sum = roll_sum(Alive, 28, align = "right", fill = NA),
      d_roll_sum = roll_sum(Dead, 28, align = "right", fill = NA)) %>% 
    subset(MinAdmissionDate >= tstart, MinAdmissionDate <= tend) %>% 
    subset(MinAdmissionDate < (today() - 6*7)) %>% 
    bind_cols(
      round(epi.conf(cbind(.$d_roll_sum, .$a_roll_sum), ctype = "prop.single"), digits = 3)
    ) %>% 
    
    ggplot() +
    geom_line(aes(x = MinAdmissionDate, y = est*100), size = 1.2) +
    geom_line(aes(x = MinAdmissionDate, y = upper*100), size= .5, colour = mae_grey) +
    geom_line(aes(x = MinAdmissionDate, y = lower*100), size= .5, colour = mae_grey) +
    xlab('Admission date') + ylab ('Mortality risk (per 100 hospitalised)') + 
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle ('Covid-19 rolling 4-week mortality in hospitalised cases') +  
    theme_mae()
  
}
