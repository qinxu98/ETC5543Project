
test_positivity_rate_graph <- function(tstart,tend) {
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read data in:
  df <- pin_read(board,"rana.abouelenein@health.vic.gov.au/epi_rep_pcr_results")
  
  df %>% 
    pivot_wider(names_from = Result, values_from = n, values_fill = 0) %>% 
    mutate(TotalTests=Negative+Positive) %>%
    ungroup() %>%
    pad() %>%
    arrange(CollectionDate) %>%
    mutate(rolling_avg = rollmean(TotalTests, k=28, fill=NA, align='right')) %>%
    mutate(
      p_roll_sum = roll_sum(Positive, 7*4, align = "right", fill = NA),
      n_roll_sum = roll_sum(Negative, 7*4, align = "right", fill = NA)) %>% 
    subset(CollectionDate >= tstart, CollectionDate <= tend) %>% 
    bind_cols(
      round(epi.conf(cbind(.$p_roll_sum, .$n_roll_sum), ctype = "prop.single"), digits = 5)
    ) %>% 
    
    ggplot(aes(x=CollectionDate)) +
    geom_ribbon(aes(ymin=0, ymax = rolling_avg/1000), color=mae_grey, alpha = 0.2) + 
    geom_line(aes(y=est*100, colour="Test positivity rate")) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y",
                 name =  'Specimen collection date') +
    scale_y_continuous(name = "Percentage positive",
                       sec.axis = sec_axis(~.*1000, name="Number of tests")
    ) + 
    theme_mae() +
    theme(legend.position="top", 
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.title=element_blank()) +
    ggtitle("Rolling 4-week PCR positivity rate")
}

