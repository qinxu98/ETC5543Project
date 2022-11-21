# Coded by Jose Canevar
trevi_hosp_age_time_graph <- function(tstart, tend){
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read data in:
  df <- pin_read(board, "jose.canevari@health.vic.gov.au/trevi_hosp_linelist")
  
  four_week_median <- function(i_date, date, value){
    q50 <- quantile(value[date >= i_date-(4*7) & date <= i_date], 0.50) 
    q25 <- quantile(value[date >= i_date-(4*7) & date <= i_date], 0.25) 
    q75 <- quantile(value[date >= i_date-(4*7) & date <= i_date], 0.75)
    return(c(q50, q25, q75))
  }
  
  rval <- data.frame(cbind(
    (tstart-(4*7)):tend,
    do.call(rbind, lapply((tstart-(4*7)):tend, 
                          four_week_median, 
                          df$MinAdmissionDate, 
                          df$Age))))
  
  names(rval) <- c('date', 'q50', 'q25', 'q75')
  
  rval$date = as.Date(rval$date, origin = '1970-01-01')
  
  ggplot(rval %>% subset(rval$date >= tstart)) +
    geom_line(aes(x = date, y = q50), size = 1.2) +
    geom_line(aes(x = date, y = q75), size= .5, colour = mae_grey) +
    geom_line(aes(x = date, y = q25), size= .5, colour = mae_grey) +
    xlab('Admission date') + ylab ('Median age (Q1, Q3)') + 
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle ('Covid-19 rolling 4-week median age of hospitalised cases') +
    theme_mae()
  
}