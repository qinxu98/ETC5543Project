# ED plot 

ED_plot <- function( daysbefore = 7, daysafter = 0 ) {
  
  # connect to the board 
  board <- pins::board_register_rsconnect()
  
  # read in pin vemdData
  
  vemdData <- 
    pin_read( board=board
              , name='dennis.wollersheim@health.vic.gov.au/vemd_linked' ) %>%
    filter(grepl('days',covid_category,ignore.case = TRUE))  %>% # filter for all covid cases  
    mutate(across( contains('date') | ends_with( 'dat' )
                   , ~as.Date(.)))
  
  # Emergency department data 
  edplotdata <- vemdData %>%
    mutate(  diagnosedPost = CalculatedOnsetDate > admdate,
             diagnosedPostv2 = DiagnosisDate >= admdate,
             diagTiming = if_else(DiagnosisDate > admdate, "Post",
                                  if_else(DiagnosisDate == admdate, "Same", "Prior"))) %>%
    filter( admdate >= ymd( 20220101)) %>%
    group_by(admdate) %>%
    summarise( N = n(),
               nPost = sum (diagTiming == "Post", na.rm =T), # have to remove NA dx dates (not many)
               nSame = sum(diagTiming == "Same", na.rm = T),
               nPrior = sum(diagTiming == "Prior", na.rm = T)) %>% 
    mutate( weeklyN =slide_dbl( N, mean, .before = daysbefore, .after = daysafter),
            weeklyPost=slide_dbl( nPost, mean, .before = daysbefore, .after = daysafter ),
            weeklySame=slide_dbl( nSame, mean, .before = daysbefore, .after = daysafter),
            weeklyPrior=slide_dbl( nPrior, mean, .before = daysbefore, .after =daysafter))
  
  edplotdata<-  edplotdata  %>%
    mutate( propDiagPost = nPost / N * 100,
            propDiagSame = nSame / N * 100,
            propDiagPostAndSame = (nPost + nSame) / N * 100 ) %>%
    mutate( propDiagPostWeekly = slide_dbl( propDiagPost, mean, .before = daysbefore, .after = daysafter ),
            propDiagSameWeekly = slide_dbl( propDiagSame, mean, .before= daysbefore, .after = daysafter),
            propDiagPostAndSameWeekly = slide_dbl( propDiagPostAndSame, mean, .before =  daysbefore, .after = daysafter))
  
  # pivot longer so we have npost, nsame, n prior into var and values into "n"
  edPlotDataLong <- edplotdata %>%
    pivot_longer(cols = c(weeklyPost, 
                          weeklySame, weeklyPrior),
                 names_to = "var",
                 values_to = "n")
  
  edPlotDataLongFiltered <- edplotdata %>% 
    arrange(desc(admdate)) %>%
    mutate(rn = 1:n()) %>%
    filter(rn != 1) %>%
    select(-rn) %>%
    arrange(admdate)
  
  y.lim.pri <- c(0, max(edPlotDataLong$N))
  y.lim.sec <- c(0, max(edPlotDataLong$propDiagPostAndSameWeekly))
  b <- diff(y.lim.pri)/diff(y.lim.sec)
  
  edPlotDataLong <- edPlotDataLong %>%  
    # mutate( var = factor(var, levels = c("nPost", "nSame", "nPrior")) ) # factorise levels
    mutate( var = factor(var, 
                         levels = c("weeklyPost", 
                                    "weeklySame", 
                                    "weeklyPrior"))) # factorise levels
  
  edPlot <- edPlotDataLong %>% 
    # filter(var %in% c("nPost", "nSame", "nPrior")) %>%
    filter(var %in% c("weeklyPost", 
                      "weeklySame", 
                      "weeklyPrior")) %>%
    ggplot(aes( x = admdate, y = n)) + 
    geom_bar( aes( x = admdate, 
                   y = n, 
                   fill = var ), stat="identity" )  + 
    # geom_line( aes( x = admdate, y=weeklyPost, colour='After' ) ) +
    # geom_line( aes( x = admdate, y=weeklyN ), colour='black' ) +
    geom_line( aes( x = admdate,
                    y = propDiagPostAndSameWeekly * b ),
               data = edPlotDataLong,
               linetype=2,
               size= 0.8) +
    geom_line( aes( x = admdate, y = propDiagPostWeekly * b ),
               data = edPlotDataLong,
               linetype=2, 
               size=0.8, 
               colour="hotpink") +
    geom_line( aes( x = admdate, y = propDiagSameWeekly * b ), 
               data = edPlotDataLong, 
               linetype=2, 
               size=0.8, 
               colour="orange") +
    scale_fill_manual( values=c('weeklyPost'='hotpink',
                                'weeklySame'='orange', 
                                'weeklyPrior'='navy'),
                       # values=c('nPost'='hotpink', 'nSame'='orange', 'nPrior'='navy'),
                       labels = c("Post", 
                                  "Same", 
                                  "Prior")) +
    scale_colour_manual( values=c('After'='navy'), guide='none' ) +
    scale_x_date( expand=c(0,0) ) +
    scale_y_continuous(labels = number_format(scale = 1),
                       sec.axis = sec_axis(trans = ~ . / b *1e-2,
                                           labels = percent_format(),
                                           name = "Percentage diagnosed 0-2 days of ED presentation")) +
    labs( x='Date', 
          y= paste0('ED presentation ', daysbefore, ' day average'),
          title='Emergency dept presentations',
          subtitle='COVID diagnosis prior to entry (1-14 days),\non the day of entry (0 days) or after (1-2 days)') +
    theme_mae() +
    theme( legend.position='bottom' )
  
  return (edPlot)
}



