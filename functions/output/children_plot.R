# Children Plot 
children_plot <- function(plot_name, daysbefore = 7, daysafter = 0 ) {
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  
  # read in Population data pin 
  popData2020 <- pin_read( board = board
                           ,name = 'craig.savage@health.vic.gov.au/pop2020')
  
  popAgeDataChildren <- popData2020 %>%
    mutate( ChildAgeGroups = cut(age_abs
                                 , breaks = c(0,4,11,18,Inf)
                                 , labels = c('0-4', '5-11','12-18','19+')
                                 , right = TRUE
                                 , include.lowest = TRUE
                                 , ordered = TRUE ) ) %>%
    group_by( ChildAgeGroups ) %>%
    summarise( Pop = sum( population ))
  
  # read cases pin 
  child_case_data <- pin_read(board,"qin.xu@health.vic.gov.au/epicases_linelist_read") %>%
    mutate( ChildAgeGroups=cut(AgeAtOnset
                               , breaks = c(0,4,11,18,Inf)
                               , labels = c('0-4', '5-11','12-18','19+')
                               , right = TRUE
                               , include.lowest = TRUE
                               , ordered = TRUE )) %>%
    select(-c (AgeAtOnset, AgeGroup)) %>%
    mutate(  thisWeek=between( DiagnosisDate
                               , today(tzone = "Australia/Melbourne")-days(7)
                               , today(tzone = "Australia/Melbourne") - days(1) )
             , lastWeek=between( DiagnosisDate
                                 , today(tzone = "Australia/Melbourne")-days(14)
                                 , today(tzone = "Australia/Melbourne")-days(8)))
  
  
  # Child case plot data
  child_case_plot_data <-  child_case_data %>%
    filter( DiagnosisDate >= ymd( 20220101 )
            , DiagnosisDate < today(tzone="Australia/Melbourne")) %>%
    group_by( ChildAgeGroups, DiagnosisDate) %>%
    summarise ( n = sum(n_case))%>%
    mutate( propDaily = n/sum(n) * 100) %>%
    group_by(ChildAgeGroups ) %>%
    mutate( average=slide_dbl( .x = n
                               , .f = mean
                               , .before = daysbefore
                               , .after = daysafter ) ) %>%
    mutate( averageProp = slide_dbl( .x = propDaily
                                     , .f = mean
                                     , .before = daysbefore
                                     , .after =  daysafter) ) %>%
    ungroup() %>%
    inner_join( popAgeDataChildren, by="ChildAgeGroups" ) %>%
    mutate( rate = n/Pop*100000 ) %>%
    mutate( averageRate=slide_dbl( .x = rate
                                   , .f = mean
                                   , .before = daysbefore
                                   , .after = daysafter)) 
  
  # ChildCasePropPlot
  ChildCasePropPlot <- ggplot(   child_case_plot_data %>%
                                   filter(ChildAgeGroups !="19+")) + 
    geom_line( aes( x=DiagnosisDate, 
                    y=averageProp, 
                    colour=ChildAgeGroups), 
               size=1.2) +
    # geom_bar( aes( x=DiagnosisDate, y=averageProp, fill=ChildAgeGroups ), stat="identity"
    #           , position = position_fill(reverse = T)
    #           ) +
    # facet_wrap(~ChildAgeGroups, nrow=4, scales='free_y') +
    labs( x ='Date',
          y =paste0('', daysbefore, ' -day Average Proportion of Total Cases'),
          title ='Proportion of cases in child age groups'
          , subtitle ='As a daily proportion of total reported cases' ) +
    scale_x_date(date_breaks = "month",
                 labels = date_format("%b"),
                 expand = c(0,0), 
                 limits = c(as.Date("2022-01-01"), 
                            as.Date(today(tzone="Australia/Melbourne")))) +
    scale_color_manual(values = c("#481567FF", 
                                  "#2D708EFF", 
                                  "#3CBB75FF"))+
    theme_mae() +
    theme( legend.position = "bottom" )
  
  
  # plot2 - Rate plot from 20220102 to latest date
  
  ChildCaseRatePlot <- ggplot(child_case_plot_data ) + 
    geom_line( aes( x = DiagnosisDate, 
                    y = averageRate, 
                    colour = ChildAgeGroups), 
               size = 1.2) +
    labs( x ='Date', 
          y ='', daysbefore, '-day Average Case Rate (per 100,000)', 
          title = 'Daily case rate (per 100,000 population) by child age groups',
          subtitle ='Smoothed ', daysbefore, '-day average rate' ) +
    scale_x_date(date_breaks = "month",
                 labels = date_format("%b"),
                 expand=c(0,0), limits = c(as.Date("2022-01-01"),
                                           as.Date(today(tzone="Australia/Melbourne")))) +
    scale_color_manual(values=c("#481567FF",
                                "#2D708EFF", 
                                "#3CBB75FF", 
                                "#FDE725FF")) +
    theme_mae() +
    theme( legend.position = "bottom" )
  
  
  #  
  
  ChildCaseRateFromT3Plot <- ggplot(child_case_plot_data%>% 
                                      filter(DiagnosisDate >= as.Date("2022-06-25"))) + 
    geom_line( aes( x = DiagnosisDate, 
                    y = averageRate, 
                    colour = ChildAgeGroups), 
               size=1.2) +
    labs( x='Date',
          y='', daysbefore, '-day Average Case Rate (per 100,000) since Term 2 end',
          title='Daily case rate (per 100,000 population) by child age groups'
          , subtitle='Smoothed ', daysbefore, '-day average rate' 
          , caption="Term dates are shown as dotted lines") +
    geom_vline(xintercept = c(as.Date("2022-09-16"),
                              as.Date("2022-07-11"),
                              as.Date("2022-06-25")),
               linetype = 2) +
    scale_x_date(date_breaks = "2 week",
                 labels = date_format("%d %b"),
                 expand=c(0,0), limits = c(as.Date("2022-06-25"), 
                                           as.Date(today(tzone="Australia/Melbourne" ) ) ) ) +
    scale_color_manual(values=c("#481567FF", "#2D708EFF", "#3CBB75FF", "#FDE725FF")) +
    theme_mae() +
    theme( legend.position = "bottom" ) +
    theme(plot.caption = element_text(size=14, colour="#222222")) 
  
  
  
  return(get(plot_name))
}





