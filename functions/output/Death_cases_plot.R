# Death_cases_plot

Death_cases_plot <- function( daysbefore = 7, daysafter = 0) {
  
  # connect to the board
  board <- pins::board_register_rsconnect()
  
  # read in case 
  death_case<- pin_read(board,"qin.xu@health.vic.gov.au/epideath_case_read") 
  case <- pin_read(board,"qin.xu@health.vic.gov.au/epicases_linelist_read")
  CFR <- pin_read(board,"qin.xu@health.vic.gov.au/epicase_fatality_read")
  
  
  deathPlotData <- death_case %>%
    group_by(DateOfDeath) %>%
    summarise( n = sum(n_death)) %>%
    mutate( averageDeaths =slide_dbl( n
                                      , .f = mean
                                      , .before = daysbefore
                                      , .after = daysafter) ) %>%
    mutate( Date = DateOfDeath ) %>%
    rename( nDeathDay = "n")
  
  # Caseplotdata 
  casePlotData <- case %>%
    filter( DiagnosisDate >= ymd( 20220101 )
            , DiagnosisDate < today(tzone="Australia/Melbourne") ) %>%
    count( DiagnosisDate) %>%
    mutate( weeklyAverage=slide_dbl( .x = n
                                     , .f= mean
                                     , .before= daysbefore  
                                     , .after= daysafter))
  
  # Adjusted CFR prep
  cfrPrep <- CFR %>% 
    filter(DiagnosisDate >= as.Date("2022-01-01") & 
             DiagnosisDate <= today(tzone="Australia/Melbourne") - days(7))
  
  dx.de.all <- as.numeric(cfrPrep$DateOfDeath - cfrPrep$DiagnosisDate) # DATE OF DEATH - DAIGNOSIS DATE 
  dx.de.all <- dx.de.all[which(!is.na(dx.de.all) & dx.de.all >=0)] # FILTER FOR ALL WHERE IT IS NOT NULL AND IS GREATER THAN 0.
  
  # hist(dx.de.all)
  # hist(log(dx.de.all))
  
  zmeanDxDeAll <- mean(dx.de.all) # MEAN  
  
  zmedianDxDeAll <- median(dx.de.all) # MEDIAN
  
  zsdDxDeAll <- sd(dx.de.all) # sd 
  
  # if the log(zmeanDxDeAll) is > log(zmeanDxDeAll); data likely skewed, otherwise use the SD of log(zmeanDxDeAll)
  
  sigmaDxDeAll <- ifelse(log(zmeanDxDeAll) - log(zmedianDxDeAll) > 0, sqrt(2 * (log(zmeanDxDeAll) - log(zmedianDxDeAll))), sd(log(dx.de.all)))
  
  muDxDeAll <- ifelse(log(zmeanDxDeAll) - log(zmedianDxDeAll) > 0, log(zmedianDxDeAll), log(zmeanDxDeAll))
  # cumulative prob of diagnosis to death distribution:  
  dxdeall.trunc <- function(x){
    plnorm(x, muDxDeAll, sigmaDxDeAll)
  }
  
  
  cfrData <- CFR %>%
    filter(DiagnosisDate <= today(tzone="Australia/Melbourne") - days(7),
           DiagnosisDate >= ymd( 20220101 )) %>%
    group_by( DiagnosisDate ) %>%
    summarise( nCase = n()
               , nDeath = sum( ClinicalStatus == "Deceased" ))%>%
    mutate(monthyear = format(as.Date(DiagnosisDate), "%Y-%m"),
           time_from_diagnosis = as.numeric(today(tzone = "Australia/Melbourne")-DiagnosisDate),
           ancase = nCase * dxdeall.trunc(time_from_diagnosis))
  
  
  cfrData <- cfrData %>% 
    mutate(method = 'crude') %>%
    bind_cols(as.data.frame(
      epi.conf(dat=cbind(cfrData$nDeath, cfrData$nCase - cfrData$nDeath), ctype ="prop.single"), digits = 3)
    ) %>%
    bind_rows(
      cfrData %>% 
        mutate(method = 'adjusted') %>%
        bind_cols(
          as.data.frame(
            epi.conf(dat = cbind(cfrData$nDeath, cfrData$ancase - cfrData$nDeath), ctype="prop.single"), digits = 3))) %>%
    rename( Date = DiagnosisDate ) %>%
    group_by(method) %>%
    mutate( averageEst=slide_dbl( est, .f=mean, .before = daysbefore, .after = daysafter ),
            averageLower=slide_dbl( lower, .f=mean, .before = daysbefore, .after = daysafter ),
            averageUpper=slide_dbl( upper, .f=mean, .before = daysbefore, .after = daysafter ))
  
  cfrDataWide <- cfrData %>% 
    pivot_wider(id_cols = c(Date:ancase), 
                names_from = method, 
                values_from = averageEst)
  
  deathPlotData <- deathPlotData %>%
    left_join( cfrDataWide,
               by='Date' ) %>%
    left_join( casePlotData %>% rename(nCases = "n", 
                                       averageCases = "weeklyAverage"),
               by =  c("Date"="DiagnosisDate"))
  
  deathPlotDataLong <- deathPlotData %>%
    mutate(crude = crude*10000,
           adjusted=adjusted*10000) %>%
    rename(crudeCFR="crude", 
           adjustedCFR="adjusted") %>%
    select(Date, averageDeaths,
           crudeCFR, 
           adjustedCFR, 
           averageCases) %>%
    pivot_longer(cols=c(averageDeaths, 
                        crudeCFR, 
                        adjustedCFR, 
                        averageCases), 
                 names_to = "var", 
                 values_to = "ave") %>%
    mutate( Type = ifelse(var == "averageDeaths", "Deaths",
                          ifelse(var == "crudeCFR" | var=="adjustedCFR", "CFR",
                                 ifelse(var == "averageCases", "Cases", NA)))) %>%
    mutate( var = ifelse(var == "crudeCFR", "averageCFRper10,000", var)) %>%
    mutate( var = ifelse(var == "adjustedCFR", "averageCFRAdjustedper10,000", var)) %>%
    mutate( var = factor(var, levels = c("averageDeaths", "averageCases", "averageCFRper10,000", "averageCFRAdjustedper10,000"))) %>%
    mutate( Type = factor( Type, levels = c("Deaths", "Cases", "CFR")))
  
  deathPlot <- ggplot(deathPlotDataLong) +
    geom_line( aes( x = Date, y = ave, colour = var), size = 1.2) +
    annotate("rect", xmin=today(tzone = "Australia/Melbourne")-14, xmax = today(tzone = "Australia/Melbourne")-1, ymin = 0, ymax = Inf, fill = "grey80", alpha = 0.5)+
    facet_wrap( ~ Type, nrow = 3, scales = "free_y") +
    labs( x = "Date", y = "7-day average", title = "Deaths, Cases and Case Fatality Rate",
          subtitle = "CFR is deaths per 10,000 cases",
          caption = "Death reporting is lagged. Whilst ~50% of deaths are actioned by DH in the two days after death, at 14 days post Death, ~80% have been actioned.\nThus, caution is required when interpreting death data and CFR in the past few weeks. (Grey period - caution recommended)\nAdjusted CFR uses the method described by Nishiura et al (2009)")+
    scale_colour_manual(values = c("#091540", "#cc1219", "#5f4b8b", "#0e6ca6"), labels = c("aveDeaths", "aveCases", "aveCFR", "aveCFRAdjusted"))+
    guides( colour = guide_legend( title = 'Metric' )
            , linetype = guide_legend( title = 'Rolling average' ) ) +
    theme_mae() +
    geom_hline(yintercept = 0, colour = "grey80") +
    theme(legend.position = 'bottom') +
    theme(plot.caption = element_text(size = 14, colour = "#222222"))
  
  
  return(deathPlot) 
}



