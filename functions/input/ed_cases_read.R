#  Emergency Department (ED), which also includes triage category 

#ED presentations in this slide include those diagnosed 14 days before presentation to 2 days post presentation. 

ed_cases_read <- function() {
  
  # constants
  ageLevels <- factor( c('0-18', '19-39', '40-64', '65+'), ordered=TRUE )
  
  # connect to board 
  board <- board_rsconnect()
  
  # read pin that has already been previously created 
  
  vemdData <- 
    pin_read( board=board
              , name='dennis.wollersheim@health.vic.gov.au/vemd_linked' ) %>%
    filter(grepl('days',covid_category,ignore.case = TRUE))  %>% # filter for all covid cases  
    mutate(across( contains('date') | ends_with( 'dat' )
                   , ~as.Date(.)))
  
  # Emergency department data 
  edData <- 
    vemdData %>%
    mutate(  diagnosedPost = CalculatedOnsetDate > admdate ,
             diagnosedPostv2 = DiagnosisDate >= admdate ) %>%
    mutate( diagTiming = if_else(DiagnosisDate > admdate, "Post",
                                 if_else(DiagnosisDate == admdate, "Same", "Prior"))) 
  
  edData<- edData  %>% mutate(
    Age = floor(as.numeric(admdate-birthdat)/365.25 ),
    AgeGroup=cut( Age
                  , c(0, 18, 39, 64, Inf)
                  , ageLevels
                  , right=TRUE
                  , include.lowest=TRUE
                  , ordered_result=TRUE ),
    ytd = admdate >= ymd( 20220101 ),
    adm_lastWeek = between( admdate,
                            today(tzone="Australia/Melbourne") - days(14),
                            today(tzone="Australia/Melbourne") - days(8)),
    adm_thisWeek = between( admdate, 
                            today(tzone="Australia/Melbourne") - days(7),
                            today(tzone="Australia/Melbourne")-days(1))) %>%
    filter( covid_category == "Infectious (-2,14] days" & ytd == TRUE ) 
  
  edData <- edData %>%
    select(
      casenumber, 
      tricat,
      CalculatedOnsetDate,
      DiagnosisDate,
      admdate,
      covid_visit_offset,
      covid_category,
      diagnosedPost,
      diagnosedPostv2,
      diagTiming,
      AgeGroup,
      ytd,
      adm_lastWeek,
      adm_thisWeek) %>%
    group_by(AgeGroup,admdate,tricat,adm_lastWeek,adm_thisWeek) %>%
    summarise( ytd = n_distinct(casenumber))
  #lastUpdated <- max(edData$admdate)
  
  return (edData)
}

