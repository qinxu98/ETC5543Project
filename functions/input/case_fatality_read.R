case_fatality_read<- function(){
  df <- read_query_sitrep("
    SELECT 
    DiagnosisDate,
    AgeAtOnset,
    DateOfDeath,
    ClinicalStatus
    FROM dbo.V_LineList 
    where DiagnosisDate IS NOT NULL;")
  
  return(df) }


