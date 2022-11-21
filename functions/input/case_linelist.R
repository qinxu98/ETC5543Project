# Daily cases and weekly avg cases data read 

cases_linelist_read <- function() {
  cases<- read_query_sitrep("
    select 
  DiagnosisDate,  
  AgeAtOnset,
    CASE WHEN AgeAtOnset >= 0 AND AgeAtOnset < 19 THEN '0-18'
    WHEN AgeAtOnset < 40 THEN '19-39'
    WHEN AgeAtOnset < 65 THEN '40-64'
    WHEN AgeAtOnset >= 65 THEN '65+'
    ELSE NULL
    END AS AgeGroup,
  Sex,
  Count( DiagnosisDate) As n_case
  from dbo.V_LineList
   where DiagnosisDate IS NOT NULL AND
   Classification  IN ('Confirmed', 'Probable') AND 
   DiagnosisDate BETWEEN '20220101' and getdate() 
   GROUP BY  
 DiagnosisDate, AgeAtOnset, Sex
  ORDER BY DiagnosisDATE Desc;")
  
  return(cases)
}




