# Death cases read function
death_cases_read <- function() {
  death_cases<-  read_query_sitrep("SELECT
AgeAtOnset,
CASE WHEN AgeAtOnset >= 0 AND AgeAtOnset < 19 THEN '0-18'
       WHEN AgeAtOnset < 40 THEN '19-39'
       WHEN AgeAtOnset < 65 THEN '40-64'
       WHEN AgeAtOnset >= 65 THEN '65+'
  ELSE NULL
  END AS AgeGroup,
  DateOfDeath,
  ClinicalStatus ,
  count(DateofDeath) as n_death
FROM dbo.V_LineList
WHERE ClinicalStatus = 'Deceased' AND 
DateOfDeath BETWEEN '20220101' and getdate() 
Group By AgeAtOnset, DateOfDeath, ClinicalStatus
Order By DateOfDeath ASC;")
  return(death_cases)
}

