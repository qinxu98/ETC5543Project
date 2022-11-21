# Vaccination data read 

vax_data_read <- function (){
  vaxdata <- read_query_sitrep("
  SELECT           
  EncounterDate,
  DoseNumber,
  Age,
CASE WHEN Age >= 5 AND Age < 12 THEN '05-11'
WHEN Age >= 12 AND Age < 16 THEN '12-15'
WHEN Age >= 16 AND Age < 30 THEN '16-29'
WHEN Age >= 30 AND Age < 50 THEN '30-49'
When Age >= 50 AND Age < 65 THEN '50- 64'
WHEN Age >= 65 THEN '65+'
ELSE NULL
END AS VaxAgeGroup,
CASE WHEN Age >= 5 AND Age < 12 THEN '05-11'
WHEN Age >= 12 AND Age < 16 THEN '12-15'
WHEN Age >= 16 AND Age < 65 THEN '16-64'
WHEN Age >= 65 THEN '65+'
ELSE NULL
END AS AgeGroup,
Count(DoseNumber) as n_vax
                  FROM air.EDW_derived
                  WHERE Age >= 5  AND DoseNumber IS NOT NULL
                  GROUP BY Age, DoseNumber, EncounterDate ;
                  ")   1,
  return(vaxdata)}
