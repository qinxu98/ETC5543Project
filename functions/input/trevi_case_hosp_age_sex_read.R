#Coded by Jose.Canevar
trevi_case_hosp_age_sex_read <- function() {
  
  read_query_sitrep(
    
    "SELECT 
      DiagnosisDate,
      ISNULL(Admitted, 0) AS Admitted, 
      Sex, 
      AgeGroupTenYr, 
      COUNT(DiagnosisDate) AS Count
    FROM V_LineList 
        WHERE DiagnosisDate IS NOT NULL AND
      Classification IN ('Confirmed', 'Probable')
    GROUP BY DiagnosisDate, ISNULL(Admitted, 0), Sex, AgeGroupTenYr 
    ORDER BY DiagnosisDate DESC;"
  )
  
}
