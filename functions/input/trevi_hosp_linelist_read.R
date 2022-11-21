# Coded by Jose. Canevar
trevi_hosp_linelist_read <- function() {
  
  read_query_sitrep(
    
    "SELECT t1.RecordID, t1.DiagnosisDate, t1.Status, 
    t1.DateOfDeath, t1.Age, t1.Sex, t2.MinAdmissionDate, t2.MAxDischargeDate
    
  FROM 

  (SELECT RecordID, DiagnosisDate, DiedDueToNotifiableCondition AS Status, 
      DateOfDeath, AgeAtOnset AS Age, Sex
     FROM dbo.V_Linelist
     WHERE 
     Classification IN ('Confirmed', 'Probable') AND
     Admitted = '1' AND
     Acquired != 'Travel overseas') as t1, 
  
  (SELECT 
      RecordID,
      MIN(AdmissionDate) AS MinAdmissionDate,
      MAX(DischargeDate) AS MAxDischargeDate
      FROM V_Presentations 
      WHERE PresentedTo = 'Hospital admission' AND
      AdmissionDate IS NOT NULL
      GROUP BY RecordID) as t2
  
  WHERE t1.RecordID = t2.RecordID"
  )
}

trevi_hosp_linelist_read()