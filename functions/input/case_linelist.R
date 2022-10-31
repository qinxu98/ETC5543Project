# Daily cases and weekly avg cases data read 

cases_linelist_read <- function() {
  df<- read_query_sitrep(
    "   SELECT CaseNumber,
    RecordID,
    DiagnosisDate,
    CASE WHEN DiagnosisDate >=  Convert(datetime, '2022-01-01') THEN 'TRUE' 
    ELSE 'FALSE' 
    END As YTD,
    case when DiagnosisDate >= DATEADD(DAY, -7, CAST(GETDATE() As Date)) And DiagnosisDate <=  DATEADD(DAY, -1, CAST(GETDATE() AS DATE))
        then 'True'
        else 'False'
    END As 'Thisweek',
    case when DiagnosisDate >= DATEADD(DAY, -14, CAST(GETDATE() As Date)) And DiagnosisDate <=  DATEADD(DAY, -8, CAST(GETDATE() AS DATE))
        then 'True'
        else 'False'
    END As 'Lastweek',
    MetroRural,
    DateOfDeath,
    ClinicalStatus,
    Indigenous,
    AgeAtOnset,
    CASE WHEN AgeAtOnset >= 0 AND AgeAtOnset < 19 THEN '0-18'
    WHEN AgeAtOnset < 40 THEN '19-39'
    WHEN AgeAtOnset < 65 THEN '40-64'
    WHEN AgeAtOnset >= 65 THEN '65+'
    ELSE NULL
    END AS AgeGroup
    FROM dbo.V_LineList
    WHERE Classification IN ('Confirmed','Probable')
    AND AgeAtOnset >= 0;")  # query duration = 14.78 sec 
}




