# Count PCR results by collection date

pcr_tests_read  <-  function(){
  
  read_query_sitrep(
  
    "SELECT 
      CAST(CollectionDate as DATE) CollectionDate,
      CASE Result 
      WHEN 'Detected' THEN 'Positive'
      ELSE 'Negative'
      END AS Result
    FROM 
      dbo.V_LabResults 
    WHERE 
      CollectionDate IS NOT NULL AND   
      TestType IN ('PCR') AND
      CollectionDate BETWEEN '20200101' and getdate()"
    ) %>% 
  
  group_by(CollectionDate,Result) %>%
    
  count()
  
}

