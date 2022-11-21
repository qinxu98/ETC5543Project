# ETC5543 Project - Optimisation of COVID-19 EPI REPORT

This is a GitHub repo for Unit ETC5543 Business analytics creative activity, Monash University, supervised by Jose Canevari from the Victorian Department of Health.

The aim of this project is to reproduce the original COVID-19 EPI review, but optimise the rendering speed and effiency of the COVID-19 Epi Review through the use of modular workflow and refactoring of the structure of the review.

## File Description 
- input functions are dataset created by directly reading from the database and is stored in the input folder in the function folder 
- input functions are then created as pins in the data_processing.Rmd.
- output functions are created by using the previously created pins and is stored in the output folder in the function folder 
- render_report.rmd is the final rmarkdown that calls ,compiles the output functions and renders in to the final review. 

**The full report describing ths project can be find at**: https://github.com/qinxu98/ETC5543Project/blob/main/Report.html
