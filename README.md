# Survey Wave Explorer: Shiny App


## Project Overview
This project is a Shiny Web Application aimed at exploring trends in responses through multiple survey waves. The goal; identify significant changes or differences between survey waves, evaluate the significance of those differences, create downloadable visualizations showcasing trend differences, and lastly provide automated evidence-backed insights to aid user interpretation of results. 

---

## Data & Methods
The project was tested on available D3 survey response data, using snippets of wave merge files. The survey files were all different modes to ensure accuracy, therefore the models included were tested on both CAPI (Computer Assisted Personal Interview) and CATI (Computer Assisted Telephone Interview) surveys. 


These methods are implemented within an interactive Shiny application to allow users to explore results dynamically.

---
## Caveats 
All in-app statistics are calculated without the use of survey weights, which might impact estimates slightly. Estimates that may be affected without the inclusion of weights are; standard error (std.error), critical value (z-value). 


---
## Tools & Technologies
- **R** 
- **Statistical methods: ** Ordinal Logistic Regression with Brant Test Assumption check, Vector Generalized Linear Modeling, Exploratory Data Analysis, Non-response Analysis (Chi-Square Testing)
- **GitHub** (code and project documentation)
- **Packages**: shiny, dplyr, tidyverse, brms, emmeans, ggplot2, ordinal, gofcat, periscope2, VGAM, haven, scales, bslib, shinycssloaders, DT, tinytex, kableExtra, grid/gridExtra
---

## Project Components
- app.r (Ui/Server Components)
- Functions.R (Helper Functions to be downloaded BEFORE app rendering)
- TestFuncs.R (Function Testing script)
- Survey Wave Explorer Notes.pdf (Process/ Choices Documentation)
- report.rmd (Support file for generating test report)
 
