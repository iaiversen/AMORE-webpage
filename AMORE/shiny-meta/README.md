# amore shiny App documentation
## Shinyapps.io Account Information
- Account Name: meta-oxytocin
- Account URL: https://www.shinyapps.io/admin/#/dashboard
- Account ID: 2889222

## Deployment Information
The Shiny application for AMORE's Living Meta-Analysis Directory is hosted on shinyapps.io. 
- App URL: https://meta-oxytocin.shinyapps.io/shiny-meta/
- Repository Location: AMORE/shiny-meta/

## Going live 
- when going live the "living meta analysis" in quarto.yml must be changed from LMA_placeholder.qmd to LMA.shiny.qmd
- Now the LMA.shiny.qmd is called LMA.shiny.qmd.bak to prevent it from disturbing the rendering when developing the webpage, remove the .bak from the filename and it becomes an active part of the project repository again
- change quarto.yml file by removing comments so that the lines becomes ative commands 
---
