# A script to auto-setup for collaborators on Bayesian Thinking, In Context

## Open the terminal tab in RStudio and 
## run the following command: quarto add coatless/quarto-webr

## Check for and Install packages here...
required_packages <- c("tidyverse", "kableExtra", "bayesrules")

for(package in required_packages){
  if(!requireNamespace(package, quietly = TRUE)){
    install.packages(package)
  }
  else{
    print(paste0(package, " already installed"))
  }
}