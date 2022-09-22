#################################################################################################### 
##
## Project: YAML document for the ML4PP 
## Script purpose: Generate a YAML document for the ML4PP website
## Encoding: UTF-8
## Packages: write_yaml
## Date: 22 September 2022
## Author: Michelle González Amador
## Version: v00
## Notes v00:
##
####################################################################################################
rm(list = ls())
#install.packages('yaml')
library(yaml)

# Write content of yaml file  ####
# https://poldham.github.io/simple_website/create_yml.html

name: "ML4PP"
output:
    html_document:
        theme: flatly
navbar:
    title: "Machine Learning for Public Policy"
    left:
        - text: "Home"
          href: index.html
        - text: "1. Introduction"
          href: intro.html
        - text: "2. Regression I"
          href: reg1.html
        - text: "3. Regression II"
          href: reg2.html
        - text: "4. Classification"
          href: classification.html
        - text: "5. Fair ML/Data Ethics"
          href: fairml.html
output_dir: "." # if publishing on Github pages