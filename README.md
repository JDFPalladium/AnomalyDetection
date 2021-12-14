# Data.FI Anomaly Finder

This repository holds scripts for the Data.FI Anomaly Finder, a project to create a tool that applies Recommender systems and Time Series to identify anomalies in MER data. This is a private repository for Palladium and USAID staff.

To run the tool, you can download the code in the FinalCode folder. This will contain two folders, one called Recommender and one called Time Series. Each contains two scripts, called main.R and utils.R, and mainTS.R and utilsTS.R, respectively. They also contain an Excel template that is populated when the tool is run, as well as an Outputs folder where tool outputs are saved. The Recommender and Time Series models are run separately. To run them, open the main.R or mainTS.R script, set your working directory to the folder than contains these scripts, set your parameters, and run the script. Progress messages will print to the console.

Alternatively, the tool is available as a packrat bundle, containing all supporting libraries. The tool is available this way in order to avoid any issues with package dependencies. The accompanying user guide explains how to unbundle the packrat version of the tool. If downloading and running the scripts fails due to a dependency issue, try this approach. The packrat package is not posted here but is available upon request to Jonathan.Friedman@thepalladiumgroup.com or adevlin@usaid.gov.


