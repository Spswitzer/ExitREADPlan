# # Script header ----
# Title: READ Plan State Submission
# Author: Susan Switzer
# Created: 10/14/24
# Revised: 
# The purpose of this script is to access READ Plan data subimission in 2024

#load libraries ----
library(odbc)
library(DBI)
library(janitor)
library(tidyverse)
# connect to SQL database ----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-test",
                 trusted_connection = "true",
                 Port = 1433)

sort(unique(odbcListDrivers()[[1]]))

# Query of READ Plan Flag in Campus ----
qryREADPlanSubmission <- odbc::dbGetQuery(con, 
                                   "
  SELECT
     *
  FROM
    dbSOARS.readplan.tREADSubmission (NOLOCK) 
  WHERE
    tREADSubmission.EndYear = 2024
"
)
