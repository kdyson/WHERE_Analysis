### ------ DATA ANALYSIS ---------- ###

## This file analyzes the analysis-ready data created in the sister file
## WHERE_processing.R. Here we create the percentages and figures needed for
## publication.

## The full workflow for this project is as follows: data collection (Data
## Collection WHERE only Excel) --> compile all journal information and export
## as .csv (Combined Data WHERE.csv) --> process raw data into analysis-ready
## table (WHERE_processing.R) --> analyze data and create figures for publication
## (this file, WHERE_analysis.R)

source("WHERE_processing.R")
library(dplyr)

# KD: I pull out individual question columns to analyze to avoid accidentally
# altering the data table. The recoded columns are then put into a new table of
# cleaned data.

## ------------ CREATE CLEAN TABLE ------------------ ##

WHERE_data_KD_clean <- as_tibble(WHERE_data_KD)
WHERE_data_KD_clean[ , 8:51] <- ""

## ------------ PROPERTY ACCESS (Q20-24) ------------ ##

# Goal: This should probably be paragraph form instead of graphs, then we can
# support with some of the qualitative information (e.g. question 21; discovery
# of acknowledgments only)

# % that mention property access (question 20) 
  # there should be essentially 3 answers: Yes, in paper text; Yes, in Acknowledgments only; and No

Q20 <- WHERE_data_KD$Q20

Q20 <- recode(
  Q20,
  "Yes; in Methods; !!!!!!!! Gj" = "Yes, in paper text",
  "Yes" = "Yes, in paper text",
  "yes" = "Yes, in paper text",
  "Yes; access negotiated by industry partner" = "Yes, in paper text",
  "Yes, in Methods" = "Yes, in paper text",
  "Yes; as which had controlled access" = "Yes, in paper text",
  "Yes; \"Because original sites were placed randomly, observers were instructed to move inaccessible sites (e.g. in a residential back yard) to the nearest accessible location (e.g. sidewalk), and detail such changes with GPS coordinates, street addresses, and/or annotations on the provided maps. \" Note this is in another paper by the author (Turner, W.R., 2003. Citywide biological monitoring as a tool for ecology and conservation in urban landscapes: the case of the Tucson Bird Count. Landscape and Urban Planning, 65(3), pp.149-166.)" = "Yes, in paper text",
  "Yes; mention that NCDOT provided access to sites" = "Yes, in paper text",
  "Yes; in methods" = "Yes, in paper text",
  "Yes; in methods but only 'four sites were visited only once or twice due to access difficulties and were excluded from subsequent analysis'" = "Yes, in paper text",
  "Y; in conclusion only" = "Yes, in paper text",
  "Yes, but obliquely." = "Yes, in paper text",
  "Y; though only obliquely " = "Yes, in paper text",
  "Y; in methods" = "Yes, in paper text",
  
  "Yes; in Acknowledgements only" = "Yes, in Acknowledgments only",
  "Yes; in Acknowledgement only" = "Yes, in Acknowledgments only",
  "Yes; only in Acknowledgements"  = "Yes, in Acknowledgments only",
  "Yes; in methods and acknowledgement" = "Yes, in Acknowledgments only",
  "Yes, in Acknowledgment only" = "Yes, in Acknowledgments only",
  "Yes, in Acknowledgments only" = "Yes, in Acknowledgments only",
  "Y; only in the acknowledgment" = "Yes, in Acknowledgments only",
  "Yes; in acknowledgements only" = "Yes, in Acknowledgments only",
  "Y; in acknowledgements only " = "Yes, in Acknowledgments only",
  "yes; only in acknowledgment" = "Yes, in Acknowledgments only",
  "Yes; In acknowledgement only" = "Yes, in Acknowledgments only",
  "Yes; in acknowledgement only" = "Yes, in Acknowledgments only",
  "Yes; in acknowledgments only" = "Yes, in Acknowledgments only",
  "Yes, in acknowledgement only." = "Yes, in Acknowledgments only",
  "Yes, in acknowledgements only" = "Yes, in Acknowledgments only",
  
  
  "No; Possible access mentioned in acknowledgements but not explicitly" = "No",
  "No; Discussed in other papers" = "No",
  "N" = "No",
  "no" = "No",
  "n " = "No",
  "No; mention 'easily accessed at a road corssing\"" = "No",
  "N; however do discuss cat recruitment" = "No",
  "No; Superintendent thanked in acknowledgements" = "No",
  "Partly; in Acknowledgements they mention the greenkeepers for participating." = "No",
  "Partly" = "No"
)

Q20_table <- table(Q20)
Q20_table/sum(Q20_table)

WHERE_data_KD_clean$Q20 <- Q20

# This can be supported by qualitative information from question 21 (Do authors
# mention using a land use/research design to avoid requesting access?).

Q21 <- WHERE_data_KD$Q21

Q21_Yes <- grep("[Y,y]es;", Q21, value = TRUE)

Q21 <- ifelse(Q21 %in% Q21_Yes, Q21_Yes, "No")

WHERE_data_KD_clean$Q21 <- Q21
  
# 9 papers that avoided requesting access.
# Every other paper is No. (150)

# % where property requires access (most are unknown; question 22)

Q22 <- WHERE_data_KD$Q22

Q22 <- recode(
  Q22,
  "Unknown" = "Not reported",
  "unknown" = "Not reported",
  "Unknown; though presumably harvest permission was required" = "Not reported",
  "Unknown; but probably" = "Not reported",
  
  "Yes" = "Yes",
  "Y" = "Yes",
  "Yes?" = "Yes",
  "Yes;" = "Yes",
  "Yes; from one landowner only" = "Yes",
  "Reported in another paper" = "Yes",
  
  "No" = "No",
  "N" = "No"
)

Q22_table <- table(Q22)
WHERE_data_KD_clean$Q22 <- Q22
  

# % where response rate is reported (mostly no; question 23)

Q23 <- WHERE_data_KD$Q23

Q23 <- recode(
  Q23,
  "Unknown" = "Not reported",
  "unkown" = "Not reported",
  "unknown" = "Not reported",
  "Not Reported" = "Not reported",
  "na" = "Not reported",
  "Unknown; 100% inferred" = "Not reported",
  
  "Reported in another paper" = "Reported in another paper",
  "Unknown; reported in another paper as 2/206" = "Reported in another paper",
  
  "40%" = "Yes",
  "One landowner acknowledged; 100 inferred" = "Yes"
  
)

Q23[which(Q22 == "No" | Q22 == "Not reported")] <- "NA"

Q23_table <- table(Q23)
WHERE_data_KD_clean$Q23 <- Q23


# % where non-responses method is reported (question 24)
# Note these last two need to be filtered to remove “No” answers to Question 22

Q24 <- WHERE_data_KD$Q24

Q24 <- recode(
  Q24,
  "Unknown" = "Not reported",
  "unkown " = "Not reported",
  "Not Reported" = "Not reported",
  "Not reported" = "Not reported",
  "unknown" = "Not reported",
  "na" = "Not reported",
  "Unknown; though some sites did drop out" = "Not reported",
  "Unknown; inferred 1 landowner accepted" = "Not reported",
  "Site not chosen?"  = "Not reported",
  
  "Unknown; reported in another paper as not sampled" = "Reported in another paper as not sampled"
)

Q24[which(Q22 == "No" | Q22 == "Not reported")] <- "NA"
Q24_table <- table(Q24)
WHERE_data_KD_clean$Q24 <- Q24


remove(Q20,Q21,Q22,Q23,Q24)
