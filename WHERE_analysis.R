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
# altering the data table.

## ------------ PROPERTY ACCESS (Q20-24) ------------ ##

# Goal: This should probably be paragraph form instead of graphs, then we can
# support with some of the qualitative information (e.g. question 21; discovery
# of acknowledgements only)

# % that mention property access (question 20) 
  # there should be essentially 3 answers: Yes, in paper text; Yes, in acknowledgements only; and No

Q20 <- WHERE_data_KD$Q20

# unique(Q20)

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
  
  "Yes; in Acknowledgements only" = "Yes, in Acknowledgements only",
  "Yes; in Acknowledgement only" = "Yes, in Acknowledgements only",
  "Yes; only in Acknowledgements"  = "Yes, in Acknowledgements only",
  "Yes; in methods and acknowledgement" = "Yes, in Acknowledgements only",
  "Yes, in Acknowledgment only" = "Yes, in Acknowledgements only",
  "Yes, in Acknowledgments only" = "Yes, in Acknowledgements only",
  "Y; only in the acknowledgment" = "Yes, in Acknowledgements only",
  "Yes; in acknowledgements only" = "Yes, in Acknowledgements only",
  "Y; in acknowledgements only " = "Yes, in Acknowledgements only",
  "yes; only in acknowledgment" = "Yes, in Acknowledgements only",
  "Yes; In acknowledgement only" = "Yes, in Acknowledgements only",
  "Yes; in acknowledgement only" = "Yes, in Acknowledgements only",
  "Yes; in acknowledgments only" = "Yes, in Acknowledgements only",
  "Yes, in acknowledgement only." = "Yes, in Acknowledgements only",
  "Yes, in acknowledgements only" = "Yes, in Acknowledgements only",
  
  
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

# This can be supported by qualitative information from question 21 (authors
# discussing avoiding access requests).



# % where property requires access (most are unknown; question 22)
# % where response rate is reported (mostly no; question 23)
# % where non-responses method is reported (question 24)
# Note these last two need to be filtered to remove “No” answers to Question 22
