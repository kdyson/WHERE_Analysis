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

## ------------ INTRODUCTION (Q9-12) ------------ ##

# Q9: were both N and sampling design included in the abstract? Usually only N for P category.

Q9 <- WHERE_data_KD$Q9

Q9 <- recode(
  Q9,
  "P; habitats sampled only no sampling choice" = "P",
  "P; n and habitat type but no sampling design method" = "P",
  "n" = "N",
  "P; number only" = "P",
  "No" = "N",
  " P" = "P",
  "P; mention number of neighborhoods and N" = "P",
  "P; N only" = "P",
  "P; N but not method" = "P",
  "P; two sites but v. vague" = "P"
)

table(Q9) ## ONLY 9!!!! with both.

WHERE_data_KD_clean$Q9 <- Q9

# Q10 is the question about having too much justification for working in urban ecosystems...

Q10 <- WHERE_data_KD$Q10

Q10 <- recode(
  Q10,
  "y" = "Y",
  "Yes" = "Y",
  "N; but they do talk too much about sprawl in lit review" = "N",
  "N; borderline" = "N",
  "N; one paragraphâ€¦" = "N",
  "Y?" = "Y"
)

table(Q10)

WHERE_data_KD_clean$Q10 <- Q10

# Q11 idenfies studies that are replications.

Q11 <- WHERE_data_KD$Q11

Q11 <- recode(
  Q11,
  "n" = "N",
  "maybe?" = "N",
  "No" = "N",
  "N; though it does compare measurement methods" = "N",
  "N; though time series with related research published previouslyâ€¦" = "N",
  "Y!" = "Y"
  )

table(Q11)

WHERE_data_KD_clean$Q11 <- Q11


# Q12 Includes the citation for each P or Y from Q11. 

Q12 <- WHERE_data_KD$Q12

recode(
  Q12,
  "N" = "NA",
  "na" = "NA",
  "uses some data from Buttschardt 2001" = "NA")

WHERE_data_KD_clean$Q12 <- Q12

# Q13 lists cities in analyses. need count/paper and 

Q13 <- WHERE_data_KD$Q13

Q13 <- recode(
  Q13,
  "no city; only Stuttgart region mentioned" = "Not Reported",
  "Not Named" = "Not Reported",
  "NOT NAMED" = "Not Reported",
  "Not mentioned" = "Not Reported",
  "not mentioned" = "Not Reported"
)

# note more cleaning is needed to map cities, but I was thinking mapping countries would be sufficient.

Q13_count <- stringr::str_count(Q13, ";") + 1

WHERE_data_KD_clean$Q13 <- Q13

# Q14 Lists the countries involved.

Q14 <- WHERE_data_KD$Q14

Q14 <- recode(
  Q14,
  "Scotland, UK" = "Scotland"
)

Q14_count <- stringr::str_count(Q14, ";") + 1

WHERE_data_KD_clean$Q14 <- Q14

# Q15 map of study area

Q15 <- WHERE_data_KD$Q15

Q15 <- recode(
  Q15,
  "Y; in results only" = "Y",
  "y" = "Y",
  "No" = "N",
  "Yes" = "Y",
  "Y; note it is missing the 34km2 area boundary" = "Y",
  "N; might be one in supplement" = "N",
  "Y; note it is included in a diagram of analysis" = "Y"
)

table(Q15)

WHERE_data_KD_clean$Q15 <- Q15

# Q16 map of study sites 

Q16 <- WHERE_data_KD$Q16

Q16 <- recode(
  Q16,
  "Y; The whole study region is the study site" = "Y",
  "Y; in results only" = "Y",
  "N; points are observed distemper, not study sites" = "N",
  "y" = "Y",
  "Y; though its very confusing" = "Y",
  "Y; it's not a very good map though" = "Y",
  "No" = "N",
  "Yes" = "Y",
  "Y; not cat areas but the neighborhoods" = "Y",
  "N; though there is an interor building map" = "N",
  "Y; however only the park where research was conductedâ€¦" = "Y",
  "Y; only one study site" = "Y",
  "P; sites used for simulated nests not noted" = "N",
  "P; study neighborhoods only" = "N"
)

table(Q16)

WHERE_data_KD_clean$Q16 <- Q16

# how many have study area map and not study sites or vice versa.

Q15_Q16 <- tibble(
  MapPresent = c("No Area Map", "Yes Area Map"),
  `No Site Map` = c(sum(WHERE_data_KD_clean$Q15 == "N" & WHERE_data_KD_clean$Q16 == "N" ),
                    sum(WHERE_data_KD_clean$Q15 == "Y" & WHERE_data_KD_clean$Q16 == "N" )),
  `Yes Site Map` = c(sum(WHERE_data_KD_clean$Q15 == "N" & WHERE_data_KD_clean$Q16 == "Y" ),
                     sum(WHERE_data_KD_clean$Q15 == "Y" & WHERE_data_KD_clean$Q16 == "Y" ))
)

# So these have some data issues because I was trying to make sense of them. So
# about a quarter are my categories and the others are what the authors
# reported. These are best reported as qualitative examples.

# Q17 

Q17 <- WHERE_data_KD$Q17

# Q18 

Q18 <- WHERE_data_KD$Q18

# Also, do a count of where Emad and KD disagree.



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


## ------------ URBAN GRADIENT (Q25) ------------ ##

Q25 <- WHERE_data_KD$Q25

Q25 <- recode(
  Q25,
  [1] "Urban/Rural"                                                                                 
  [2] "No gradient"                                                                                 
  [3] "Urban/Rural dichotomy"                                                                       
  [4] "Neither"                                                                                     
  [5] "Other; gradient based on subjective road variables."                                         
  [6] "Distance from city center; categorical along urban to rural gradient"                        
  [7] "Distance from city center; 1 large study areas chosen for distance from city center regions."
  [8] "Area is one large zone; Urban/Rural dichotomy for bear samples"                              
  [9] "distance from city center"                                                                   
  [10] "Urban/Rural dichotomy; also included forest."                                                
  [11] "No Gradient"                                                                                 
  [12] "Gradient based on continuous variabile; Degree of urbanization"                              
  [13] "no gradient"                                                                                 
  [14] "No gradient; but area chosen for Urban Rural dichotomy"                                      
  [15] "Distance from city center"                                                                   
  [16] "Gradient based on NLCD land cover class"                                                     
  [17] "categorical gradient of desert disturbance"                                                  
  [18] "Urban/Periurban dichotomy"                                                                   
  [19] "Surrounding forest cover"                                                                    
  [20] "Urban/Rural; technically two different urbans"                                               
  [21] "Gradient based on (categorical) variable; Use the PRIZM set for site selection"              
  [22] "Gradient based on another variable; near freeway vs. not"                                    
  [23] "Gradient based on continuous variable (dichotomy used)"                                      
  [24] "Urban Rural dichotomy"                                                                       
  [25] "Distance from city center (urban gradient); used size and landscape characteristics"         
  [26] "Gradient based on continous variable; though they binned it"                                 
  [27] "gradient based on continous variable; socio-economic status"                                 
  [28] "urban/rural dichotomy; though only one apiary sampled"                                       
  [29] "gradient based on another variable; percent impervious"                                      
  [30] "gradient based on another variable; different surrounding land uses"                         
  [31] "gradient based on another continuous variable; households/ha and built cover"                
  [32] "Gradient based on continuous variable; "                                                     
  [33] "Urban Rural gradient"                                                                        
  [34] "Unknown; urbanization gradient mentioned in discussion"                                      
  [35] "gradient based on continuous variables; also analyzed as categorical urban/rural"            
  [36] "categories based on continuous variable (binned housing density)"                            
  [37] "urban/rural dichotmy"                                                                        
  [38] "low/high cityscape"                                                                          
  [39] "land use comparison\\"                                                                       
  [40] "urban/rural dichotomy; gradient based on other variables"                                    
  [41] "gradient based on percent built"                                                             
  [42] "degree of urbanization"                                                                      
  [43] "urban/intermediate/desert"                                                                   
  [44] "urban rural dichotomy"                                                                       
  [45] "Gradient based on restoration site vs. existing forest"                                      
  [46] "distance from city center (urban-rural land use gradient)"                                   
  [47] "urbanization scale; based on Delphi technique"                                               
  [48] "No urban gradient; gradient based on other vars"                                             
  [49] "Urban/protected dichotomy"                                                                   
  [50] "gradient based on another variable; housing density"                                         
  [51] "urbanization gradient not specified?"                                                        
  [52] "Gradient based on traffic conditions"                                                        
  [53] "urban/rural dichotomy"                                                                       
  [54] "Gradient based on household income"                                                          
  [55] "restoration gradient"  
)


