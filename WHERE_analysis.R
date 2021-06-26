### ------ DATA ANALYSIS ---------- 

## This file analyzes the analysis-ready data created in the sister file
## WHERE_processing.R. Here we create the percentages and figures needed for
## publication.

## The full workflow for this project is as follows: data collection (Data
## Collection WHERE only Excel) --> compile all journal information and export
## as .csv (Combined Data WHERE.csv) --> process raw data into analysis-ready
## table (WHERE_processing.R) --> analyze data and create figures for publication
## (this file, WHERE_analysis.R)

source("WHERE_processing.R")
library(ggplot2)
library(dplyr)

# KD: I pull out individual question columns to analyze to avoid accidentally
# altering the data table. The recoded columns are then put into a new table of
# cleaned data.

## ------------ CREATE CLEAN TABLE ------------------ 

WHERE_data_KD_clean <- as_tibble(WHERE_data_KD)
WHERE_data_KD_clean[ , 8:51] <- ""

## ------------ SUMMARY STATS (Q9-12) ------------

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

Q9_table <- table(Q9) ## ONLY 9!!!! with both.

WHERE_data_KD_clean$Q9 <- Q9







# Q10 is the question about having too much justification for working in urban ecosystems...

Q10 <- WHERE_data_KD$Q10

Q10 <- recode(
  Q10,
  "y" = "Y",
  "Yes" = "Y",
  "N; but they do talk too much about sprawl in lit review" = "N",
  "N; borderline" = "N",
  "N; one paragraph…" = "N",
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
  "N; though time series with related research published previously…" = "N",
  "Y!" = "Y"
  )

table(Q11)

WHERE_data_KD_clean$Q11 <- Q11


# Q12 Includes the citation for each P or Y from Q11. 

Q12 <- WHERE_data_KD$Q12

Q12 <- recode(
  Q12,
  "N" = "NA",
  "na" = "NA",
  "uses some data from Buttschardt 2001" = "NA"
  )

Q12[Q12 == "NA"] <- NA
WHERE_data_KD_clean$Q12 <- Q12

# Q13 lists cities in analyses. 

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
  "Scotland, UK" = "UK",
  "England" = "UK",
  "Kashmir Himalaya; Canada" = "India; Canada",
  "Belgium; Netherlands; Germany; Poland; the Czech Republic; Slovakia; Switzerland; Austria; Slovenia;  Hungary" = "Belgium; Netherlands; Germany; Poland; Czech Republic; Slovakia; Switzerland; Austria; Slovenia;  Hungary"
)

Q14_count <- stringr::str_count(Q14, ";") + 1

WHERE_data_KD_clean$Q14 <- Q14


all_countries <- stringr::str_trim(unlist(stringr::str_split(WHERE_data_KD_clean$Q14, "; ")), side = "both")
table(all_countries)
unique(all_countries)




# Q15 map of study area

Q15 <- WHERE_data_KD$Q15

Q15 <- recode(
  Q15,
  "Y; in results only" = "Yes",
  "y" = "Yes",
  "N" = "No",
  "Y" = "Yes",
  "Y; note it is missing the 34km2 area boundary" = "Yes",
  "N; might be one in supplement" = "No",
  "Y; note it is included in a diagram of analysis" = "Yes"
)


WHERE_data_KD_clean$Q15 <- Q15
table(WHERE_data_KD_clean$Q15)


# Q16 map of study sites 

Q16 <- WHERE_data_KD$Q16

Q16 <- recode(
  Q16,
  "Y" = "Yes",
  "N" = "No",
  "Y; The whole study region is the study site" = "Yes",
  "Y; in results only" = "Yes",
  "N; points are observed distemper, not study sites" = "No",
  "y" = "Yes",
  "Y; though its very confusing" = "Yes",
  "Y; it's not a very good map though" = "Yes",

  "Y; not cat areas but the neighborhoods" = "Yes",
  "N; though there is an interor building map" = "No",
  "Y; however only the park where research was conducted…" = "Yes",
  "Y; only one study site" = "Yes",
  "P; sites used for simulated nests not noted" = "No",
  "P; study neighborhoods only" = "No"
)

table(Q16)

WHERE_data_KD_clean$Q16 <- Q16

# how many have study area map and not study sites or vice versa.

Q15_Q16 <- tibble(
  MapPresent = c("No Area Map", "Yes Area Map"),
  `No Site Map` = c(sum(WHERE_data_KD_clean$Q15 == "No" & WHERE_data_KD_clean$Q16 == "No" ),
                    sum(WHERE_data_KD_clean$Q15 == "Yes" & WHERE_data_KD_clean$Q16 == "No" )),
  `Yes Site Map` = c(sum(WHERE_data_KD_clean$Q15 == "No" & WHERE_data_KD_clean$Q16 == "Yes" ),
                     sum(WHERE_data_KD_clean$Q15 == "Yes" & WHERE_data_KD_clean$Q16 == "Yes" ))
)

# So these have some data issues because I was trying to make sense of them. So
# about a quarter are my categories and the others are what the authors
# reported. These are best reported as qualitative examples.

# Q17 

Q17 <- WHERE_data_KD$Q17

Q17 <- recode(Q17,
  
  "unknown" = "Not reported",
  "Unknown" = "Not reported",
  "not reported" = "Not reported"
)

WHERE_data_KD_clean$Q17 <- Q17

# Q18 

WHERE_data_KD_clean$Q18 <- WHERE_data_KD$Q18

# Also, do a count of where Emad and KD disagree.

remove(Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18)


## ------------ PROPERTY ACCESS (Q20-24) ------------ 

# Goal: This should probably be paragraph form instead of graphs, then we can
# support with some of the qualitative information (e.g. question 21; discovery
# of acknowledgments only)

# What type of property? Fairly lenient here, if I have a ? then its ignored.

Q19 <- WHERE_data_KD$Q19

Q19 <- if_else(grepl("Unknown|unknown|not|Not", Q19), "Not reported", Q19)

WHERE_data_KD_clean$Q19 <- Q19

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
# Every other paper is No. (149)

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

Q24[(Q22 == "No" | Q22 == "Not reported")] <- "NA"
Q24_table <- table(Q24)
WHERE_data_KD_clean$Q24 <- Q24


remove(Q20,Q21,Q22,Q23,Q24)


## ------------ URBAN GRADIENT (Q25) ------------ 

# The 'other' gradients are either categorical or continuous. 

Q25 <- WHERE_data_KD$Q25

Q25 <- recode(
  Q25,
  "Urban/Rural" = "Urban and Rural dichotomy",
  "Urban/Rural dichotomy" = "Urban and Rural dichotomy",
  "Area is one large zone; Urban/Rural dichotomy for bear samples"    = "Urban and Rural dichotomy",
  "Urban/Rural dichotomy; also included forest." = "Urban and Rural dichotomy",
  "Urban/Periurban dichotomy" = "Urban and Rural dichotomy",
  "Urban/Rural; technically two different urbans" = "Urban and Rural dichotomy",
  "Urban Rural dichotomy" = "Urban and Rural dichotomy",
  "urban/rural dichotomy; though only one apiary sampled" = "Urban and Rural dichotomy",
  "Urban Rural gradient" = "Urban and Rural dichotomy",
  "Urban/protected dichotomy" = "Urban and Rural dichotomy",
  "urban/rural dichotmy" = "Urban and Rural dichotomy",
  "urban/intermediate/desert" = "Urban and Rural dichotomy",
  "urban rural dichotomy" = "Urban and Rural dichotomy",
  "urban/rural dichotomy" = "Urban and Rural dichotomy",
  "urban/rural dichotomy; gradient based on other variables" = "Urban and Rural dichotomy",
  
  "No Gradient" = "No gradient",
  "Neither" = "No gradient",
  "no gradient" = "No gradient",
  "No gradient; but area chosen for Urban Rural dichotomy"= "No gradient",
  
  "Other" = "Other variable for gradient",
  "Other; gradient based on subjective road variables." = "Other variable for gradient",
  "Gradient based on continuous variabile; Degree of urbanization"   = "Other variable for gradient",
  "Gradient based on NLCD land cover class" = "Other variable for gradient",
  "categorical gradient of desert disturbance" = "Other variable for gradient",
  "Surrounding forest cover" = "Other variable for gradient",
  "Gradient based on (categorical) variable; Use the PRIZM set for site selection" = "Other variable for gradient",
  "Gradient based on another variable; near freeway vs. not" = "Other variable for gradient",
  "Gradient based on continuous variable (dichotomy used)" = "Other variable for gradient",
  "Gradient based on continous variable; though they binned it" = "Other variable for gradient",
  "gradient based on continous variable; socio-economic status" = "Other variable for gradient",
  "gradient based on another variable; percent impervious" = "Other variable for gradient",
  "gradient based on another variable; different surrounding land uses" = "Other variable for gradient",
  "gradient based on another continuous variable; households/ha and built cover" = "Other variable for gradient",
  "Gradient based on continuous variable; " = "Other variable for gradient",
  "categories based on continuous variable (binned housing density)" = "Other variable for gradient",
  "Gradient based on traffic conditions" = "Other variable for gradient",
  "land use comparison\\" = "Other variable for gradient",
  "degree of urbanization" = "Other variable for gradient",
  "No urban gradient; gradient based on other vars" = "Other variable for gradient",
  "urbanization scale; based on Delphi technique" = "Other variable for gradient",
  "gradient based on another variable; housing density" = "Other variable for gradient",
  "Gradient based on household income" = "Other variable for gradient",
  "restoration gradient" = "Other variable for gradient",
  "low/high cityscape" = "Other variable for gradient",
  "gradient based on percent built" = "Other variable for gradient",
  "Gradient based on restoration site vs. existing forest" = "Other variable for gradient",
  "gradient based on continuous variables; also analyzed as categorical urban/rural" = "Other variable for gradient",
  
  "Distance from city center; categorical along urban to rural gradient" = "Distance from city center",
  "Distance from city center; 1 large study areas chosen for distance from city center regions." = "Distance from city center",
  "distance from city center" = "Distance from city center",
  "Distance from city center (urban gradient); used size and landscape characteristics" = "Distance from city center",
  "distance from city center (urban-rural land use gradient)" = "Distance from city center",
  
  "Unknown; urbanization gradient mentioned in discussion" = "Unknown",
  "urbanization gradient not specified?" = "Unknown"
)


Q25_table <- table(Q25)
WHERE_data_KD_clean$Q25 <- Q25

remove(Q25)

## ------------ SAMPLING FRAME vs. POPULATION (Q27-29) ------------ 

## Q26

WHERE_data_KD_clean$Q26 <- WHERE_data_KD$Q26


## Question 27: % where population q includes ‘unknown’ (question 27). This is
## probably an underestimate b/c we did make some assumptions although we tried
## not to. Can support with qualitative data from the ‘evaluator notes’

Q27 <- WHERE_data_KD$Q27

Q27 <- recode(
  Q27,
  "unknown" = "Unknown",
  "Unknown; all neighborhoods with cats?" = "Unknown",
  "Unknown; Not reported" = "Unknown"
)

Q27[Q27 %in% grep("\\?", Q27, value = TRUE)] <- "Uncertain"

Q27_table <- table(Q27)
WHERE_data_KD_clean$Q27 <- Q27

# 56 Unknown and 25 Uncertain. So about one third I couldn't understand. Others are
# generally reasoned out based on the text. not many explicitly said 'study
# population' or whatever.



## Question 28: % where sampling frame includes ‘unknown’ (question 28). Can
## support with qualitative data from the evaluator notes.

Q28 <- WHERE_data_KD$Q28

Q28 <- recode(
  Q28,
  "unknown" = "Unknown",
  "Unknown; habitat types near Port-of-Spain the author had access to?" = "Unknown",
  "Unknown; Not reported" = "Unknown"
)

Q28[Q28 %in% grep("\\?", Q28, value = TRUE)] <- "Uncertain"

Q28_table <- table(Q28)
WHERE_data_KD_clean$Q28 <- Q28

# 63 Unknown and 19 Uncertain. So about one third I couldn't understand. Others are
# generally reasoned out based on the text. not many explicitly said 'study
# population' or whatever.

Q27_Q28 <- tibble(
  Pop_SampFrame = c("Population Unknown or Uncertain", "Population Understood"),
  `Sampling Frame Unknown or Uncertain` = c(
    sum(
      WHERE_data_KD_clean$Q27 %in% c("Unknown", "Uncertain") &
        WHERE_data_KD_clean$Q28 %in% c("Unknown", "Uncertain")
    ),
    sum(
      !(WHERE_data_KD_clean$Q27 %in% c("Unknown", "Uncertain")) &
        WHERE_data_KD_clean$Q28 %in% c("Unknown", "Uncertain")
    )
  ),
  `Sampling Frame Understood` = c(sum(
    WHERE_data_KD_clean$Q27 %in% c("Unknown", "Uncertain") &
      !(WHERE_data_KD_clean$Q28 %in% c("Unknown", "Uncertain"))
  ),
  sum(
    !(WHERE_data_KD_clean$Q27 %in% c("Unknown", "Uncertain")) &
      !(WHERE_data_KD_clean$Q28 %in% c("Unknown", "Uncertain"))
  )
  ))


## Q29: % where study site population and sampling frame match (question 29).
## This will be mostly unknowns. Common reason of unknown for 29 is that
## sampling frame was very subjective. For example sites meeting authors
## criteria--don’t know how that compares with population because not stated, or
## criteria not well defined. “Sites with similar vegetation and soils” or
## “Sites meeting author’s selection criteria” or “plots authors got access to”


Q29 <- WHERE_data_KD$Q29

Q29 <- recode(
  Q29,
  "No; forest patches subdivided into 100x100m " = "No",
  "no  " = "No",
  "No; Sampling frame is smaller than study site population and more homogenous" = "No",
  "N" = "No",
  "Frame is subset of population representing extremes of gradient" = "No",
  "No--small sites excluded" = "No",
  "N; but this doesn't increase bias I don't think" = "No",
  "No, excludes irregular or smallest" = "No",
  "No; possible issue where some residential sites were located more than 100m away from water" = "No",
  "No?" = "No",
  "No; or at least probably not" = "No",

  "Yes; though more evenly distributed than completely random the 4km grid essentially acts as a strata" = "Yes",
  "Yes; authors point out differences and commonalities between samples and population" = "Yes",
  "Y" = "Yes",
  "partly; they're more evenly distributed" = "Yes",
  "Y; note this limits generalizability" = "Yes",
  "yes" = "Yes",
  "Yes? But just this one area" = "Yes",
  "Yes?" = "Yes",
  "\"gardens in the study reflect southern English suburbia\"" = "Yes",
  "Yes; based on previous study" = "Yes",
  "Partly, authors claim forest types the same as 57% of forest cover in MA" = "Yes",
  "Yes; although they mght not if data is bad. But they say database is census" = "Yes",
  "Maybe" = "Yes",
  "Probably" = "Yes",
  
  "unknown" = "Unknown",
  "Unknown; not reported" = "Unknown",
  "Unknown; authors are emphatic that district is representative" = "Unknown",
  "Unknown, not well defined" = "Unknown",
  "Unknown; sampling frame not well defined" = "Unknown",
  "Unknown; possibly?" = "Unknown"
  
)

Q29[is.na(Q29)] <- "No"

## If either is unknown then the match should be unknown. This checks out.

Q29[Q27 == "Unknown" | Q28 == "Unknown"]

Q29_table <- table(Q29)
WHERE_data_KD_clean$Q29 <- Q29


remove(Q27,Q28,Q29)

## ------------ SAMPLING DESIGN (Q30-38) ------------ 

## Clean up the number of sampling sites

#	This is an extremely basic question, but even here there are papers that
#	didn’t report. Other papers were written in a way that the number of sampling
#	sites wasn’t clear, or reported the number only in the abstract and not in the
#	methods.


Q30 <- WHERE_data_KD$Q30

Q30 <- recode(
  Q30,
  "4; bird species" = "4",
  "18; I think?? Only mentioned in abstract, not methods" = "18",
  "36?" = "36",
  "674; 550 + 124 opportunisticly sampled plots" = "674",
  "34;" = "34",
  "17 streets" = "17",
  "Either 17 or 41 depending on how you count" = "Unknown",
  "91?" = "Unknown",
  "9; though 21 sites mentione" = "Unknown",
  "1 site for each of 5 studies?" = "5",
  "Unknown?" = "Unknown",
  "~4000" = "4000",
  "2;10" = "12",
  "6? Or 36? Or maybe more?" = "Unknown",
  "102 sampling sites" = "102",
  "1 or 51" = "Unknown",
  "296; this isnt mentioned till results" = "296",
  "Not clear--could be 3 landscapes they focused on, 9 starting landscapes or 140?? for forest patches" = "Unknown"
)

Q30_table <- table(Q30)
Q30_numeric <- as.numeric(Q30[!is.na(as.numeric(Q30))])
WHERE_data_KD_clean$Q30 <- Q30


## % using non-probabilitic sampling (question 31 & 32)

Q31 <-WHERE_data_KD$Q31

Q31 <- recode(
  Q31,
  "Unknown; likely Non-probability convenience" = "Non-probability; Stratified", # Fonaroff has little info but probably just went where they wanted in strata. Same with Ficetola. Ficetola not stratified.
  "Unknown; non-probability sample likely" = "Non-probability", #another 'this was sampled'
  "Unknown; likely what was accessible?" = "Unknown; Clustered", # these sampling areas were chosen post-hoc---the polygons are based on waterbodies the turtles visited or something. 
  "Unknown; likely non-probability conveneince sample" = "Unknown",
  "unknown " = "Non-probability", # this one they chose a sampling area, then identified forest stands within it. Non probability refers to chosing sampling area
  "Unknown  " = "Unknown",
  "unknown; likely non-probability" = "Unknown", #Sturdevant unknown; Quigley also. both take area of sampling as given. Sturdevant has clustering.
  "Unknown; likely non-probability" = "Non-probability", #Parris should be Non-probability, they're one of the "samples were collected" papers. So is dos Santos
  "UNKNOWN; says random in abstract based on diameter; however in methods random not mentioned and diameter was measured not a stratification measure…" = "Unknown; Stratified",
  
  
  "Non-probability; Convenience based on flier response" = "Non-probability",
  "Non-probability; Expert choice" = "Non-probability",
  
  "Random Systematic sampling" = "Probability",
  "Non-probability; Convenience sampling w/ cluster & strata" = "Non-probability; Clustered; Stratified",
  "Non-probability; Convenience sampling w/cluster design" = "Non-probability; Clustered",
  "Non-probability; Convenience sampling" = "Non-probability",
  "Non-probability" = "Non-probability",
  "Non-probability; Conveneince" = "Non-probability",
  "Non-probability; Convenience" = "Non-probability",
  "Non-probability; convenience sample" = "Non-probability",
  "Non-probability and probability; convenience for site and  " = "Non-probability",
  "Convenience sample; Census" = "Non-probability",
  "Non-proability sampling" = "Non-probability",
  "Non-probability; likely conveneince?" = "Non-probability",
  "Transects Non-probability; plots stratified random sample" = "Non-probability",
  "Non-probability sample" = "Non-probability",
  "Probability; Random stratified sample" = "Probability; Stratified",
  "Probability; random dual-density tessellation stratified design" = "Probability; Stratified",
  "Probability; stratified random sample; modified to make it more evenly distributed spatially" = "Probability; Stratified",
  "non-probability; cluster" = "Probability; Clustered", #"Non-probability; Clustered", 
  "stratified random; with access constraints" = "Probability; Stratified",
  "non-probability cluster sampling; some stratification" = "Non-probability; Clustered; Stratified",
  "non-probability; cluster and stratified" = "Non-probability; Clustered; Stratified",
  "Non-probability; Stratified; cluster?" = "Non-probability; Clustered; Stratified",
  "Non-probability; maybe stratified" = "Non-probability; Stratified",
  "Non-probability; cluster; stratified" = "Non-probability; Clustered; Stratified",
  "Probability; stratified" = "Probability; Stratified",
  "Non-probability; stratified" = "Non-probability; Stratified",
  "Stratified; Census of all parks meeting study criteria" = "Census; Stratified",
  "Non-probability; Stratified" = "Non-probability; Stratified",
  "Non-probability stratified" = "Non-probability; Stratified",
  "Non-probability; not reported"  = "Non-probability",
  "Non-probability sampling, systematic cluster sampling" = "Probability; Clustered", # Apparenlty systematic sampling is considered probability sampling.
  "non-probability ; stratified sampling" = "Non-probability; Stratified",
  "probability; hierarchical stratified random sampling; strata go city -> land use -> pond characteristics. I consider land use and pond characteristics strata w/in city" = "Probability; Stratified",
  "Non-probability cluster sampling" = "Non-probability; Clustered",
  "Non-probability ; stratified" = "Non-probability; Stratified",
  "Non-probability sampling" = "Non-probability",
  "probability; stratified; cluster; non-probability" = "Probability; Clustered; Stratified",
  "non-probability" = "Non-probability",
  "Stratified; non-probability; note that researchers call this stratified sampling but it seems more like cluster sampling" = "Probability; Stratified",
  "Probability; Non-probability; cluster sampling; stratified sampling" = "Probability; Clustered; Stratified",
  "Non-probability likely; not reported" = "Unknown",
  "non-probability cluster;used for bird and tree surveys" = "Non-probability; Clustered",
  "census of all habitat fragments in region" = "Census",
  "probability; stratified; cluster" = "Probability; Clustered; Stratified",
  "Non-probability stratified " = "Non-probability; Stratified",
  "Non-probability?" = "Non-probability",
  "Non-probability; post stratified" = "Non-probability",
  "non-probability stratified" = "Non-probability; Stratified",
  "non-probability; stratified" = "Non-probability; Stratified",
  "Non-probability; not strat but multiple areas within the study site were examined as different parts of this study." = "Non-probability",
  "Census of all houses" = "Census",
  "Non-probability; wetland of particular significance" = "Non-probability",
  "Non-probability; method of chosing is not reported." = "Non-probability",
  "Non-probability; though not reported" = "Non-probability; Stratified",
  "Probability; Non-probaility" = "Probability",
  "Non-probability cluster" = "Non-probability; Clustered",
  "random stratified sample" = "Probability; Stratified",
  "Non-probability; though WHAT has stratified?" = "Non-probability",
  "stratified random sampling; note though that sample and population not well defined" = "Probability; Stratified",
  "Non-probability; cluster; strata" = "Non-probability; Clustered; Stratified",
  "random sampling; random stratified" = "Probability; Stratified",
  "stratification,clustering, random selection" = "Probability; Clustered; Stratified",
  "non-probabilitic; systematic sampling; cluster sampling" = "Probability; Clustered",
  "non-probability; stratified; cluster" = "Non-probability; Clustered; Stratified",
  "Non-probability; stratified;cluster" = "Non-probability; Clustered; Stratified",
  "Non-probabilistic; cluster" = "Non-probability; Clustered",
  "Random stratified sample" = "Probability; Stratified",
  "non-probability; randomized design for tree planting with 2 treatments; cluster " = "Non-probability; Clustered",
  "non-probability cluster" = "Non-probability; Clustered",
  "Non-probability purposive sample" = "Non-probability",
  "probability; stratified" = "Probability; Stratified",
  "non-probability cluster sampling" = "Non-probability; Clustered",
  "non-probability based on previous study" = "Non-probability",
  "non probability stratified" = "Non-probability; Stratified",
  "non-probability; cluster; stratified" = "Non-probability; Clustered; Stratified",
  "Non-probability; cluster" = "Non-probability; Clustered",
  "non-probability?" = "Non-probability",
  "stratified; probability; non-probability" = "Probability; Stratified",
  "Not reported; may be in another paper" = "Unknown",
  "Non-probability; unknown"  = "Non-probability",
  "Census of all sites holding water" = "Census"
)


Q31_table <- table(Q31)
WHERE_data_KD_clean$Q31 <- Q31
WHERE_data_KD_clean$Q31[WHERE_data_KD_clean$Q7 == "URBECO-STU-2002"] <- "Unknown; Clustered"
WHERE_data_KD_clean$Q31[WHERE_data_KD_clean$Q7 == "BIOCON-FIC-2004"] <- "Unknown"



## Of projects that were Non-probability, which ones described their
## non-probability sampling and which just said "XXX was sampled"?

#	Non-probability sampling included convenience sampling, such as sites the
#	authors could easily get access to, or were chosen due to ‘expert’ assessment.
#	Many papers did not report their method of site selection; in one extreme case
#	the abstract mentioned random stratified sampling based on tree diameter,
#	however neither the method for site selection nor even measuring tree diameter
#	were mentioned in the methods section.

Q32 <- WHERE_data_KD$Q32

Q32 <- recode(
  Q32,
  "Expert opinion" = "Expert choice",
  "Likely convenience or purposive sample of different habitat strata" = "Expert choice",
  "no mention of study site choice was made." = "Not reported",
  "Likely convenience sample; other than size and separation constraints no information on chosing study sites is given." = "Convenience sample",
  "Likely convenience or purposive sample of the different habitat types; wetlands used to locate turtles and then turtle range defined the unit of analysis." = "Convenience sample",
  "Fliers put up in neighborhoods to recruit cats." = "Convenience sample",
  "Expert choice of location within city?" = "Expert choice",
  "Expert choice of four clustered sampling sites" = "Expert choice",
  "Expert choice." = "Expert choice",
  "Expert choice?" = "Expert choice",
  "Area demarcated by vaccine treatment. area was defined by treatments prior to the experiment, I think" = "Convenience sample",
  "Trees taken from campus based on access and permission to harvest; trees taken from town and office development based on opt-in survey & expert choice" = "Convenience sample",
  "Researchers  picked the landfill, then an area on the old landfill, then used a two by two factorial design within the area of the old landfill." = "Expert choice",
  "May be a census of all forest patches (then broken up into habitat patches) in the area, but no idea why this research area was chosen." = "Not reported",
  "Catchments were placed into categories based on the range of available urbanization intensities and a sample of study streams from each category was made using a filtering algorithm based on proximity to the St. Johns River (i.e., only nontidal sites were selected) and road access. ... A stream reach (100 m) was selected in each catchment. Reaches were selected based on road access." = "Convenience sample",
  "na" = "NA",
  "systematic sampling based on distance (3 and 6 km) intervals from city center along cardinal direction transects" = "NA",
  "not reported" = "Not reported",
  "Nothing other than \"we used this area\" mentioned." = "Not reported",
  "\"Trees were measured in two separate sections of the Arboretum, which we will refer to as site A and site B.\" Nothing else is said." = "Not reported",
  "\"We established two transects across the study area\n(Fig. 1a). Both transects extend westward from the City of Boston, but demonstrate contrasting patterns of development.\" ; \"We used this stratified random sampling scheme to ensure that all land use classes and intensities of urban development would be adequately sampled\"" = "Not reported",
  "Authors chose three parks for unknown reasons; then chose fourty sampling sites." = "Expert choice",
  "Chose 53 sites, got permission from 21; then went door to door to get an additional 5" = "Expert choice",
  "chose old green roofs that authors could get access to" = "Convenience sample",
  "not described, just \"…was sampled at seven different sites\"" = "Not reported",
  "just says sites were chosen" = "Not reported",
  "just says samples were collected" = "Not reported",
  "just says there are three sites" = "Not reported",
  "sites were selected'" = "Not reported",
  "Authors selected sites fitting certain characteristics, no information on how they were chosen." = "Expert choice",
  "From parks of specific size, income filter was applied to reduce pool. Of that pool all but 4 were sampled; those 4 couldn't be visited for misc. reasons." = "NA",
  "Just \"we sampled\"" = "Not reported",
  "Says \"we collected digital recordings… at 47 lentic water bodies… with varying exposure to traffic noise\"" = "Expert choice",
  "\"We identified 58 study sites in narrow strips of roadside vegetation… using aerial photos and digital map of roads\"" = "Expert choice",
  "\"7 sites were surveyd… the number of sites was limited by availability of suitable areas and logistic constraints\"" = "Not reported",
  "\"we selected 8 sites…\"" = "Not reported",
  "Just states that one landfill was sampled" = "Not reported",
  "Used expert knowledge and site visits to find high use sites. Randomly chose stream direction to get paired low site" = "Expert choice",
  "\"sampling design was based on the representativeness of different geological materials and soil orders throughout the bay area\"" = "Expert choice",
  "No information given, just mentions study areas" = "Not reported",
  "Likely convenience sample" = "Not reported",
  "Streams chosen to maximize differences in catchment land use" = "Expert choice",
  "field sites were identified" = "Not reported",
  "sites chosen based on specific criteria" = "Expert choice",
  "\"this study was conducted in a 25m long reach of the river\"" = "Not reported",
  "Site where parks department let them change sidewalk" = "Convenience sample",
  "chose three neighborhoods similar but different light regimes" = "Expert choice",
  "sites selected on size and landscape characteristics" = "Expert choice",
  "which patches were accessible and had landowner permission" = "Convenience sample",
  "parks with variety of different areas, parks without connection to other parks" = "Expert choice",
  "\"opportunistic plots were also sampled as encountered throughout the study region\"" = "Convenience sample",
  "location for apiary chosen" = "Not reported",
  "unknown; just says bee foragers were sampled" = "Not reported",
  "gardens intentionally chosen to reflect a gradient in landscape diversity and intensity across the central coast." = "Expert choice",
  "sites were chosen based on household/ha and built cover variables" = "Expert choice",
  "chose two locations within each grid cell" = "Expert choice",
  "just says 'we estimated… for lawns of 6 homes'." = "Not reported",
  "All residential streets in study area" = "Expert choice",
  "chosen based on characteristics above and to have a high degree of similarity in habitat availablity and composition." = "Expert choice",
  "just sites chosen. No details. " = "Not reported",
  "just says data concerning 135 sampling sites were collected…" = "Not reported",
  "from all pocket parks, used criteria to get to final set" = "Expert choice",
  "just says 36 sites chosen" = "Not reported",
  "selected from known breeding sites matching criteria to maximize variation in wetland type and based on access" = "Expert choice",
  "used a 'survey gap-analysis' procedure… plus added another 40 or so sites by unknown means" = "Expert choice",
  "just sites selected" = "Not reported",
  "Chose all mixed hardwood stands larger than 1 ha, looks to be census of sampling frame" = "Expert choice",
  "Just says area studied" = "Not reported",
  "authors chose hedges meeting criteria" = "Expert choice",
  "just says neighborhoods provided data" = "Not reported",
  "just says sampling occurred in residential suburb" = "Not reported",
  "these two sites were chosen based on ecological conditions present" = "Expert choice",
  "Census of all houses" = "NA",
  "just describes wetland chosen. " = "Not reported",
  "just says that the plots were established. Don’t know how they located the plots for the balanced experimental design" = "Not reported",
  "No description given; just that the 15 nests were found." = "Not reported",
  "just says they placed the nests" = "Not reported",
  "Second phase used advertisements for gardens with skinks" = "Convenience sample",
  "just says they identified properties" = "Not reported",
  "selected based on mainstream identiy, degree of urbanization, and landscape characterization" = "Expert choice",
  "just says that courtyard was investigated" = "Not reported",
  "Just says sites were selected" = "Not reported",
  "Just sites were selected" = "Not reported",
  "web-research and snowball techniques" = "Convenience sample",
  "selected to represent a range of ages but maintain consistent design specifications" = "Expert choice",
  "Site chosen presumably because researchers could grow & harvest trees present" = "Convenience sample",
  "One is convenience sample of public reported trees, one is trees on main streets" = "Convenience sample",
  "just says trees were planted at…" = "Not reported",
  "Just says that trees were pruned in various ways doesn't discuss the where…" = "Not reported",
  "selected a green within golfcourse with history of issues" = "Expert choice",
  "just says 'the study was located in four large'…" = "Not reported",
  "just says streets selected; sample plots marked" = "Not reported",
  "just says trees were chosen" = "Not reported",
  "systematic sampling (sampling sites and line transects); strata reported but they are post-hoc as best I can tell" = "Expert choice", # transect placement is expert choice and then following is a systematic sample for clusters...
  "just says 50x6 plants planted along a road" = "Not reported",
  "sites selected from places meeting author's selection criteria" = "Expert choice",
  "sites selected" = "Expert choice",
  "just says areas were sampled" = "Not reported",
  "UNKNOWN" = "Not reported",
  "just says 10 parks chosen based on 'representative' characteristics" = "Not reported",
  "two sites chosen; no reason why given" = "Not reported",
  "used site characteristics to narrow down, not sure how they chose from within that pool. Or if they identified the entire pool first or just found sites " = "Not reported",
  "no detail on site selection given" = "Not reported",
  "Area chosen b/c monk parakeets live here" = "Expert choice",
  "Unknown; just says \"were sampled\"" = "Not reported",
  "just gives details about three reaches, not how they were chosen." = "Not reported",
  "in acknowledgements notes person who helped authors determine which 9 ponds to use" = "Expert choice",
  "real estate agents suggested neghborhoods; they puled all home sales in thse areas" = "Expert choice",
  "just 'sites were sampled'. " = "Not reported",
  "garden sites identified with help of Extension service" = "Expert choice",
  "six parks were selected" = "Not reported",
  "based on previous study area selected exactly" = "Expert choice",
  "sites… were selected'" = "Not reported",
  "just says study took place at this green roof" = "Not reported",
  "garden owners recruited through various means" = "Convenience sample",
  "just say sites were surveyed; 'representative of different land uses'" = "Not reported",
  "No information given" = "Not reported",
  "just says bird census were conducted at 30 greenspaces" = "Not reported",
  "based on bus routes, truck routes, traffic volumes, cover peninsula" = "Expert choice",
  "sites chosen based on aerial photographs previously classified by forest type, height, and crown density" = "Expert choice",
  "Just says site was chosen, possibly because the author lived there." = "Not reported",
  "Authors chose 3 suburbs of different ages" = "Expert choice",
  "Chose 23 tree species--represented by at least 16 individuals." = "Expert choice",
  "author and coworkers examined plots drawn from a wide range of neighborhoods" = "Expert choice",
  "chose five trees from the species but do not mention how" = "Not reported",
  "just says trees were sampled" = "Not reported",
  "sites were selected based on multiple criteria" = "Expert choice",
  "landscapes chosen; forests surveyed when found on aerial maps but not clear if this is a census" = "Expert choice",
  "Combination of re-sampling sites examined in the 1990s and selection of new sites. For new sites just says \"we selected a larger array…\"" = "Not reported",
  "Just says \"I surveyed…\"" = "Not reported",
  "Authors chose plots within the sites… which they also chose without anything other than \"we observed birds at\"" = "Not reported",
  "just says 'we selected'" = "Not reported",
  "all sites authors knew of" = "Expert choice",
  "all HOAs within CAP LTER Phoenix sites." = "Expert choice" # this is a weird one, think it is expert choice of long term CAP LTER sites fitting specific conditions (data, HOA status, ???). "All HOA" may or may not be correct on additional readthrough
)

Q32[Q32 == "NA"] <- NA
Q32[grep("^Probability|Unknown", Q31, TRUE)] <- NA
Q32_table <- table(Q32)
WHERE_data_KD_clean$Q32 <- Q32


## THIS IS THE STRATIFIED SAMPLING QUESTIONS

## Q33 How many strata for stratified samples.

Q33 <- WHERE_data_KD$Q33
Q33 <- recode(
  Q33,
  "na" = "NA",
  "4; 3 groups originally, one group split in two" = "4",
  "Unknown" = "Not reported",
  "not reported" = "Not reported",
  "2?" = "2",
  "unknown" = "Not reported",
  "3,4" = "4",
  "stratified after sampling, I believe" = "NA",
  "5?" = "5",
  "two cities only; not reported" = "Not reported",
  "3;?" = "3",
  "2; treatments" = "2"
)

Q33[Q33 == "NA"] <- NA
Q33_table <- table(Q33)
WHERE_data_KD_clean$Q33 <- Q33
sum(!is.na(Q33))


# Q34 How many sites in each strata.

Q34 <- WHERE_data_KD$Q34
Q34 <- recode(
  Q34,
  "Not Reported" = "Not reported",
  "na" = "NA",
  "Unknown" = "Not reported",
  "not reported" = "Not reported",
  "not reported?" = "Not reported",
  "not well reported; 8 disturbed but that is only n reported" = "Uncertain",
  "not sure" = "Not reported",
  "two cities only; not reported" = "Not reported",
  "2?" = "Uncertain",
  "not reported; 6" = "Not reported"
)

Q34[Q34 == "NA"] <- NA
Q34[Q33 == "Not reported" & is.na(Q34)]<- "Not reported"
Q33[Q34 == "Not reported" & is.na(Q33)]<- "Not reported"

Q34_table <- table(Q34)
WHERE_data_KD_clean$Q34 <- Q34

## Q35 Which gradient defined the strata?

Q35 <- WHERE_data_KD$Q35
Q35 <- recode(
  Q35,
  "na" = "NA",
  "two cities only; not reported" = "Not reported"
)

Q35[Q35 == "NA"] <- NA
Q35[Q33 == "Not reported" & is.na(Q35)]<- "Not reported"

Q35_table <- table(Q35)
WHERE_data_KD_clean$Q35 <- Q35

## THIS IS THE CLUSTER SAMPLING QUESTIONS

## Q36 How many clusters

Q36 <- WHERE_data_KD$Q36

Q36 <- recode(
  Q36,
  "na" = "NA",
  "6?" = "6",
  "Unknown" = "Not reported",
  "4 marshes" = "4",
  "9 high level" = "9",
  "3; 8" = "3"
)

Q36[Q36 == "NA"] <- NA
Q36_table <- table(Q36)
WHERE_data_KD_clean$Q36 <- Q36

# Q37 How are clusters defined?

Q37 <- WHERE_data_KD$Q37

Q37 <- recode(
  Q37,
  "na" = "NA",
  "site'" = "site",
  "City" = "city"
)

Q37[Q37 == "NA"] <- NA
Q37[Q36 == "Not reported" & is.na(Q37)]<- "Not reported"
Q37_table <- table(Q37)
WHERE_data_KD_clean$Q37 <- Q37

# Q38 Number of units in each cluster

Q38 <- WHERE_data_KD$Q38

Q38 <- recode(
  Q38,
  "na" = "NA",
  "unknown; total of 139" = "Not reported",
  "not reported" = "Not reported",
  "unknown; not sure how they got 466 samples from the 130 plots…" = "Not reported",
  "unknown; total of 210, 140, and 140" = "Not reported",
  "unknown" = "Not reported",
  "not reported; total 646" = "Not reported",
  "1? Or 6?" = "Unclear",
  "40 or 50, unclear" = "Unclear"
)

Q38[Q38 == "NA"] <- NA
Q38[Q36 == "Not reported" & is.na(Q38)]<- "Not reported"
Q38_table <- table(Q38)
WHERE_data_KD_clean$Q38 <- Q38

# May have missed stratified/cluster in Q31, so need to add that in if 33-35 or 36-38 being filled in.

Q31 <- ifelse((!is.na(Q33) | !is.na(Q34) | !is.na(Q35)) & !grepl("Strat",Q31,T),
       paste0(Q31,"; ", "Stratified"),
       Q31
)       

Q31 <- ifelse((!is.na(Q36) | !is.na(Q37) | !is.na(Q38)) & !grepl("Clust",Q31,T),
       paste0(Q31,"; ", "Clustered"),
       Q31
)

WHERE_data_KD_clean$Q31 <- Q31
Q31_table <- table(Q31)


## Q39 what is the sample unit of ANALYSIS

Q39 <- WHERE_data_KD$Q39

Q39 <- recode(
  Q39,
  "patch   " = "patch",
  "plot? Not sure" = "Unsure"
)

Q39 <- ifelse(grepl("\\?",Q39,T),
              "Unsure",
              Q39)

WHERE_data_KD_clean$Q39 <- Q39

# Q40 Is sample unit of observation different than sample unit of analysis?

Q40 <- WHERE_data_KD$Q40

Q40 <- recode(
  Q40,
  "no; both general habitat types" = "No",
  "no" = "No",
  "No; they are the same" = "No",
  "N" = "No",
  "no; though spatial reach of the bat detector isn't known…" = "No",
  "no; large area where nests were searched" = "No",
  "No; they do a good job of establishing plots and then using those as analysis units" = "No",
  "no?" = "No",
  "no? but also discusses cost" = "No",
  
  ## For the yes', one common reason was because a sample point was used to
  ## represent the whole plot in analysis. Others use post hoc stratification
  
  "yes" = "Yes",
  "Y" = "Yes",
  "Yes; grid placed based on river location so that is at least described" = "Yes",
  "Yes; Mixed effects models done for patches, not sites (2 x per patch)" = "Yes",
  "Yes; samples pooled across sites" = "Yes",
  "Yes; takes sample homes and extrapolates citywide." = "Yes",
  "yes; samples taken for site" = "Yes",
  "yes; substitutes measurement area for site" = "Yes",
  "Yes; what transects are taken as measure for garden" = "Yes",
  "yes; points for woodland" = "Yes",
  "yes; transect used as substitute for neighborhood." = "Yes",
  "Yes; transect used for park" = "Yes",
  "Yes; 10 transects/1 ha plot/suburb for birds; 20 continguous 1 ha plots per suburb" = "Yes",
  "yes; takes 50m radius for park" = "Yes",
  "Yes, aggregates to good vs bad areas," = "Yes",
  "yes; pooled all data" = "Yes",
  "Yes; land use is new" = "Yes",
  "Yes; use plots for sites" = "Yes",
  "yes but repeated measures design" = "Yes",
  "Yes; plots vs whole site" = "Yes",
  
  ## Subset of the mismatches are due to aggregation. ie. taking multiple plots as the sample.
  "Yes; aggregate to park" = "Yes, due to aggregation",
  "Yes; some aggregated to land use" = "Yes, due to aggregation",
  "yes; aggregated to treatment" = "Yes, due to aggregation",
  "Yes; but its because of clustering" = "Yes, due to aggregation",
  "No; just un-clustered" = "Yes, due to aggregation",
  "Y; however due to cluster design" = "Yes, due to aggregation",
  "yes; created clusters from sampling units using heirarchical clustering" = "Yes, due to aggregation",
  "Yes, trees aggregated to tree species" = "Yes, due to aggregation",
  "yes tree aggregated to tree species" = "Yes, due to aggregation",
  "No; though they do aggregate unit of observation for some analyses." = "Yes, due to aggregation",
  
  "Partly; depends on analysis" = "Partly, depends on analysis",
  "partly; summed results for the three suburban habitats and treated them as one" = "Partly, depends on analysis",
  "Partly." = "Partly, depends on analysis",
  "Partly; transect for plot for veg?" = "Partly, depends on analysis",
  "Partly; seem to aggreagate forest to cell level." = "Partly, depends on analysis",
  "No; but note fourth neighborhood for birds (bush) not evaluated." = "Partly, depends on analysis",
  
  "maybe" = "Uncertain",
  "Maybe; different measurements taken at sampling site and at different scales…" = "Uncertain",
  "confusing, think this is different and it has been aggregated to site type" = "Uncertain",
  "Maybe?" = "Uncertain",
  "Yes?" = "Uncertain",
  "yes?" = "Uncertain",
  "Unknown" = "Uncertain"
       )
Q40_table <- table(Q40)
WHERE_data_KD_clean$Q40 <- Q40
  
remove(Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40)

## --------- DATA COLLECTION METHODS (Questions 41-42) ------------ 

Q41 <- WHERE_data_KD$Q41
Q41 <- recode(
  Q41,
  "Radio collars; photos; " = "radio collars; photos",
  "trapping; " = "trapping",
  "branch harvest, " = "branch harvest",
  "Mist netting" = "mist netting",
  "??? Doesn’t say how they got the data!!" = "Not reported",
  "Nest survey; gis analysis" = "nest survey; GIS analysis",
  "Tree measurements" = "tree measurements",
  "Vegetation surveys; bird point count" = "vegetationsurveys; bird point count",
  "Systematic search of all nests" = "systematic search of all nests",
  "Surveys and observation of wasp nests" = "surveys and observation of wasp nests",
  "Mostly GIS analysis; forest tract surveys" = "mostly GIS analysis; forest tract surveys",
  "Surveys for ant nests" = "surveys for ant nests"
  
)

WHERE_data_KD_clean$Q41 <- Q41

## Were GIS data used in Site Selection? 

Q42 <- WHERE_data_KD$Q42
Q42 <- recode(
  Q42,
  "no" = "No",
  "N" = "No",
  "No? but maps were used" = "No",
  "No; traffic data used though" = "No",
  "No; though possibly aerial photos" = "No",
  
  "Yes; aerial photographs" = "Yes",
  "Y" = "Yes",
  "y" = "Yes",
  "yes" = "Yes",
  "Yes; aerial photos" = "Yes",
  "Yes (google maps named)" = "Yes",
  "GIS analysos completed for part" = "Yes",
  "yes; site age" = "Yes",
  "Yes; well aerial photos, no GIS at the time" = "Yes",
  
  "Unknown" = "Uncertain",
  "unknown" = "Uncertain",
  "Unknown; but I think so?" = "Uncertain",
  "Unknown but I don't think so" = "Uncertain",
  "Unknown but probably" = "Uncertain",
  "Unknown; but definitely in analysis" = "Uncertain",
  "unknown but presumably" = "Uncertain",
  "Yes?" = "Uncertain",
  "yes?" = "Uncertain",
  "Unknown; but probably" = "Uncertain",
  "Maybe; tree database used" = "Uncertain"
)

Q42_table <- table(Q42)
WHERE_data_KD_clean$Q42 <- Q42

remove(Q41,Q42)

## ----------- SUMMARY QUESTIONS (Q43-Q50) ----------------------

# These questions are useful for evaluating the paper's ability to be replicated
# as well as the quality of the sampling design and reporting of the sampling
# design

# Q43 Is sampling design (WHERE) well described in methods?

Q43 <- WHERE_data_KD$Q43
Q43 <- recode(
  Q43,
  "No; site location choice not well justified" = "No",
  "No; references another study that also doesn't give information" = "No",
  "No; key selection steps omitted." = "No",
  "n" = "No",
  "N; not sure how authors spaced 6km?" = "No",
  "N" = "No",
  "No; don't know why this landfill or why this transect" = "No",
  "no; urbanization gradient isnt mentioned until the discussion" = "No",
  "No; also Mostly a desciptive study" = "No",
  "no" = "No",
  "No; relies on interpretation of multiple author decisions" = "No",
  "No; popultion etc not defined" = "No",
  "No; sample frame poorly descriped and population is not described" = "No",
  "No; however this is proof of concept" = "No",
  "NO" = "No",
  "No; no information on the relationship between sampling sites and parks" = "No",
  "No; missing info on site selection" = "No",
  "No; confusion about trees" = "No",
  "No " = "No",
  "No; though they do describe requirements for selection well." = "No",
  
  "Yes; though note LIMITED sampling design is pick an area, find cougars." = "Yes",
  "Y" = "Yes",
  
  "Yes, however it's reported in a separate paper. This paper does not describe sampling design" = "Yes, though reported in a separate paper",
  "Yes; though most is reported in other papers" = "Yes, though reported in a separate paper",
  "Yes; used same area as previous study" = "Yes, though reported in a separate paper",
  "Yes; described in another paper" = "Yes, though reported in a separate paper",
  
  "Partly, they used two sites (a paired sample) based on key characterisitscs." = "No",
  "Mostly; doesn't mention what dataset what used to generate random sampling points" = "No",
  "Maybe; it is very complex and missing some details e.g. does not mention all details for cluster samples" = "No",
  "Maybe; uncertainty about how many trees in each treatment in each location " = "No",
  "P" = "No",
  "partly" = "No",
  "Yes?" = "Yes",
  
  "na; where sampling didn't really happen" = "No" # these were trees harvested from a specific site
  
)

Q43_table <- table(Q43)
WHERE_data_KD_clean$Q43 <- Q43

# Q44 Could study site (WHERE) selection be reproduced or replicated?

Q44 <- WHERE_data_KD$Q44
Q44[rownames(WHERE_data_KD) == "JOUE-BOU-2017-KD"] <- "N" # This is a typo, checked paper.
Q44[rownames(WHERE_data_KD) == "JOUE-OW-2017-KD"]	<- "Same location" # ditto


Q44 <- recode(
  Q44,
                                                                                                                             
  "No; neighborhood vs. edge habitat distinction is very confusing" = "No",
  "No; no description of selection criteria or process" = "No",
  "No, not sure why old dump was chosen or why  150x400m area was placed where it was within larger area." = "No",
  "No; not clear why study area was chosen. Census of forests is ok." = "No",
  "No; doesn't mention what the categories based on available urbanization intensities were" = "No",
  "No; don't know why these 67 of all possible were studied" = "No",
  "n" = "No",
  "No; not clear why study area was chosen." = "No",
  "No; not clear why these areas of Arboretum were chosen or how they differed from others" = "No",
  "No; not clear why these three parks were chosen or why specific trees were chosen" = "No",
  "No; Don't know how they chose the 66 addresses from the ~600 surveys" = "No",
  "No; don't know how random stratified design was carried out." = "No",
  "N" = "No",
  "N; largely due to the 'random' selection of schools. Unclear what sampling frame is." = "No",
  "No; particularly with site/tree confusion" = "No",
  "no" = "No",
  "No " = "No",
  "N; this is due to opportunistic sampling" = "No",
  "no; this is a particularly bad paper" = "No",
  "No; don’t know if belts are on the same side of the road or different sides or in the center??? Very confusing" = "No",
  "Probably not; but maybe if locations known…" = "No",
  "No; I have no idea what the study design is." = "No", # hahaha
  "No; contradicting information given" = "No",
  "No; no information on the relationship between sampling sites and parks" = "No",
  "Would be yes--but they don't describe what is considered urban or intermediate" = "No",
  "No; doesn’t give enough detail about why sites were chosen" = "No",
  "No; possibly reproduced if transect sites are documented somewhere." = "No", # this means duplicated
  "No; the choice of sites is still the problem, and unknown sample frame" = "No",
  "No; sample population information missing" = "No",
  "No; however this is proof of concept" = "No",
  "No; have site limitations but don't know study population or how sites were chosen from the (presumably many) sitest that fit the bill." = "No",
  "No; plot locations iwthin sites not known" = "No",
  "Replicated only; though no information about how they collected front yard data." = "No", # Don't mention how data collected so cant be replicated.
  "Possibly; but samples might be taken from different distribution" = "No", 
  "Possibly. " = "No", # Requires additional data
  "Maybe; the key issue is not knowing how sites were selected. " = "No",
  "Mostly; doesn't mention what dataset what used to generate random sampling points" = "No",
  "Maybe; uncertainty about how many trees in each treatment in each location " = "No",
  "Partly; don’t describe how sites were chosen within neighborhoods…" = "No",
  "Replication only, though transect location isn't known" = "No", #IDK why i put replication, this is not clear
  "partly; choice of sites is opaque but good information on difference in housing etc." = "No",
  "Possibly; depends on sampling methods reported in other papers" = "No", #only what sampling in other pap
  
  "Yes?" = "Yes", # This was cougar study & ospreys
  "Yes; described in another paper" = "Yes",
  "Y" = "Yes",
  "Yes; because all sites over 1 ha used. Main question is classifying mixed hardwood forest." = "Yes",
  "Yes; though sampled settlement types may not be the same in other locations…" = "Yes",
  "Yes; census so thus can be reproduced." = "Yes",
  "Yes; though only in same areas given industry collaboration" = "Yes",
  "Yes; used same area as previous study" = "Yes",
  "Yes; with some differences in definition. They are pretty well described though" = "Yes",
  "Yes, however it's reported in a separate paper. This paper does not describe sampling design" = "Yes",
  "Yes; provided description of selection criteria (land use, soil type, across city) thus with that data could do something similar" = "Yes",
  "Yes; though only in same areas given industry collaboration" = "Yes", # area means cities here. convenience sample reliant on industry collab.
  "Replicated; only concern is making sure definition of riparian patch is consistent" = "Yes", ## probability
  "Maybe; just pick a park and can recreate analysis…" = "Yes", # park not chosen for a reason, so could be replicated at another park... might not replicate results though haha
  "Y; unsure of how to determine plot number per site" = "Yes",
  
  ## I confused myself using different terms for study replication. Basically, I
  ## wanted to distinguish between studies that could be replicated in another
  ## location or in the same location with a different sample from studies that
  ## could only be duplicated at the same sites (site names given, but not how
  ## those sites were chosen). This recoding is correct & has been checked against the papers.
  
  ## Following are ones that can only be DUPLICATED with the same sites.
  "Replicated only because name of parks surveyed is given." = "Duplicated at same sites only",
  "No; however replication possible b/c site names are in acknowledgements" = "Duplicated at same sites only",
  "Yes; only because it was limited to one area where the trapping program was in place" = "Duplicated at same sites only",
  "Yes; because only one named site surveyed, can reproduce study" = "Duplicated at same sites only",
  "No; what sampling could be replicated at this landfill though" = "Duplicated at same sites only",
  "Likely reproduced but not replicated?" = "Duplicated at same sites only", # map shows where sampling occured
  "Yes; reproduction only" = "Duplicated at same sites only",
  "yes; reproduced only" = "Duplicated at same sites only",
  "Reproduced only" = "Duplicated at same sites only",
  "Replicated only as two sites are explicitly named" = "Duplicated at same sites only",
  "Replicated only because exact location given" = "Duplicated at same sites only",
  "Yes; census of same areas could be reproduced" = "Duplicated at same sites only",
  "Yes; methods are very simple so not hard to repro" = "Duplicated at same sites only",
  "na; where sampling didn't really happen" = "Duplicated at same sites only", # these were trees harvested from a specific site.
  "Partly; only along same gradients." = "Duplicated at same sites only", # gradients aren't well defined
  "Same location" = "Duplicated at same sites only"
)

Q44_table <- table(Q44)
WHERE_data_KD_clean$Q44 <- Q44

plot(WHERE_data_KD_clean$Q4, as.factor(WHERE_data_KD_clean$Q44))



# Q45 Are limitations of WHERE sampling design addressed?

# Note that there are a number of the 'no's that either should have (ie their
# study design had important flaws) or discussed it for the WHAT and not the
# WHERE

Q45 <- WHERE_data_KD$Q45
Q45 <- recode(
  Q45,
  "No; and they really should be due to university ownership." = "No", # Note should...
  "No; and they should mention issues with " = "No",
  "No; quite the opposite. Says WHERE means they can generalize to whole eastern North America." = "No",
  "No; though they do mention road access?" = "No",
  "n" = "No",
  "N" = "No",                                                                                                         
  "No; though sample point vs. forest patch distinction was made." = "No",
  "no" = "No",
  "not really; though the limitations of variable selection is discussed" = "No",
  "No; WHAT limitations mentioned with observer detection bias" = "No",
  "No; though WHAT sampling design is addressed" = "No",
  "No; however WHAT limitations are" = "No",
  "No; though they do mention limits of WHAT historical documentation" = "No",
  "No; though some mention of WHAT sampling" = "No",
  "Some limitations of WHAT sampling given." = "No",
  "No, though census makes this unnecessary" = "No",
  "No; however this is proof of concept" = "No",
  "N; mention two neighborhoods they discarded but that is it" = "No",
  "no?" = "No",
  
  "Yes; explicitly talk about small sample size" = "Yes",
  "Yes; address sample size limitations" = "Yes",
  "Yes, in terms of sampling design they discuss covariates (specifically geography)." = "Yes",
  "Yes; discuss possible confounding factors and how they were controlled for!" = "Yes",
  "Yes; mentions multiple times low sample size issues" = "Yes",
  "Yes; though mostly about timing" = "Yes",
  "Y" = "Yes",
  "Yes; they mention that size and percent conifer cover are correlated" = "Yes",
  "Yes; does mention limits of chronosequence approach" = "Yes",
  "Yes; mention both limited study sites and time" = "Yes",
  "Yes; that only a few sites measured" = "Yes",
  "Yes; mention lack of control treatment" = "Yes",
  "Yes, mention psuedo replication limits scale of inference citing Hurlbert 1984" = "Yes",
  "Yes; in discussion 'research limitations' section" = "Yes",
  "Yes; no trees in city center so couldn’t sample lichen there" = "Yes",
  
  # Partly papers address some but not all limitations (e.g. spatial but not temporal)
  "Partly; briefly mention 6km sites added to densify sampling network and capture spatial heterogeneity" = "Some, but not all",
  "Partly; discuss issues with length of study" = "Some, but not all",
  "Partly; mention establishing 30 sites to represent wide range of site types" = "Some, but not all",
  "Some; mention that the watershed has some unique characteristics" = "Some, but not all",
  "Partly; they do mention they couldn't sample one type of garden because they weren't built yet" = "Some, but not all",
  "Partly, mention temporal and sampling differences within " = "Some, but not all",
  "Partly, mention that study only happened in the open street environment" = "Some, but not all",
  "Partly; mention need for studies in other locations" = "Some, but not all",
  "Partly; discuss unballanced design" = "Some, but not all",
  "Partly, do mention trying to get the same size site" = "Some, but not all",
  "Partly, mention that gardens sampled are representative (qualitatively)" = "Some, but not all",
  "No; though they do talk about uniqueness of the university setting in a way that addresses some limitations" = "Some, but not all"
)

Q45_table <- table(Q45)
WHERE_data_KD_clean$Q45 <- Q45

# Q46 Are study sites representative of the population?

Q46 <- WHERE_data_KD$Q46
Q46 <- recode(
  Q46,
  "Yes; barring periodic pattern issues" = "Yes",
  "Yes; or at least likely so based on descriptions" = "Yes",
  "Yes; but only if population is 34km study area" = "Yes",
  "popuation and study sites the same; compare with other local areas" = "Yes",
  "Yes; census" = "Yes",
  "Yes?" = "Yes",
  "Yes; though they will not be representative of sites that do not hold water." = "Yes",
  "Y" = "Yes",
  "qualitatively" = "Yes", # though note that this is based on author's reporting.
  "Maybe" = "Yes", #Aerial detected forests which is pretty close samppling frame to popn
  "Partly" = "Yes", # this one uses a grid with random samples, so its potentially a bit more even than landscape but fits for population of grids
  "na; single site is populatin" = "Yes",
  
  "No; university ownership of many plots means they're likely not representative." = "No",
  "No; sites are more homogenous in their neighborhood but this is in service of the research" = "No",
  "No; areas chosen based on ease of access" = "No",
  "No; areas chosen based on another program and by ease of access" = "No",
  "No; due to access bias." = "No",
  "No; just due to size." = "No",
  "One site only; not representative of all green roofs" = "No",
  "Likely not" = "No",
 
  # The unknowns are basically not well enough reported to make a good
  # judgement. Many of these are because the population (not the sample) is not
  # well defined
  " Unknown" = "Unknown",
  "Unknown; don't know what population is." = "Unknown",
  "Unknown; though they did at least use an algorithm which provides some reduction in observer bias." = "Unknown",
  "Unknown; likely significant variation within land use" = "Unknown",
  "unknown " = "Unknown",
  "Unknown; but likely no. possible issues with bias along meridians." = "Unknown",
  "Unknown; but im not as fussed about it as with other papers as they were purposefully looking at the extremes" = "Unknown",
  "Unknown; would be yes except for addition of opportunistic plots, don't know distribution of those" = "Unknown",
  "Unknown; probably not use parks as a standin for all suburban" = "Unknown", # THATS A YIKES
  "unknown" = "Unknown",
  "Unknown; its not clear what population they are part of. The golf course could be part of 'naturaliztic golf courses' but given the relatively unique dunes ecosystem, not sure " = "Unknown",
  "Unknown; population not reported." = "Unknown",
  "Unknown; population isnt well defined" = "Unknown",
  "Unknown; population not well defined" = "Unknown",
  "Unknown; population not defined" = "Unknown",
  "Unknown; don’t really know what population is" = "Unknown",
  "Unknown; however likely not. Looks like most of the transplantation sites were on the university campus which may vary in important ways from other urban areas…" = "Unknown",
  "Unknown; site diversity is built in but population not discussed and sampling frame only obliquely" = "Unknown",
  "unknown?" = "Unknown",
  "Not the popultion of urban wetlands, this seems unique. However they seem only to consider this one so what the population is is uncertain" = "Unknown",
  "Unknown but probably" = "Unknown",
  "Unknown; but probably" = "Unknown", # Not sure why i wrote but probably, neither popn or sample frame are well defined.
  "Unknown; though likely yes" = "Unknown",
  "na; where sampling didn't really happen" = "Unknown" # trees harvested from a site, but popn unknown
  
)

Q46_table <- table(Q46)
WHERE_data_KD_clean$Q46 <- Q46
  

# Q47 Was study site bias from selection mentioned?

Q47 <- WHERE_data_KD$Q47
Q47 <- recode(
  Q47,
  "Partial; vaccine vs not vaccine discussed" = "Partial",
  "Partial; low sample size but doesn’t discuss non-random sampling bias" = "Partial",
  "Partly; discuss elimintating confounding factors by choosing very similar sites." = "Partial",
  "Partly; discussed spatial correlation" = "Partial",
  "Partly; they discuss differences in how urban gradients are quantified, however not how they chose study sites" = "Partial",
  "Partly, mention temporal limits (short study)" = "Partial",
  "Authors mention that method of selecting sites is not sufficient for predicting screening" = "Partial",
  
  "Yes; mention study site selection process used to \"reduce observer bias during site selection\"" = "Yes",
  "Yes; Authors discuss covariates of socioeconomic & social gradients" = "Yes",
  "Yes; great rundown in discussion " = "Yes",
  "Yes; discuss avoiding bias towards public lands" = "Yes",
  "Yes; at least partly discussed in uncertainties section." = "Yes",
  "Yes; good section on uncertainty in measuring and upscaling C pools" = "Yes",
  "Yes; discuss public vs private sampling" = "Yes",
  "Yes!" = "Yes",
  "Yes; mention overabundance of rainforest sites in discussion" = "Yes",
  "Yes; mention lack of control treatment" = "Yes",
  "Yes; discuss space for time substitution" = "Yes",
  "Discuss small sample sizes" = "Yes",
  
  "No; though they seem to think it’s a near census." = "No",
  "no" = "No",
  "Not really, though " = "No",
  "N" = "No",
  "N; however what limitations are discussed" = "No",
  "No; though they do mention different human behavior for this area compared with others" = "No",
  "No; though climactic and growing conditions are mentioned which would impact." = "No",
  "No; though limits of the technique were discussed" = "No",
  "No; however the park studied varied significantly from other parks in Shanghai." = "No",
  "No; but do mention limitations for WHAT sampling" = "No",
  
  "na; census" = "NA",
  "No, though census makes this unnecessary" = "NA",
  "na; single site is populatin" = "NA",
  "No; though this was a census of water holding sites so should not be an issue." = "NA",
  
)

Q47[Q47 == "NA"] <- NA
Q47_table <- table(Q47)
WHERE_data_KD_clean$Q47 <- Q47

# Q48 Do limitations to property access introduce bias in sample site selection?

Q48 <- WHERE_data_KD$Q48
Q48 <- recode(
  Q48,
  "Unknown" = "Not reported",
  "Unknown; but possible. Would need to have different growth between access sites and not access sites; such as the neighborhing trees mentioned that were in better condition." = "Not reported",
  "Unknown; seems like they were able to get access to all forest patches somehow?" = "Not reported",
  "unknown " = "Not reported",
  "Unknown; probably yes though not reported" = "Not reported",
  "Unknown; though it looks like all of the sites were on public property?" = "Not reported",
  "Unknown (not reported)" = "Not reported",
  "Unknown; but if all public property probably not" = "Not reported",
  "Unknown; likely no" = "Not reported",
  "Unknown; but probably not since it seems most occurred on university property" = "Not reported",
  "unknown" = "Not reported",
  "Probably not; though no comparison of yes vs no" = "Not reported",
  "Probably not; but not reported" = "Not reported",
  "Maybe? Don’t know who didn’t respond to the advertisements" = "Not reported",
  "Unknown; they purposefully used public areas which may differ in some way from private areas…" = "Not reported",
  
  "Yes; Residential areas are sampled differently than publically accessible areas!" = "Yes",
  "Yes; only streetsides sampled so results may not be generalizable to residential etc land use." = "Yes",
  "Yes; university property" = "Yes",
  "Yes; road access mentioned as selection criteria. \"A stream reach (100 m) was selected in each catchment. Reaches were selected based on road access.\"" = "Yes",
  "Yes; some of these are mentioned in the Uncertainty section" = "Yes",
  "YES!!; \"Although I aimed to include as many wetlands as possible within 1000 m of the highway, it was not possible to include all wetlands due to constraints on\nsite access; consequently there was a strong positive correlation between the proportion of accessible habitat and native vegetation (rs = 0.81).\"" = "Yes",
  
  "No; dead animals came from multiple sources including people with access to private property" = "No",
  "No; all in one general area so no difference in access." = "No",
  "No; they sampled public and private lands at the same proportion as initial sample. \"to prevent a potential bias towards public lands, for which permissions were easier to obtain, we chose to sample public and private plots in the same proportion as found among the first 15 randomly chosen plots in each category.\"" = "No",
  "No" = "No",
  "Partial; not sure how much of a role accept/reject by homeowners played" = "No",
  "N" = "No",
  "N; permissions for cats may have" = "No",
  "No; though somewhat unknown" = "No",
  "No; all areas were surveyed although access isnt mentioned" = "No",
  "Don't think so, sounds like they were able to visit all sites over 1 acre?" = "No",
  "No, census" = "No",
  "No; only one site studied." = "No",
  "No; only one park and it was surveyed" = "No",
  "No?" = "No"
)

Q48_table <- table(Q48)
WHERE_data_KD_clean$Q48 <- Q48

# Q49 Do the authors mention limits to the generalizability of their study

Q49 <- WHERE_data_KD$Q49
Q49 <- recode(
  Q49,
  "Yes; esp. different species may react differently" = "Yes, generalizes and mentions limits",
  "Yes" = "Yes, generalizes and mentions limits",
  "Yes, generalize and discuss; mention study site limits." = "Yes, generalizes and mentions limits",
  "Yes; lots of great discussion of limitation and what generalization is appropriate" = "Yes, generalizes and mentions limits",
  "y" = "Yes, generalizes and mentions limits",
  "Yes; uncertainty section mentions some including temporal limits" = "Yes, generalizes and mentions limits",
  "Yes; discuss limiatations due to lack of temporal data, discuss generalizability issues between private and public land sampling" = "Yes, generalizes and mentions limits",
  "Yes, mention spatial and temporal limits" = "Yes, generalizes and mentions limits",
  "Yes; mention temporal limits and state cities likely different from one another" = "Yes, generalizes and mentions limits",
  " generalize and discuss; methods includes discussion of assumptions and discuss uncertainties in discussion" = "Yes, generalizes and mentions limits",
  "Yes; authors discuss spatial patterns and limits on generalizability" = "Yes, generalizes and mentions limits",
  "Yes; they don't generalize beyond species and area studied and mention limits on generalizing explicitly!" = "Yes, generalizes and mentions limits",
  "Generalize within PNW (spatial limits)" = "Yes, generalizes and mentions limits",
  "Yes; low sample size issues" = "Yes, generalizes and mentions limits",
  "Y; mention possible issues with extinction debt and time" = "Yes, generalizes and mentions limits",
  "Yes; generalize and partly discuss; mention temporal limits" = "Yes, generalizes and mentions limits",
  "Yes, generalize and mention spatial limits" = "Yes, generalizes and mentions limits",
  "Yes; metnion that it occurred only in one location" = "Yes, generalizes and mentions limits",
  "Yes; mention temporal limits of the study." = "Yes, generalizes and mentions limits",
  "Yes; they mention that their study was correlational which needs furher study and that there are temporal and spatial limitations. " = "Yes, generalizes and mentions limits",
  "Yes; discuss temporal limitations" = "Yes, generalizes and mentions limits",
  "Yes, explicitly mentioned in 'research limitations' section." = "Yes, generalizes and mentions limits",
  "Mention spatial limits, specifically comparing method development locations (North USA) and CA climates" = "Yes, generalizes and mentions limits",
  "Generalize from space-for-time substitution to likely time trends; explicitly mention spatial limits" = "Yes, generalizes and mentions limits",
  "Yes; mention geographic and measurement limitations" = "Yes, generalizes and mentions limits",
  "Don’t generalize except for golf course recommendations, which they do qualify" = "Yes, generalizes and mentions limits",
  "Generally limit to southwestern deserts, which is appropriate" = "Yes, generalizes and mentions limits",
  "Generalize and mention that study is based on one city (spatial limitation)" = "Yes, generalizes and mentions limits",
  
  "Don't generalize outside of study area" = "Study doesn't generalize",
  "Don't generalize" = "Study doesn't generalize",
  "No generalizations made" = "Study doesn't generalize",
  "Don't generalize beyond study region" = "Study doesn't generalize",
  "Don't really generalize" = "Study doesn't generalize",
  "Does not generalize" = "Study doesn't generalize",
  "don't generalize" = "Study doesn't generalize",
  "don't really generalize; mention temporal differences between years re: weather" = "Study doesn't generalize",
  "Don’t really generalize; mention limits for what but not where" = "Study doesn't generalize",
  "don't really generalize, but do mention differences between this study and the one it replicates" = "Study doesn't generalize",
  "Don’t really generalize results" = "Study doesn't generalize",
  "Don't really generalize, but when they do it is to western us; but their scale is huge so I think this is ok" = "Study doesn't generalize",
  "Don't generalize much; do mention temporal limits" = "Study doesn't generalize",
  "Don't really generalize, more compare/contrast with other research" = "Study doesn't generalize",
  "Don't generalize beyond study area" = "Study doesn't generalize",
  "Don't generalize really" = "Study doesn't generalize",
  "Don't generalize really; make statements about urbanizing landscape but using numbers from the study" = "Study doesn't generalize",
  "No limits mentioned, but don't really generalize" = "Study doesn't generalize",
  "no limits mentioned, but don’t really generalize beyond bangkok" = "Study doesn't generalize",
  "Don't really generalize. Comparisons between papers do mention climactic (spatial) differences." = "Study doesn't generalize",
  "Do not generalize, situate in Moscow green studied" = "Study doesn't generalize",
  "No limits mentioned; doesn't generalize really" = "Study doesn't generalize",
  "Doesn't generalize really" = "Study doesn't generalize",
  "Don't really generalize beyond Edgewater" = "Study doesn't generalize",
  "Doesn’'t really generalize" = "Study doesn't generalize",
  "Don't relly generalize except for recommendations." = "Study doesn't generalize",
  "don't generalize beyond study area really" = "Study doesn't generalize",
  "Don't really generalize, discussion is focused on the 65 species seen and how they interacted with habitat." = "Study doesn't generalize",
  "No generalizations made?" = "Study doesn't generalize",
  "Don’t really generalize beyond MA, though they use vague language." = "Study doesn't generalize",
  "Don’t generalize beoyond study area" = "Study doesn't generalize",
  "No generalizations made, however authors compare with other techniques e.g. LiDAR " = "Study doesn't generalize",
  "Don't really generalize; few statements are backed up by addt'l studies" = "Study doesn't generalize",
  "Don't really generlize." = "Study doesn't generalize",
  "Don't generalize beyond sites" = "Study doesn't generalize",
  
  "No" = "No, generalizes but does not mention limits",
  "No; they draw some gerneralizations to arboreal mammals in fragmented urban landscapes broadly" = "No, generalizes but does not mention limits",
  "Generalize but don't discuss" = "No, generalizes but does not mention limits",
  "generalize but don't discuss; talk about wetern US broadly from small area" = "No, generalizes but does not mention limits",
  "generalize but don't discuss;" = "No, generalizes but does not mention limits",
  "No " = "No, generalizes but does not mention limits",
  "generalize but don't discuss" = "No, generalizes but does not mention limits",
  " generalize but don't discuss" = "No, generalizes but does not mention limits",
  "No, generalize and don't mention" = "No, generalizes but does not mention limits",
  "Generalize but don’t discuss" = "No, generalizes but does not mention limits",
  "no" = "No, generalizes but does not mention limits",
  "N" = "No, generalizes but does not mention limits",
  "no; Generalize and don't discuss" = "No, generalizes but does not mention limits",
  "generalize homes to city. " = "No, generalizes but does not mention limits",
  "No, generalize but don't discuss" = "No, generalizes but does not mention limits",
  "Explicitly say in methods that their choice of neighborhood to study means it can be generalized to other urban ecosystems in eastern North America… this is a stretch." = "No, generalizes but does not mention limits",
  "They make some temporal generalization (speaking of possible future events) and then in the last sentences hugely generalize to other animals in urban areas globally…" = "No, generalizes but does not mention limits",
  "Generalize frequently; no limitations made" = "No, generalizes but does not mention limits",
  "no mention of limits" = "No, generalizes but does not mention limits",
  "generalize to the future and to a broader area; but do not discuss limitations" = "No, generalizes but does not mention limits",
  "No; and generalize the patterns they found in categorical vs continuous variables broadly" = "No, generalizes but does not mention limits",
  "generalize but don’t discuss" = "No, generalizes but does not mention limits",
  "they generalize quite a lot; no limits given." = "No, generalizes but does not mention limits",
  "No limits mentioned; however don’t generalize beyond study site until planning implication section" = "No, generalizes but does not mention limits",
  "Generalize to urban areas in Aus" = "No, generalizes but does not mention limits",
  "Mention some unique watershed characteristics; but don’t discuss implications for generalizability" = "No, generalizes but does not mention limits",
  "Seem to generalize to other cities about park size; but do mention the study limits" = "No, generalizes but does not mention limits",
  "Authors generalize only in conclusion; don’t limit" = "No, generalizes but does not mention limits",
  "Generalize without mentioning limits; however temporal projection specific to Baltimore" = "No, generalizes but does not mention limits",
  "Generalize but do not mention any limits" = "No, generalizes but does not mention limits",
  "Generalization mostly limited to 'The West'" = "No, generalizes but does not mention limits",
  "Generalize results to urban areas; no temporal or geographic limitations mentioned but they do mention tree species and size" = "No, generalizes but does not mention limits",
  "I didn't find any limits to their generalizations" = "No, generalizes but does not mention limits",
  "No limits mentioned" = "No, generalizes but does not mention limits",
  "no limits to generalization given" = "No, generalizes but does not mention limits",
  "Generalize, but don't discuss limits" = "No, generalizes but does not mention limits",
  "no limits mentioned" = "No, generalizes but does not mention limits",
  "Don’t mention limits. Some generalization, though they do specifically mention that models from other areas are not generalizable for allometric equations" = "No, generalizes but does not mention limits",
  "generalize to england, which seems reasonable if somewhat unsupported" = "No, generalizes but does not mention limits",
  "No; and generalize to sedum + bees generally..." = "No, generalizes but does not mention limits",
  "last paragraph generalizes without limits" = "No, generalizes but does not mention limits",
  "Generalize to all Florida coyotes. Western FL is different in some ways from eastern or central or southern fl, so this may or may not be valid." = "No, generalizes but does not mention limits",
  "Don't really generalize beyond Australia; however limits not really mentioned beyond issues with chronoseuqnce approach" = "No, generalizes but does not mention limits",
  
  "Generalize and partly discuss" = "Partly, generalizes and mentions some limits",
  "generalize and partly discuss" = "Partly, generalizes and mentions some limits",
  "generalize and partly discuss; mention controling for habitat factors" = "Partly, generalizes and mentions some limits",
  "Partially; they mention issues with avian disperser generalization. Howevre not mentioned with regard to broader restoration generalization" = "Partly, generalizes and mentions some limits",
  "Partial; limit to southeastern US Coastal Plain" = "Partly, generalizes and mentions some limits",
  "Partly; discuss issues with scaling up NPP though don't explicitly discuss temporal and spatial limits" = "Partly, generalizes and mentions some limits",
  "some generalization without limits" = "Partly, generalizes and mentions some limits",
  "Partly; temporal limits but no spatial limits" = "Partly, generalizes and mentions some limits",
  "Yes; generalizes and partly discusses spatial limits" = "Partly, generalizes and mentions some limits",
  "Yes, generalizes and parlty discusses; mention spatial limits related mostly to land cover 30m resolution" = "Partly, generalizes and mentions some limits",
  "P; mention temporal limits in methods" = "Partly, generalizes and mentions some limits",
  "Generalizes and partly discusses" = "Partly, generalizes and mentions some limits",
  "Generalize somewhat; mention temporal limits." = "Partly, generalizes and mentions some limits",
  "Partly, mention that study only happened in the open street environment" = "Partly, generalizes and mentions some limits",
  "Partly; they discuss differences between two years." = "Partly, generalizes and mentions some limits",
  "Partly discuss; mention small sample sizes." = "Partly, generalizes and mentions some limits",
  "Generalize and discuss parlty; spatial limits" = "Partly, generalizes and mentions some limits",
  "generalize to midwest and northeast parks, partly supported by literature." = "Partly, generalizes and mentions some limits",
  
)

Q49_table <- table(Q49)
WHERE_data_KD_clean$Q49 <- Q49

table(WHERE_data_KD_clean$Q49[WHERE_data_KD_clean$Q32 %in% c("Expert choice", "Convenience sample", "Not reported")])



# Q50 Are there any policy/conservation recommendations made beyond the research's scope of generalizability?

Q50 <- WHERE_data_KD$Q50
Q50 <- recode(
  Q50,
  
  # Many papers did not make any policy recommendations
  "No, draws some conclusions about the origin of urban bird species but nothing else" = "No policy recommendations made",
  "No policy suggestions made" = "No policy recommendations made",
  "No, appropriate recommendations made" = "No policy recommendations made",
  "No policy recommendations made" = "No policy recommendations made",
  "No specific policy recommendations made; only that we should manage urban landscape at a wider scale" = "No policy recommendations made",
  "No specific policy recommendations made; only that increased connectivity might enhance natural biological pest control" = "No policy recommendations made",
  "No policy suggestions made; just mention that emissions inventories might underestimate leaf mass…" = "No policy recommendations made",
  "No recommendations made" = "No policy recommendations made",
  "No policy suggestions made; recommendation is for more study" = "No policy recommendations made",
  "no policy recommendations made" = "No policy recommendations made",
  "no recommendations given" = "No policy recommendations made",
  "No clear policy recommendations--just mention need for environmental assessment" = "No policy recommendations made",
  "No policy recommendations made; \"even modest controls on the amount of sprawl may help preserve biodiversity\"" = "No policy recommendations made",
  "No policy recommendtions made" = "No policy recommendations made",
  "No policy recommendations made; just that urban flowers is likely important." = "No policy recommendations made",
  "No policies recommended" = "No policy recommendations made",
  "Mosty discuss how their results agree/disagree with previous conservation recommendations" = "No policy recommendations made",
  "no recommendations made" = "No policy recommendations made",
  "no clear policy recommendations made, just for urban plnners et al to consider the role of urban forests." = "No policy recommendations made",
  "no recommendations made." = "No policy recommendations made",
  "No policy recommendations made." = "No policy recommendations made",
  "no policy recommendations made. Just some general cmments about planners" = "No policy recommendations made",
  "No policy recommendations made; however scientific recs made" = "No policy recommendations made",
  "No policy recommendations given" = "No policy recommendations made",
  "no policy recommendations given" = "No policy recommendations made",
  "No recommendations made." = "No policy recommendations made",
  "no policy recommendations made; although they do talk about impacts on animals" = "No policy recommendations made",
  "No policy recommendations given." = "No policy recommendations made",
  "Don't provide any policy recommendations." = "No policy recommendations made",
  "Suggestions are for research/thought" = "No policy recommendations made",
  "no recommendations given; mostly just call for more monitoring" = "No policy recommendations made",
  "No recommendations made; though recommend future research." = "No policy recommendations made",
  "n" = "No policy recommendations made", # checked this one
  
  # Papers that made recommendations beyond
  "Yes policy recommendations beyond scope" = "Yes, make policy recommendations beyond scope",
  "Yes; policy reccomendations don't account for local birds etc. that are mentioned in paper." = "Yes, make policy recommendations beyond scope",
  "Yes policy recommendations beyond scope; make recommendations for land use planning (which seem good), but don't mention differences betwn their study area and other areas of western US that they extrapolate to." = "Yes, make policy recommendations beyond scope",
  "Yes policy recommendations beyond scope; Recommendations are made without reference to geographic area." = "Yes, make policy recommendations beyond scope",
  "Yes policy recommendations beyond scope; they discuss highway barriers which seems a bit beyond scope" = "Yes, make policy recommendations beyond scope",
  "yes" = "Yes, make policy recommendations beyond scope", # checked this one
  " Yes policy recommendations beyond scope" = "Yes, make policy recommendations beyond scope",
  "Yes; beyond scope" = "Yes, make policy recommendations beyond scope",
  "Yes, recommendations beyond geographic scope" = "Yes, make policy recommendations beyond scope",
  "yes; policy recs are for entire UK" = "Yes, make policy recommendations beyond scope",
  "recommendations focus on matrix quality and mostly cite other research. One does not; it generalizes outside of the area" = "Yes, make policy recommendations beyond scope",
  "Yes; recommendations don’t have any limit on scope." = "Yes, make policy recommendations beyond scope",
  "Policy recommendations made are outside of scope of generalization, but are straightforward." = "Yes, make policy recommendations beyond scope",
  "Make recommendations that are very general, including some about vegetation when that wasn’t the main study focus" = "Yes, make policy recommendations beyond scope",
  "recommend trimming/pruning for greater density; however don't place any limits on this." = "Yes, make policy recommendations beyond scope",
  "Only made about Addis Ababa and they're not very specific." = "Yes, make policy recommendations beyond scope",
  "The recommendations are made in line with findings; however they generalize without any limits" = "Yes, make policy recommendations beyond scope",
  "Management recommendations fit within research scope however they do not mention limits (eg spatial)" = "Yes, make policy recommendations beyond scope",
  "Recommendations don't include limits (only implied limits based on road type)" = "Yes, make policy recommendations beyond scope",
  "policy and management recommendations made follow from the study but they may be beyond research's scope of generalizability." = "Yes, make policy recommendations beyond scope",
  "Here they do generalize quite broadly. No limits are mentioned, though they do follow from the results of the study." = "Yes, make policy recommendations beyond scope",
  "Recommendations given follow but are generalized to ancient walls" = "Yes, make policy recommendations beyond scope",
  "recommendations are very broad" = "Yes, make policy recommendations beyond scope",
  "They generalize about what species to plant without discussing e.g. different regions phrenology" = "Yes, make policy recommendations beyond scope",
  "Make a number of policy recommendations that are not limited by spatial extent or other study design considerations." = "Yes, make policy recommendations beyond scope",
  
  # Papers did not make recommendations beyond scope, sometimes because they are
  # limited, sometimes because they are banal
  "No; make a number of suggestions but point out that they may not have detected mortality" = "No, policy recommendation are within scope",
  " No, policy recommendations are appropriate" = "No, policy recommendation are within scope",
  "No; recommendations seem appropriate." = "No, policy recommendation are within scope",
  "No; recommendations seem appropriate" = "No, policy recommendation are within scope",
  "No, policy recommendations are appropriate" = "No, policy recommendation are within scope",
  " No, policy recommendations are appropriate; recommend scientists use explicit definitions." = "No, policy recommendation are within scope",
  " No, policy recommendations are appropriate; they mostly discuss CH4 management in Wisconsin…" = "No, policy recommendation are within scope",
  "No, policy recommendations are appropriate; made for city where study was completed" = "No, policy recommendation are within scope",
  "No, policy recommendations are appropriate; extensive policy recommendations are good" = "No, policy recommendation are within scope",
  "No, policy recommendations are appropriate; recommendations are fairly basic." = "No, policy recommendation are within scope",
  "no, recommendations are within scope" = "No, policy recommendation are within scope",
  "No; policy recommendations are very broad" = "No, policy recommendation are within scope",
  "Recommendations are made, they make sense based on the research however they aren't region specific." = "No, policy recommendation are within scope",
  "Policy recommendations limited to area & supported by analysis" = "No, policy recommendation are within scope",
  "recommendation for public education within scope" = "No, policy recommendation are within scope",
  "policy recommendations are banal" = "No, policy recommendation are within scope",
  "No; policy recommendations are appropriate" = "No, policy recommendation are within scope",
  "No, policy recommendations are appropriate; they could be more location specieic though" = "No, policy recommendation are within scope",
  "No; management implications are only for local area, I think" = "No, policy recommendation are within scope",
  "Policy recommendations are limited to education" = "No, policy recommendation are within scope",
  "only very vague policy recommendations made (conserve greenspace)" = "No, policy recommendation are within scope",
  "recommendations rely on citations" = "No, policy recommendation are within scope",
  "Limit policy recommendations to the Northeast. This is reasonable." = "No, policy recommendation are within scope",
  "They're pretty broad geographically but limited regarding tree species" = "No, policy recommendation are within scope",
  "Recommendations within study limits, and use other results to support." = "No, policy recommendation are within scope",
  "No, recommendations made are within study limits and geographicly limited" = "No, policy recommendation are within scope",
  "No, recommendations made are within the study limits, though no limits mentioned the support suggestions with other literature." = "No, policy recommendation are within scope",
  "Yes; actually review previous conservation recommendations made and find that they were not quite accurate. Very interesting!!" = "No, policy recommendation are within scope",
  "Recommendations made are only about cost calculations; don't mention limits e.g. based on climactic conditions?" = "No, policy recommendation are within scope",
  "policy suggestion is for flexibility; hard to say if this is beyond the scope of the paper" = "No, policy recommendation are within scope",
  "Recommendations made I think are specific to Baltimore" = "No, policy recommendation are within scope",
  "Recommendations made are within scope of study; specific to Rwanda" = "No, policy recommendation are within scope",
  "Conservation recommendations limited to site" = "No, policy recommendation are within scope",
  "No, they mention limits of test when discussing application to land planning. For forest management, suggestions follow from research ecosystem." = "No, policy recommendation are within scope",
  "Policy recommendations made are very basic, so not outside of scope." = "No, policy recommendation are within scope",
  "Policy recommendations made are within scope." = "No, policy recommendation are within scope",
  "they make recommendations on golf course design, however they are very broad and pointing to future research needed" = "No, policy recommendation are within scope",
  "make general suggestion to wetland managers, but only to monitor their human visitation." = "No, policy recommendation are within scope",
  "Recommendations drawn from other papers." = "No, policy recommendation are within scope",
  "They make only vague conservation recommendations--along the lines of look at your site" = "No, policy recommendation are within scope",
  "Only one very mild management recommendation (last sentence of paper)" = "No, policy recommendation are within scope",
  "recommendations made for densely urbanized areas; this is reasonable but depends on how the reader interprets the statement" = "No, policy recommendation are within scope",
  "Recommendation is very vague and geographically limited to southern europe which is appropriate." = "No, policy recommendation are within scope",
  "Recommendations made pull from additional literature." = "No, policy recommendation are within scope",
  "No, recommendations are within scope though they could be more specific about when these apply (it is mostly addressed in following paragraph)." = "No, policy recommendation are within scope",
  "No, they explicitly mention the need to test other locations" = "No, policy recommendation are within scope",
  "policy recommendations made are to continue riparian conservation in the southwest--seems fairly general but within scope." = "No, policy recommendation are within scope",
  "Recommendations are made but they do mention limited inference" = "No, policy recommendation are within scope",
  "Conservation recommendations made are within the scope." = "No, policy recommendation are within scope",
  "Partly; they make recommendations for southwest reaches. Their n is low but this is appropriately bounded spatially" = "No, policy recommendation are within scope",
  "No" = "No, policy recommendation are within scope",# checked this one

)

Q50_table <- table(Q50)
WHERE_data_KD_clean$Q50 <- Q50


remove(Q43,Q44,Q45,Q46,Q47,Q48,Q49,Q50)

#------------- INDEX Calculation ---------------------------------

# Calculate the reporting quality index

reporting_quality <- tibble(

  WHERE_data_KD_clean[ , c(
    "Q1", # title
    "Q2", # journal
    "Q4", # year
    "Q7", # id
    
    "Q9", "Q15", "Q16", "Q17",
    "Q19", "Q20", "Q21", "Q22",
    "Q23", "Q24",
    
    "Q25", "Q26", "Q27", "Q28",
    "Q30", "Q31", "Q32", "Q33",
    "Q34", "Q35", "Q36", "Q37",
    "Q38", "Q39", "Q43", "Q44",
    "Q45", "Q47", "Q49"
    )]
    
)

reporting_quality$Q4 <- as.Date(reporting_quality$Q4, format = "%Y")

reporting_quality$RQ_index <-   
  
  if_else(reporting_quality$Q9 == "Y", 2, 
          if_else(reporting_quality$Q9 == "P", 1, 0)) +

  if_else(reporting_quality$Q15 == "Yes", 1, 0) +
  
  if_else(reporting_quality$Q16 == "Yes", 1, 0) +
  
  if_else(reporting_quality$Q17 == "Not reported", 0 ,1) +
  
  if_else(reporting_quality$Q19 == "Not reported", 0, 1) + 
  
  if_else(reporting_quality$Q20 == "Yes, in paper text", 2, 
          if_else(reporting_quality$Q20 == "Yes, in Acknowledgments only", 1, 0)) +
  
  if_else(reporting_quality$Q21 == "Yes", 1, 0) +
  
  if_else(reporting_quality$Q22 == "Not reported", 0, 1) +
  
  if_else(reporting_quality$Q23 == "Not reported", 0, 1) + 
  
  if_else(reporting_quality$Q24 == "Not reported", 0, 1) +
  
  if_else(reporting_quality$Q25 == "Not reported", 0, 1) +
  
  if_else(reporting_quality$Q26 == "Unknown", 0, 1) +
  
  if_else(reporting_quality$Q27 == "Unknown", 0, 2) +
  
  if_else(reporting_quality$Q28 == "Unknown", 0, 2) +

  if_else(reporting_quality$Q30 == "Unknown", 0, 1) +
  
  if_else(grepl(pattern = "Non-prob", reporting_quality$Q31), 3,
          if_else(grepl("Probability", reporting_quality$Q31), 4, 0)) +
  
  if_else(is.na(reporting_quality$Q32), 0,
          if_else(reporting_quality$Q32 == "Not reported", -1, 0)) +
  if_else(is.na(reporting_quality$Q33), 0,
          if_else(reporting_quality$Q33 == "Not reported", -1, 0)) +
  if_else(is.na(reporting_quality$Q34), 0,
          if_else(reporting_quality$Q34 == "Not reported", -1, 0)) +
  if_else(is.na(reporting_quality$Q35), 0,
          if_else(reporting_quality$Q35 == "Not reported", -1, 0)) +
  if_else(is.na(reporting_quality$Q36), 0,
          if_else(reporting_quality$Q36 == "Not reported", -1, 0)) +
  if_else(is.na(reporting_quality$Q37), 0,
          if_else(reporting_quality$Q37 == "Not reported", -1, 0)) +
  if_else(is.na(reporting_quality$Q38), 0,
          if_else(reporting_quality$Q38 == "Not reported" | reporting_quality$Q38 == "Unclear", -1, 0)) +
  
  if_else(reporting_quality$Q39 == "Unsure", 0, 1) +
  
  if_else(reporting_quality$Q43 == "No", 0, 4) +
  
  if_else(reporting_quality$Q44 == "No", 0,
          if_else(reporting_quality$Q44 == "Duplicated at same sites only", 2, 4)) +
  
  if_else(is.na(reporting_quality$Q45), 0,
    if_else(reporting_quality$Q45 == "No", 0,
          if_else(reporting_quality$Q45 == "Some, but not all", 1, 2))) +  
  
  if_else(is.na(reporting_quality$Q47), 0,
          if_else(reporting_quality$Q47 == "No", 0,
          if_else(reporting_quality$Q47 == "Partial", 1, 2))) +  

  if_else(reporting_quality$Q49 == "No, generalizes but does not mention limits", 0,
          if_else(reporting_quality$Q49 == "Study doesn't generalize", 1,
                  if_else(reporting_quality$Q49 == "Partly, generalizes and mentions some limits", 2,
                          4)))  
  



library(ggplot2)
ggplot(reporting_quality, aes(Q2, RQ_index)) + geom_boxplot() +
  theme(axis.text.x = element_text(
    angle = 50,
    vjust = 1,
    hjust = 1
  ),
  axis.title.x = element_blank()) + ylab("Reporting Quality Index")

ggplot(reporting_quality, aes(Q4, RQ_index)) + geom_jitter() + geom_smooth() +
  theme(axis.text.x = element_text(
    angle = 0,
    vjust = 1,
    hjust = 1
  ),
  axis.title.x = element_blank()) + ylab("Reporting Quality Index")

library(Kendall)

RQ_TS <- group_by(reporting_quality, reporting_quality$Q4) %>% 
  summarise(meanRQ = mean(RQ_index))
  
  MannKendall(RQ_TS$meanRQ)


# Calculate the sampling quality index

sampling_quality <- tibble(
  
  Q1 = WHERE_data_KD_clean$Q1,
  Q2 = WHERE_data_KD_clean$Q2,
  Q4 = as.Date(WHERE_data_KD_clean$Q4, format = "%Y"),
  
  Q29 = WHERE_data_KD_clean$Q29,
  Q31 = WHERE_data_KD_clean$Q31,
  Q40 = WHERE_data_KD_clean$Q40,
  Q46 = WHERE_data_KD_clean$Q46,
  Q48 = WHERE_data_KD_clean$Q48
  
)

sampling_quality$SQ_index <- 
  
  if_else(sampling_quality$Q29 == "Yes", 2, 
          if_else(sampling_quality$Q29 == "No", 1, 0)) +
  if_else(grepl(pattern = "Probability",x = sampling_quality$Q31) == TRUE, 2, 0) +
  if_else(sampling_quality$Q40 == "No", 2,
          if_else(grepl(pattern = "Yes|Partly", x = sampling_quality$Q40), 1, 0)) +
  if_else(sampling_quality$Q46 == "Yes", 2, 
          if_else(sampling_quality$Q46 == "No", 1, 0)) +
  if_else(sampling_quality$Q48 == "No", 2, 
          if_else(sampling_quality$Q48 == "Yes", 1, 0)) 

ggplot(sampling_quality, aes(Q2, SQ_index)) + geom_boxplot() +
  theme(axis.text.x = element_text(
    angle = 50,
    vjust = 1,
    hjust = 1
  ),
  axis.title.x = element_blank()) + ylab("Sampling Quality Index")


ggplot(sampling_quality, aes(Q4, SQ_index)) + geom_jitter() + geom_smooth() +
  theme(axis.text.x = element_text(
    angle = 0,
    vjust = 1,
    hjust = 1
  ),
  axis.title.x = element_blank()) + ylab("Sampling Quality Index")




ggplot() + geom_point(aes(reporting_quality$RQ_index, sampling_quality$SQ_index)) +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1))
cor(reporting_quality$RQ_index, sampling_quality$SQ_index)


SQ_TS <- group_by(sampling_quality, sampling_quality$Q4) %>% 
  summarise(meanSQ = mean(SQ_index))

MannKendall(SQ_TS$meanSQ)


## ------ Graphs -------------------------------


hist(as.numeric(WHERE_data_KD_clean$Q4), 45, xlim = c(1970,2020), ylim = c(0,25), xlab = "Publication Year", main = "Histogram of Publication Year") 

ReviewedPapers <- group_by(WHERE_data_KD_clean, Q4) %>%
  summarise(count = n()) %>%
  rename(year = Q4)

ReviewedPapers$type <- rep("Read", nrow(ReviewedPapers))

AllPapers <- rbind(TotalPapers, FittingPapers, ReviewedPapers)

AllPapers$type <- as.factor(AllPapers$type) 
AllPapers$type <- factor(AllPapers$type, levels = c("Total", "Fitting", "Read"))


ggplot() +
  geom_area(
    data = AllPapers,
    aes(x = as.numeric(year), y = count, fill = type),
    alpha = 0.9,
    position = "identity"
  ) +
  
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ),
  axis.title.x = element_blank()) +
  
  ylab("Number of Papers") +
  
  scale_x_continuous(breaks = seq(1945, 2020, 10)) +
  scale_fill_manual("Legend",
                    values = c(
                      "Total" = "#dddddd",
                      "Fitting" = "#555555",
                      "Read" = "#000000"
                    ))




# needs count per year probably

group_by(WHERE_data_KD_clean, Q4, Q25) %>%
  summarise(count = n()) %>%
  ggplot() + geom_line(aes(as.numeric(Q4), count)) + facet_grid(rows = vars(Q25),
                                                                labeller = labeller(facet_category = label_wrap_gen(width = 10))) +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ),
  axis.title.x = element_blank()) + ylab("Gradient type used") +
  scale_x_continuous(breaks = seq(1970, 2020, 5))



