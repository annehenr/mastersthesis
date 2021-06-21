setwd("~/Dropbox/MA_dissertations_20-21/Anne Kari/R")

#### LIBRARY ####
packages <- c("Amelia", "mlbench", "naniar", "missForest", "blme", "raster","readxl", "arsenal", "haven", 
              "dplyr", "ggplot2", "openxlsx", "ggpubr", "stargazer", "dplyr", "separationplot", "haven", 
              "lmtest", "multiwayvcov", "MASS", "texreg", "broom", "DescTools", "sandwich", "xtable", 
              "cowplot", "wesanderson", "sqldf", "e1071", "showtext", "effects", "lme4", "mice", "Amelia", "Zelig", "miceadds", "ICC", 
              "pROC", "grDevices", "gdata", "MuMIn", "AICcmodavg", "broom", "tidyverse")

lapply(packages, library, character.only = TRUE)

#### DATA ####

# IMPORTING DATA #

# WiRe #

wire_dta <- read.xls("http://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/BYFJ3Z/6AURFD")

#Subsetting to only nonviolent campaings

wire_nonviol_dta <-  wire_dta[ which(wire_dta$nonviol==1),]

#Importing manually coded merge variables

merge_variables <- read.xls("http://github.com/annehenr/mastersthesis/blob/main/mergevariables.xls?raw=true")

# V-Dem #

load(url("http://github.com/vdeminstitute/vdemdata/blob/master/data/vdem.RData?raw=true"))

# Barro Lee #

barrolee <- read.csv("https://barrolee.github.io/BarroLeeDataSet/BLData/BL2013_F1599_v2.2.csv")

# MERGING WIRE DATA WITH MERGE VARIABLES #

dta <- merge(wire_nonviol_dta, merge_variables,
                 by = c("navcoid", "location"),
                 all.x = TRUE, all.y = FALSE)

# SUBSETTING DATA #

#V-Dem: removing unnecessary variables
vdem_sub <- subset(vdem, year >= 1940)

#V-Dem: keeping only relevant variables
vdem_sub <-  subset(vdem_sub, select = c(country_name, histname, country_id, country_text_id, year, COWcode, 
                                         v2x_polyarchy, v2x_libdem, v2exfemhos, v2exfemhog, v2clslavef, 
                                         v2clslavef_ord, v2clacjstw, v2clacjstw_ord, v2cldiscw, v2cldiscw_ord, 
                                         v2cldmovew, v2cldmovew_ord, v2clprptyw, v2clprptyw_ord, v2pepwrgen, 
                                         v2pepwrgen_ord, v2csgender, v2csgender_ord, v2clgencl, v2clgencl_ord, 
                                         v2peapsgen, v2peapsgen_ord, v2peasjgen, v2peasjgen_ord, v2peasbgen, 
                                         v2peasbgen_ord, v3elwomcab, v2xpe_exlgender, v2x_gender, v2x_gencl, 
                                         v2x_gencs, v2x_genpp, e_autoc, e_democ, e_peaveduc, e_peedgini, e_migdpgro,
                                         e_migdppc, e_total_fuel_income_pc, e_total_oil_income_pc, 
                                         e_total_resources_income_pc, e_miferrat, e_miurbani, e_miurbpop, e_pefeliex, 
                                         e_wb_pop))

# CREATING VARIABLES FOR MERGING #

#Need byear_t variables in order to merge with Barro Lee data set.

dta$byear_t1 <- dta$byear - 1
dta$byear_t2 <- dta$byear - 2
dta$byear_t3 <- dta$byear - 3
dta$byear_t4 <- dta$byear - 4

# MERGING DATA #

# Merging WiRe and V-Dem #
#t
dta_a <- merge(dta, vdem_sub,
               by.x = c("byear", "country_id"),
               by.y = c("year", "country_id"),
               all.x = TRUE, 
               all.y = FALSE)

#t_1
dta_a <- merge(dta_a, vdem_sub,
               by.x = c("byear_t1", "country_id_t1"),
               by.y = c("year", "country_id"),
               all.x = TRUE, 
               all.y = FALSE, 
               suffixes = c("", "_t1"))

#Cleaning and re-aranging
head(dta_a)

dta_a <-  subset(dta_a, select = c(navcoid, country_id, country_name, histname, country_id_t1, country_text_id, 
                        byear, byear_t1, byear_t2, byear_t3, byear_t4, byear_t5, eyear, campaign, barroleemerge, 
                        location, bdate, edate, duration, asia, americas, africa, europe, fsu, mena, actor, target, 
                        success, limited, failure, ongoing, unknown, fselfdet, regchange, secession, other, nmembers, 
                        regviol, defect, violsim, radflank, regaid, statesup, frontlinerole, extentfrontlinerole, 
                        supportrole, symbolicrole, leadership, extentleadership, figureheads, voluntary, coerced, 
                        wmissues, gi_ideol, ge_ideol, formalinvolve, peace_calls, gbvwithin, gbvagainst, discrimination, 
                        v2x_polyarchy, v2x_polyarchy_t1, v2x_libdem, v2x_libdem_t1, v2x_libdem, v2exfemhos, v2exfemhos_t1, 
                        v2clslavef, v2clslavef_t1, v2clslavef_ord, v2clslavef_ord_t1, v2clacjstw, v2clacjstw_t1, 
                        v2clacjstw_ord, v2clacjstw_ord_t1, v2cldiscw, v2cldiscw_t1, v2cldiscw_ord, v2cldiscw_ord_t1, 
                        v2cldmovew, v2cldmovew_t1, v2cldmovew_ord, v2cldmovew_ord_t1, v2clprptyw, v2clprptyw_t1, 
                        v2clprptyw_ord, v2clprptyw_ord_t1, v2pepwrgen, v2pepwrgen_t1, v2pepwrgen_ord, v2pepwrgen_ord_t1,
                        v2csgender, v2csgender_t1, v2csgender_ord, v2csgender_ord_t1, v2clgencl, v2clgencl_t1, 
                        v2clgencl_ord, v2clgencl_ord_t1, v2peapsgen, v2peapsgen_t1, v2peapsgen_ord, v2peapsgen_ord_t1, 
                        v2peasjgen, v2peasjgen_t1, v2peasjgen_ord, v2peasjgen_ord_t1, v2peasbgen, v2peasbgen_t1, 
                        v2peasbgen_ord, v2peasbgen_ord_t1, v2xpe_exlgender, v2xpe_exlgender_t1, v2x_gender, 
                        v2x_gender_t1, v2x_gencl, v2x_gencl_t1, v2x_gencs, v2x_gencs_t1, v2x_genpp, v2x_genpp_t1, 
                        e_autoc, e_autoc_t1, e_democ, e_democ_t1, e_peaveduc, e_peaveduc_t1, e_peedgini, e_peedgini_t1, 
                        e_migdpgro, e_migdpgro_t1, e_migdppc, e_migdppc_t1, e_total_fuel_income_pc, 
                        e_total_fuel_income_pc_t1, e_total_oil_income_pc, e_total_oil_income_pc_t1, 
                        e_total_resources_income_pc, e_total_resources_income_pc_t1, e_miferrat, e_miferrat_t1, 
                        e_miurbani, e_miurbani_t1, e_miurbpop, e_miurbpop_t1, e_pefeliex, e_pefeliex_t1, e_wb_pop, 
                        e_wb_pop_t1))

# Merging with Barro Lee data set #

#Keeping only relevant variables from Barro Lee
barrolee <- subset(barrolee, select = c(year, country, yr_sch, yr_sch_pri, 
                                        yr_sch_sec, yr_sch_ter))

#Barro Lee data set only observations every fifth year:

#t
dta_b <- merge(dta_a, barrolee,
               by.x = c("barroleemerge", "byear"), by.y = c("country", "year"),
               all.x = TRUE, all.y = FALSE)
#t1
dta_b <- merge(dta_b, barrolee, 
               by.x = c("barroleemerge", "byear_t1"), by.y = c("country", "year"),
               all.x = TRUE, all.y = FALSE, suffixes = c("", "1"))
#t2
dta_b <- merge(dta_b, barrolee,
               by.x = c("barroleemerge", "byear_t2"), by.y = c("country", "year"),
               all.x = TRUE,
               all.y = FALSE, suffixes = c("", "2"))
#t3
dta_b <- merge(dta_b, barrolee,
               by.x = c("barroleemerge", "byear_t3"), by.y = c("country", "year"),
               all.x = TRUE, all.y = FALSE, suffixes = c("", "3"))
#t4
dta_b <- merge(dta_b, barrolee, 
               by.x = c("barroleemerge", "byear_t4"), by.y = c("country", "year"),
               all.x = TRUE, all.y = FALSE, suffixes = c("", "4"))

#Merging variables together
dta_b <- dta_b %>%
  mutate(yr_sch = coalesce(yr_sch, yr_sch1, yr_sch2, yr_sch3, yr_sch4),
         yr_sch_pri = coalesce(yr_sch_pri, yr_sch_pri1, yr_sch_pri2, 
                               yr_sch_pri3, yr_sch_pri4),
         yr_sch_sec = coalesce(yr_sch_sec, yr_sch_sec1, yr_sch_sec2, 
                               yr_sch_sec3, yr_sch_sec4),
         yr_sch_ter = coalesce(yr_sch_ter, yr_sch_ter1, yr_sch_ter2, 
                               yr_sch_ter3, yr_sch_ter4))

#Dropping the t variables
dta_b <- subset(dta_b, 
                select = -c(yr_sch1, yr_sch2, yr_sch3, yr_sch4, yr_sch_pri1, 
                            yr_sch_pri2, yr_sch_pri3, yr_sch_pri4, yr_sch_sec1, yr_sch_sec2, 
                            yr_sch_sec3, yr_sch_sec4, yr_sch_ter1, yr_sch_ter2, yr_sch_ter3, 
                            yr_sch_ter4))

dta <- dta_b

#### CLEANING AND ORGANIZING ####

#Creating binary extent womens participation variable
dta$bin_extentfrontline <- ifelse(dta$extentfrontlinerole >= "2", 1, 0)

table(dta$extentfrontlinerole) #Checking that recoding lines up
table(dta$bin_extentfrontline)

#Recoding gi_variable variable
dta$gi_ideol_recode <- ifelse(dta$gi_ideol == "1", 1, 0)

table(dta$gi_ideol) #Checking that recoding lines up
table(dta$gi_ideol_recode)

#Creating logged variables

dta$e_migdppc_ln <- log(dta$e_migdppc)
dta$nmembers_ln <- log(dta$nmembers)

#Creating logged t-1 variables for regression

dta$e_migdppc_t1_ln <- log(dta$e_migdppc_t1)

# Creating categorical region variable

dta$region[dta$asia == 1] <- 1 
dta$region[dta$africa == 1] <- 2 
dta$region[dta$europe == 1] <- 3 
dta$region[dta$americas == 1] <- 4
dta$region[dta$fsu == 1] <- 5
dta$region[dta$mena == 1] <- 6

table(dta$region)
summary(dta$region)

dta$region <- factor(dta$region, labels = c("Asia", "Africa", "Europe", 
                                            "Americas", "Former Soviet Union",
                                            "Middle East and North Africa"))

table(dta$region)

#Creating country variable with values for the NAs from V-Dem

dta$countryvar = dta$histname

dta$countryvar

dta$countryvar <- recode(dta$countryvar,
                        "People's Socialist Republic of Albania" = "Albania",
                         "People's Democratic Republic of Algeria [independent state]" = "Algeria",
                         "Argentine Nation/Republic" = "Argentina",
                         "Republic of Armenia [independent state]" = "Armenia",
                         "Kingdom of Bahrain [independent state]" = "Bahrain",
                         "People's Republic of Bangladesh [independent state]" = "Bangladesh",
                         "Russian Socialist Federative Republic (USSR)" = "USSR",
                         "Republic of Belarus [independent state]" = "Belarus",
                         "Republic of Benin [independent state]" = "Benin",
                         "Republic of Bolivia" = "Bolivia", "Federative Republic of Brazil" = "Brazil",
                         "Bulgarian People's Republic" = "Bulgaria", "Republic of Bulgaria" = "Bulgaria",
                         "Republic of Cameroon" = "Cameroon", "Central African Republic" = "CAR",
                         "Republic of Chile" = "Chile", "People's Republic of China" = "China",
                         "Republic of Côte d'Ivoire [independent state]" = "Ivory Coast",
                         "Republic of Croatia [independent state]" = "Croatia",
                         "Czechoslovakia Socialist Republic as part of Soviet bloc" = "Czechoslovakia",
                         "Republic of Djibouti [independent state]," = "Djibouti",
                         "German Democratic Republic" = "East Germany", 
                         "Democratic Republic of Timor-Leste under Indonesian occupation" = "East Timor",
                         "Republic of Ecuador" = "Ecuador", "Arab Republic of Egypt" = "Egypt",
                         "Republic of El Salvador" = "El Salvador", "Republic of Fiji" = "Fiji",
                         "Republic of the Fiji Islands" = "Fiji", 
                         "Georgia [independent state]" = "Georgia", "Gold Coast Colony" = "Ghana",
                         "Republic of Ghana" = "Ghana", "Kingdom of Greece [restored]" = "Greece",
                         "Third Hellenic Republic (Greek Republic)" = "Greece", 
                         "Republic of Guinea [independent state]" = "Guinea", 
                         "Co-operative Republic of Guyana" = "Guyana", "Republic of Haiti" = "Haiti",
                         "Republic of Honduras" = "Honduras", 
                         "Second Hungarian People Republic under Soviet guidance" = "Hungary", 
                         "Republic of Hungary [independent from Russian influence]" = "Hungary",
                         "Republic of Iceland" = "Iceland", 
                         "Republic of India [independent state]" = "India", 
                         "Republic of Indonesia" = "Indonesia", "Islamic Republic of Iran" = "Iran", 
                         "State of Israel [independent state]" = "Israel", 
                         "The Hashemite Kingdom of Jordan [independent state]" = "Jordan", 
                         "Republic of Kenya [independent state]" = "Kenya",
                         "Kirghiz Soviet Socialist Republic under Soviet rule" = "Kyrgyzstan", 
                         "Kyrgyz Republic [independent state]" = "Kyrgyzstan",
                         "Republic of Lebanon and occupied Southern Lebanon" = "Lebanon", 
                         "Republic of Madagascar [independent state]" = "Madagascar", 
                         "Federation of Rhodesia and Nyasaland/Central African Federation [consolidated with Rhodesia (North and South)]" =
                           "Malawi",
                         "Republic of Malawi [independent state]" = "Malawi", 
                         "Republic of Maldives [independent state]" = "Maldives", 
                         "Republic of Mali" = "Mali", 
                         "Islamic Republic of Mauritania [independent state]" = "Mauritania",
                         "United Mexican States" = "Mexico", "Mongolian People's Republic" = "Mongolia",
                         "Kingdom of Morocco [independent state]" = "Morocco", 
                         "Republic of the Union of Myanmar [independent state]" = "Myanmar", 
                         "Kingdom of Nepal" = "Nepal", "Federal Democratic Republic of Nepal" = "Nepal",
                         "Republic of Nicaragua" = "Nicaragua", 
                         "Republic of Niger [independent state]" = "Niger", 
                         "Colony and Protectorate of Nigeria [unified]" = "Nigeria",
                         "Federal Republic of Nigeria [independent state]" = "Nigeria", 
                         "United Republic of Tanzania [Zanzibar joins Tanganyika][independent state]" =
                           "Tanzania", 
                         "Islamic Republic of Pakistan [independent state]" = "Pakistan", 
                         "West Bank under Israeli occupation [civil administration]" = "Palestine", 
                         "Republic of Panama [independent state]" = "Panama", "Republic of Peru" = "Peru", 
                         "Republic of the Philippines [independent state]" = "Philippines", 
                         "People's Republic of Poland" = "Poland", "Portuguese Republic" = "Portugal", 
                         "Republic of Korea" = "South Korea", "Socialist Republic of Romania" = "Romania", 
                         "Russian Federation" = "Russia", "Senegambia" = "Senegal", 
                         "Republic of Senegal" = "Senegal", "Federal Republic of Yugoslavia" = "Yugoslavia",
                         "Socialist Federal Republic of Yugoslavia" = "Yugoslavia", 
                         "Union of South Africa [independent state]" = "South Africa", 
                         "Republic of Vietnam (also known as South Vietnam)" = "South Vietnam",
                         "Republic of the Sudan [independent state]" = "Sudan", 
                         "Republic of Suriname [independent state]" = "Suriname", 
                         "Kingdom of Swaziland [independent state]" = "Swaziland", 
                         "Syrian Arab Republic" = "Syria", 
                         "Republic of China [headquartered in Taiwan]" = "Taiwan", 
                         "Kingdom of Thailand" = "Thailand", 
                         "Togolese Republic [independent state]" = "Togo", 
                         "Tunisian Republic [independent state]" = "Tunisia", 
                         "Republic of Turkey [Does not include Algeria, Tunisia, Egypt, Cyprus, Kuwait nor Bahrain (coded separately)]" =
                           "Turkey", 
                         "Ukrainian Soviet Socialist Republic" = "Ukraine", 
                         "United Republic of Tanzania [Zanzibar joins Tanganyika][independent state]" =
                           "Tanzania",
                         "Oriental Republic of Uruguay" = "Uruguay", "Republic of Venezuela" = "Venezuela",
                         "Republic of Yemen" = "Yemen", 
                         "Colony of Northern Rhodesia [consolidated with Nyasaland and Southern Rhodesia into the Federation of Rhodesia and Nyasaland/Central African Federation]" =
                           "Zambia", 
                         "Republic of Zambia [independent state]" = "Zambia", 
                         "Republic of South Africa" = "South Africa")

dta$countryvar

dta$countryvar[dta$navcoid == 254] <- "Aruba"
dta$countryvar[dta$navcoid == 10013] <- "Tibet"
dta$countryvar[dta$navcoid == 310] <- "Tonga"
dta$countryvar[dta$navcoid == 10015] <- "Western Sahara"
dta$countryvar[dta$navcoid == 243] <- "Western Sahara"

#Writing Excel file in order to inspect data 

write.xlsx(dta, file = "masterdataset.xlsx")


#### SECTION 2 - DATAVISUALIZATION ####
# LABELS ON CATEGORICAL VARIABLES #
dta_viz <- within(dta, {
  frontlinerole <- factor(frontlinerole, labels = c("No", "Yes"))
  extentfrontlinerole <- factor(extentfrontlinerole, labels = c("None", "Limited",
                                                                "Moderate", "Extensive"))
  bin_extentfrontline <- factor(bin_extentfrontline, labels = c("None/limited",
                                                                "Moderate/extensive"))
  gi_ideol_recode <- factor(gi_ideol_recode, labels = c("No",
                                                        "Yes"))
  regviol <- factor(regviol, labels = c("No", 
                                        "Yes"))
  gbvagainst <- factor(gbvagainst, labels = c("No",
                                              "Yes"))
  
  
})

# TIME, LENGTH AND WOMENS PARTICIPATION #

p_2_1 <- ggplot(dta_viz, aes(x=byear, y=duration, colour = bin_extentfrontline,
                            label=campaign)) +
  geom_point(size=1) +
  geom_vline(xintercept = 1989, linetype="dotted", 
             color = "gray", size=1) +
  geom_text(aes(label=ifelse(duration>5000, as.character(campaign), '')), size=2.5,
            colour="Black", hjust=0, vjust=1, family = "Times") +
  labs(x = "Year in which campaign began",
       y = "Length of campaign in days",
       caption = "Gray dottet line indicates fall of Berlin Wall",
       colour ="") +
  scale_color_manual(values= c("coral", "#0B775E")) +
  theme_light() + 
  theme(text=element_text(family="Times"))
p_2_1

# DISTRIBUTION ACROSS REGIONS #

p_2_2 <- ggplot(dta_viz, mapping = aes(x = region)) +
  geom_bar(fill = "#0B775E", colour = "#0B775E") +
  labs(x = "Regions",
       y = "Count") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))
p_2_2

p_2_3 <- ggplot(dta_viz, aes(x = region, fill = bin_extentfrontline)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  scale_fill_manual(values= c("coral", "#0B775E")) +
  theme_classic() +
  labs(x = "Region",
       y = "Count",
       fill ="") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))
p_2_3

p_2_4 <- ggplot(dta_viz, aes(x=byear, y=duration, colour= bin_extentfrontline,
                            label=campaign)) +
  geom_point(size=1) +
  geom_vline(xintercept = 1989, linetype="dotted", 
             color = "gray", size=1) +
  geom_text(aes(label=ifelse(duration>5000, as.character(campaign), '')), size=2.5,
            colour="Black", hjust=0, vjust=1, family = "Times") +
  labs(x = "Year in which campaign began",
       y = "Length of campaign in days",
       caption = "Gray dottet line indicates fall of Berlin Wall",
       colour ="") +
  scale_color_manual(values= c("coral", "#0B775E")) +
  facet_wrap(~region) +
  theme_minimal() +
  theme(text=element_text(family="Times", size=11))
p_2_4

#### SECTION 5 - DATA AND METHODS ####

### VARIABLES ###

# Summary statistics - complete case analysis

dta_sum_complete <- subset(dta, select = c("bin_extentfrontline","gi_ideol_recode", "regviol", "gbvagainst",
                      "e_migdppc_ln", "e_miferrat", "yr_sch", "nmembers_ln"))
dta_sum_complete <- na.omit(dta_sum_complete, na.actio = "exclude")

                      
stargazer(dta_sum_complete[c("bin_extentfrontline","gi_ideol_recode","regviol", "gbvagainst",
                "e_migdppc_ln", "e_miferrat", "yr_sch", "nmembers_ln")],
          type = "latex", 
          omit.summary.stat = c("p25", "p75", "N"),
          covariate.labels = c(bin_extentfrontline = "Extent of women's frontline participation (bin)",
                               gi_ideol_recode = "Gender-inclusive ideology",
                               regviol = "Regime violence against campaign",
                               gbvagainst = "Gender-based violence against campaign",
                               migdppc_ln = "GDP per capita, logged",
                               miferrat_ln = "Fertility rate",
                               yr_sch = "Average years of education in population of women",
                               nmembers_ln = "Campaign membership, logged"))

# Distribution of frontline variables #

dta_depvar <- subset(dta_viz, select = c(countryvar, extentfrontlinerole, bin_extentfrontline, gi_ideol_recode, regviol, gbvagainst, 
                                         e_migdppc_ln, e_miferrat, yr_sch, nmembers_ln))

dta_depvar <- na.omit(dta_depvar, na.action = "exclude")

p_5_1 <- ggplot(dta_depvar, mapping = aes(x = extentfrontlinerole)) +
  geom_bar(fill = "#0B775E", colour = "#0B775E") +
  labs(x = "Extent of women's frontline participation",
       y = "Count") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))

p_5_2 <- ggplot(dta_depvar, aes(x=bin_extentfrontline)) +
  geom_bar(fill = "#0B775E", colour = "#0B775E") +
  labs(x = "Extent of women's frontline participation (bin)",
       y = "Count") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))

plot_grid(p_5_1, p_5_2, ncol = 2)

### MULTILEVEL STRUCTURE ###

p_5_3 <- ggplot(dta_depvar, mapping = aes(x = countryvar)) + 
  geom_bar(fill = "#0B775E") +
  coord_flip() +
  labs(x = "Country",
       y = "Count") +
  theme(text=element_text(family="Times", size=11))
p_5_3

#Checking ICC

icc_dta <- subset(dta, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, gbvagainst, e_migdppc_ln, e_miferrat, yr_sch, nmembers_ln))    

icc_dta <- na.omit(icc_dta, na.action = "exclude")

ICCest(countryvar, bin_extentfrontline, data = icc_dta, alpha = 0.05, CI.type = c("THD", "Smith"))
#ICC: 0.1297452

#### REPORTED IN APPENDIX B ####

### CORRELATION MATRIX ###

correlation_table <- cor(dta_sum_complete, use = "complete.obs")

cor_x <- xtable(correlation_table)

print(cor_x, rotate.colnames = TRUE)

### VARIANCE AND SKEWNESS ###

dta_skew <- subset(dta, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, gbvagainst, e_migdppc, e_migdppc_ln, e_miferrat, yr_sch, nmembers, nmembers_ln))    

dta_skew <- na.omit(dta_skew, na.action = "exclude")

# e_migdppc
skewness(dta_skew$e_migdppc, na.rm = T)
#2.270051
kurtosis(dta_skew$e_migdppc, na.rm = T)
#8.100488

#Fertilityrate
skewness(dta_skew$e_miferrat, na.rm = T)
#0.5569763
kurtosis(dta_skew$e_miferrat, na.rm = T)
#-0.8822311

#Average total years of school in population of women
skewness(dta_skew$yr_sch, na.rm = T)
#0.09910392
kurtosis(dta_skew$yr_sch, na.rm = T)
#-0.9773547

#Nmembers
skewness(dta_skew$nmembers, na.rm = T)
#9.052497
kurtosis(dta_skew$nmembers, na.rm = T)
#86.12002


#GDP per cap
p_b_1 <- ggplot(data = dta_skew, aes(e_migdppc)) + 
  geom_histogram(aes(y = ..density..), col = "#0B775E", fill = "#0B775E") + 
  geom_density() +
  theme_light() +
  theme(text=element_text(family="Times", size=11))
p_b_1
#GDP per cap logged
p_b_2 <- ggplot(data = dta_skew, aes(e_migdppc_ln)) + 
  geom_histogram(aes(y = ..density..), col = "#0B775E", fill = "#0B775E") + 
  geom_density() +
  theme_light() +
  theme(text=element_text(family="Times", size=11))
p_b_2

#Nmembers
p_b_3 <- ggplot(data = dta_skew, aes(nmembers)) + 
  geom_histogram(aes(y = ..density..), col = "#0B775E", fill = "#0B775E") +
  geom_density() +
  theme_light() +
  theme(text=element_text(family="Times", size=11))
p_b_3
#Nmembers logged
p_b_4 <- ggplot(data = dta_skew, aes(nmembers_ln)) + 
  geom_histogram(aes(y = ..density..), col = "#0B775E", fill = "#0B775E") +
  geom_density() +
  theme_light() +
  theme(text=element_text(family="Times", size=11))
p_b_4

plot_grid(p_b_1, p_b_2, p_b_3, p_b_4, ncol = 2)


### DISTRIBUTION OF BINARY VARIABLES OF INTEREST ###

dta_binary <- subset(dta_viz, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, gbvagainst, e_migdppc_ln, e_miferrat, yr_sch, nmembers_ln))

dta_bin <- na.omit(dta_binary, na.action = "exclude")

p_b_1 <- ggplot(dta_bin, mapping = aes(x = gi_ideol_recode)) +
  geom_bar(fill = "#0B775E", colour = "#0B775E") +
  labs(x = "Gender-inclusive ideology",
       y = "Count") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))

p_b_2 <- ggplot(dta_bin, aes(x=regviol)) +
  geom_bar(fill = "#0B775E", colour = "#0B775E") +
  labs(x = "Regime violence",
       y = "Count") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))

p_b_3 <- ggplot(dta_bin, aes(x=gbvagainst)) +
  geom_bar(fill = "#0B775E", colour = "#0B775E") +
  labs(x = "Gender-based violence",
       y = "Count") +
  theme_light() +
  theme(text=element_text(family="Times", size=11))

plot_grid(p_b_1, p_b_2, p_b_3, ncol = 2)

#### SECTION 6 - ANALYSIS ####

# Preprosessing data before regression - turning categorical variables into factors#

dta_reg <- dta

str(dta_reg, list.len=ncol(dta_reg))

col_names <- sapply(dta_reg, function(col) length(unique(col)) < 4)
dta_reg[ , col_names] <- lapply(dta[ , col_names] , factor)

str(dta_reg, list.len=ncol(dta_reg))

col_names_2 <- c('extentfrontlinerole', 'extentleadership', 'v2clslavef_ord', 'v2clslavef_ord_t1', 'v2clacjstw_ord', 
                 'v2clacjstw_ord_t1', 'v2cldiscw_ord', 'v2cldiscw_ord_t1', 'v2cldmovew_ord', 'v2cldmovew_ord_t1', 
                 'v2clprptyw_ord', 'v2clprptyw_ord_t1', 'v2pepwrgen_ord', 'v2pepwrgen_ord_t1', 'v2csgender_ord', 
                 'v2csgender_ord_t1', 'v2clgencl_ord', 'v2clgencl_ord_t1', 'v2peapsgen_ord', 'v2peapsgen_ord_t1', 
                 'v2peasjgen_ord', 'v2peasjgen_ord_t1', 'v2peasbgen_ord', 'v2peasbgen_ord_t1')
dta_reg[,col_names_2] <- lapply(dta_reg[,col_names_2] , factor)

dta_reg %>% replace_with_na(replace = list(extentleadership = -99))

str(dta_reg, list.len=ncol(dta_reg))

### REGRESSION TABLE 1 ###

dta_reg_1 <- subset(dta_reg, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, gbvagainst, e_migdppc_ln, e_miferrat, yr_sch, nmembers_ln))

dta_reg_1 <- na.omit(dta_reg_1, na.actio = "exclude")


# Logistic regression - base model #

log1 <- glm(bin_extentfrontline ~ 
              gi_ideol_recode +
              regviol +
              gbvagainst +
              e_migdppc_ln +
              e_miferrat +
              yr_sch,
            family = "binomial",
            data=dta_reg_1)

summary(log1)

# Creating clustered robust standard errors clustered on country
clusteredVCOVlog1 <- vcovCL(log1, 
                            cluster = dta_reg_1$countryvar)

log1$clusterVCOV <- clusteredVCOVlog1

coeftest(log1, 
         vcov. = vcovCL(log1, cluster = dta_reg_1$countryvar))


# logistic regression - controls #

log2 <-  glm(bin_extentfrontline ~ 
               gi_ideol_recode +
               regviol +
               gbvagainst +
               e_migdppc_ln +
               e_miferrat +
               yr_sch +
               nmembers_ln,
             family = "binomial",
             data=dta_reg_1)

summary(log2)

# Creating clustered robust standard errors clustered on country
clusteredVCOVlog2 <- vcovCL(log2, 
                            cluster = dta_reg_1$countryvar)

log2$clusterVCOV <- clusteredVCOVlog2

coeftest(log2, 
         vcov. = vcovCL(log2, cluster = dta_reg_1$countryvar))


### BUTTON - REGRESSION TABLE 1 ###

stargazer(log1, log2,
          p.auto = TRUE,
          se = list(sqrt(diag(log1$clusterVCOV)),
                    sqrt(diag(log2$clusterVCOV))),
          type = "latex",
          model.names = FALSE,
          dep.var.labels=c("Extent of women's frontline participation"),  
          digits = 3,
          covariate.labels = c(gi_ideol_recode = "Gender inclusive ideology",
                               regviol = "Regime violence against campaign",
                               gbvagainst = "Gender-based violence against campaign",
                               e_migdppc_ln = "GDP per capita, logged",
                               e_miferrat = "Fertility rate",
                               yr_sch = "Average years of education",
                               nmembers_ln = "Campaign membership, logged"),
          notes = "Robust standard errors clustered around country",
          notes.append = T,
          font.size = "footnotesize")

### EFFECT OF SIGNIFICANT VARIABLES ###

#Predplot 1 - Gender-inclusive ideology

simb <- mvrnorm(n=1000,
                 mu=log2$coefficients,
                 Sigma=vcov(log2))

names(coefficients(log2))

xMatrix1 <- cbind(1,
                  c(0,1), 
                  0,
                  0,
                  mean(dta_reg_1$e_migdppc_ln, na.rm = TRUE),
                  mean(dta_reg_1$e_miferrat, na.rm = TRUE),
                  mean(dta_reg_1$yr_sch, na.rm = TRUE),
                  mean(dta_reg_1$nmembers_ln, na.rm = TRUE))

ncol(simb) == ncol(xMatrix1)

xBetaMatrix1 <- xMatrix1 %*% t(simb)
predProps1 <- 1/(1+exp(-xBetaMatrix1))

quantileValues1 <- apply(X = predProps1,
                         MARGIN = 1,
                         FUN = quantile, plobs = c(.05,.5,.95))
quantileValues1 <- as.data.frame(t(quantileValues1))

plotPoints1 <- cbind(c("No (or contested)", 
                       "Yes"),
                     quantileValues1)

colnames(plotPoints1) <- c("Ideology", "lower", 
                           "estimate", "upper")

predplot1 <- ggplot(plotPoints1,
                    aes(x = Ideology,
                        y = estimate,
                        ymin = lower,
                        ymax = upper)) +
  geom_errorbar(width =.2) +
  geom_point(aes(col=as.factor(round(estimate, 3))))+
  color_palette(c("darkseagreen4", "darkseagreen4")) +
  ylim(0, 0.4) +
  xlab("Gender-inclusive ideology") +
  ylab("Probability of moderate to extensive women's frontline participation") +
  theme_minimal() +
  labs(color="Probabilities") +
  theme(text=element_text(family="Times", size=11))

predplot1

#predplot2 - Gender-based violence

xMatrix2 <- cbind(1,
                  0,
                  0,
                  c(0,1),
                  mean(dta_reg_1$e_migdppc_ln, na.rm = TRUE),
                  mean(dta_reg_1$e_miferrat, na.rm = TRUE),
                  mean(dta_reg_1$yr_sch, na.rm = TRUE),
                  mean(dta_reg_1$nmembers_ln, na.rm = TRUE))

ncol(simb) == ncol(xMatrix2)

xBetaMatrix2 <- xMatrix2 %*% t(simb)
predProps2 <- 1/(1+exp(-xBetaMatrix2))

quantileValues2 <- apply(X = predProps2,
                         MARGIN = 1,
                         FUN = quantile, plobs = c(.05,.5,.95))
quantileValues2 <- as.data.frame(t(quantileValues2))


plotPoints2 <- cbind(c("No",
                       "Yes"),
                     quantileValues2)

colnames(plotPoints2) <- c("GBV", "upper", 
                           "estimate", "lower")

predplot2 <- ggplot(plotPoints2,
                    aes(x = GBV,
                        y = estimate,
                        ymin = lower,
                        ymax = upper)) +
  geom_errorbar(width =.2) +
  geom_point(aes(col=as.factor(round(estimate, 3))))+
  theme_minimal() +
  color_palette(c("darkseagreen4", "darkseagreen4")) +
  ylim(0, 0.4) +
  xlab("Gender-based violence against campaing") +
  ylab("Probability of moderate to extensive women's frontline participation") +
  labs(color="Probabilities") +
  theme(text=element_text(family="Times", size=11))
  

predplot2

plot_grid(predplot1, predplot2)

# Effect of GDP per capita #

ourRange <- seq(min(dta_reg_1$e_migdppc_ln, na.rm = TRUE),
                max(dta_reg_1$e_migdppc_ln, na.rm = TRUE),
                by = .1)

ourX <- cbind(1,
              0,
              0,
              0,
              ourRange,
              mean(dta_reg_1$e_miferrat, na.rm = TRUE),
              mean(dta_reg_1$yr_sch, na.rm = TRUE), 
              mean(dta_reg_1$nmembers_ln, na.rm = TRUE))

ncol(simb) == ncol(ourX)

xbetas <- ourX %*% t(simb)
expY <- 1/(1+exp(-xbetas))

quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))
plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`)) +
  geom_ribbon(alpha = 0.3, fill = "#0B775E") +
  geom_line(color = "#0B775E") +
  xlab("GDP per capita, logged") +
  ylab("Probability of moderate to extensive frontline participation of women") +
  theme(text=element_text(family="Times"))

# AICc #

cand_models = list(log1, log2)

aictab <- aictab(cand.set = cand_models, second.order = T)

aictab <- xtable(aictab)

print(aictab)

### RESULTS ###

#Model 2 Gender Inclusive Ideology
exp(1.162)
#3.19632

#Model 2 Gender Based Violence
exp(1.104)
#3.016207

#Model 2 logged GDP per capita
exp(0.811)
#2.250157
(exp(0.811) - 1)*100
#125.0157

### MISSING ###

dta_varint <- subset(dta, select = c(bin_extentfrontline, gi_ideol_recode, regviol, 
                                     gbvagainst, e_migdppc_ln, 
                                     e_miferrat, yr_sch, nmembers_ln))

missmap(dta_varint,
        font = "Times New Roman",
        col=c("coral","#0B775E"), 
        margins = c(7, 2), 
        main = "")

# Random Forest Imputation #

dta_imp <- subset(dta_reg, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, 
                                      gbvagainst, e_migdppc_ln, e_miferrat, yr_sch, nmembers_ln))

dta_imp$countryvar <- as.numeric(as.factor(dta_imp$countryvar))

dta_imp$countryvar

set.seed(69)

dta.imp <- missForest(dta_imp, ntree = 10000, mtry = 1000)

dta.imp$OOBerror

dta_imp_df <- dta.imp$ximp

# Logistic regression base model #

logimp1 <- glm(bin_extentfrontline ~ 
                 gi_ideol_recode +
                 regviol +
                 gbvagainst +
                 e_migdppc_ln +
                 e_miferrat +
                 yr_sch,
               family = "binomial",
               data=dta_imp_df)

summary(logimp1)

# Creating clustered robust standard errors clustered on location
clusteredVCOVlogimp1 <- vcovCL(logimp1, 
                               cluster = dta_imp_df$countryvar)

logimp1$clusterVCOV <- clusteredVCOVlogimp1

coeftest(logimp1, 
         vcov. = vcovCL(logimp1, cluster = dta_imp_df$countryvar))

# Logistic regression - controls 1 #

logimp2 <- glm(bin_extentfrontline ~ 
                 gi_ideol_recode +
                 regviol +
                 gbvagainst +
                 e_migdppc_ln +
                 e_miferrat +
                 yr_sch +
                 nmembers_ln,
               family = "binomial",
               data=dta_imp_df)

summary(logimp2)

# Creating clustered robust standard errors clustered on location
clusteredVCOVlogimp2 <- vcovCL(logimp2, 
                               cluster = dta_imp_df$countryvar)

logimp2$clusterVCOV <- clusteredVCOVlogimp2

coeftest(logimp2, 
         vcov. = vcovCL(logimp2, cluster = dta_imp_df$countryvar))


### BUTTON: REGRESSION TABLE 2 ###

stargazer(logimp1, logimp2, 
          p.auto = TRUE,
          se = list(sqrt(diag(logimp1$clusterVCOV)),
                    sqrt(diag(logimp2$clusterVCOV))),
          type = "latex",
          dep.var.labels=c("Extent of women's frontline participation"),
          model.names = FALSE, 
          font.size = "footnotesize",
          digits = 3,
          ord.intercepts = T,
          notes = "Robust standard errors clustered around country",
          notes.append = T,
          covariate.labels = c(gi_ideol_recode = "Gender inclusive ideology",
                               regviol = "Regime violence against campaign",
                               gbvagainst = "Gender-based violence against campaign",
                               migdppc_ln = "GDP per capita, logged",
                               miferrat = "Fertility rate",
                               yr_sch = "Average years of education",
                               nmembers_ln = "Campaign membership, logged"))

#### REPORTED IN APPENDIX C ####

### ROC CURVE ###

preds1=predict(log1)
roc1=pROC::roc(dta_reg_1$bin_extentfrontline ~ preds1)

preds2=predict(log2)
roc2=pROC::roc(dta_reg_1$bin_extentfrontline ~ preds2)

auc(roc1)
auc(roc2)
plot(roc1, family = "Times New Roman")
lines(roc2, col="#0B775E") 
text(0.4, 0.3, labels = "AUC Model 1: 0.7868", family = "Times")
text(0.4, 0.25, labels = "AUC Model 2: 0.7968", col = "#0B775E", family = "Times")


### LINEARITY ###

# Predict the probability (p) of diabete positivity
probabilities <- predict(log2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# Only numeric predictors
mydata <- dta_reg_1 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Plot
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(col="#0B775E") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  theme(text=element_text(family="Times", size=11))

#Model without fertility rate
mid_minfer <- glm(bin_extentfrontline ~ 
                    gi_ideol_recode +
                    regviol +
                    gbvagainst +
                    e_migdppc_ln +
                    yr_sch +
                    nmembers_ln,
                  family = "binomial",
                  data=dta_reg_1)

clusteredVCOVmid_minfer <- vcovCL(mid_minfer, 
                               cluster = dta_reg_1$countryvar)

mid_minfer$clusterVCOV <- clusteredVCOVmid_minfer

coeftest(mid_minfer, 
         vcov. = vcovCL(mid_minfer, cluster = dta_reg_1$countryvar))

# BUTTON #
stargazer(mid_minfer,
          type = "latex", 
          p.auto = TRUE,
          se = list(sqrt(diag(mid_minfer$clusterVCOV))),
          dep.var.labels=c("Extent of women's frontline participation"),
          model.names = FALSE, 
          font.size = "footnotesize",
          digits = 3,
          ord.intercepts = T,
          notes = "Robust standard errors clustered around country",
          notes.append = T,
          covariate.labels = c(gi_ideol_recode = "Gender inclusive ideology",
                               regviol = "Regime violence against campaign",
                               gbvagainst = "Gender-based violence against campaign",
                               migdppc_ln = "GDP per capita, logged",
                               yr_sch = "Average years of education",
                               nmembers_ln = "Campaign membership, logged"))

### INFLUENSIAL OUTLIERS ###


model.data <- augment(log2) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = bin_extentfrontline), alpha = .5) +
  scale_color_manual(values= c("coral", "#0B775E")) +
  theme_bw() +
  theme(text=element_text(family="Times", size=11))

model.data %>% 
  filter(abs(.std.resid) > 3)

cooksd <- cooks.distance(log2)

N=sum(!is.na(cooks.distance(log2)))
k=length(coefficients(log2)-1)
limit=4/(N-k-1)
limit

plot(cooksd, 
     with = model.data,
     
     col = "#0B775E",
     family = "Times New Roman",
     xlab = "Index", 
     ylab = "Cook’s distance")
abline(h=limit, 
       lty=2,
       col="coral")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/(N-k-1), names(cooksd),""), family = "Times New Roman", pos = 4)


outlier <- ifelse(cooksd < 4/nrow(log1), "keep","delete")

outlier

data_mininfluence

# Model without outlier #

data_mininfluence <- dta_reg_1[-c(72), ]


mid_inf <- glm(bin_extentfrontline ~ 
                    gi_ideol_recode +
                    regviol +
                    gbvagainst +
                    e_migdppc_ln +
                    e_miferrat +
                    yr_sch +
                    nmembers_ln,
                  family = "binomial",
                  data=data_mininfluence)

clusteredVCOVmid_inf  <- vcovCL(mid_inf , 
                                  cluster = data_mininfluence$countryvar)

mid_inf$clusterVCOV <- clusteredVCOVmid_inf 

coeftest(mid_inf, 
         vcov. = vcovCL(mid_inf, cluster = data_mininfluence$countryvar))

# Button #
stargazer(mid_inf,
          type = "latex", 
          p.auto = TRUE,
          se = list(sqrt(diag(mid_inf$clusterVCOV))),
          dep.var.labels=c("Extent of women's frontline participation"),
          model.names = FALSE, 
          font.size = "footnotesize",
          digits = 3,
          ord.intercepts = T,
          notes = "Robust standard errors clustered around country",
          notes.append = T,
          covariate.labels = c(gi_ideol_recode = "Gender inclusive ideology",
                               regviol = "Regime violence against campaign",
                               gbvagainst = "Gender-based violence against campaign",
                               migdppc_ln = "GDP per capita, logged",
                               miferrat = "Fertility rate",
                               yr_sch = "Average years of education",
                               nmembers_ln = "Campaign membership, logged"))

### REGRESSION T-1 ###

dta_reg_t1 <- subset(dta_reg, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, gbvagainst, e_migdppc_t1_ln, e_miferrat_t1, yr_sch, nmembers_ln))

dta_reg_t1 <- na.omit(dta_reg_t1, na.actio = "exclude")

t1_log1 <- glm(bin_extentfrontline ~ 
                 gi_ideol_recode +
                 regviol +
                 gbvagainst +
                 e_migdppc_t1_ln +
                 e_miferrat_t1 +
                 yr_sch +
                 nmembers_ln,
               family = "binomial",
               data=dta_reg_t1)

summary(t1_log1)

# Creating clustered robust standard errors clustered on location
clusteredVCOVt1_log1 <- vcovCL(t1_log1, 
                               cluster = dta_reg_t1$countryvar)

t1_log1$clusterVCOV <- clusteredVCOVt1_log1

coeftest(t1_log1, 
         vcov. = vcovCL(t1_log1, cluster = dta_reg_t1$countryvar))

# Button #

stargazer(t1_log1,
          p.auto = TRUE,
          se = list(sqrt(diag(t1_log1$clusterVCOV))),
          type = "latex",
          dep.var.labels=c("Extent of women's frontline participation"),
          model.names = FALSE, 
          font.size = "footnotesize",
          digits = 3,
          ord.intercepts = T,
          notes = "Robust standard errors clustered around country",
          notes.append = T,
          covariate.labels = c(gi_ideol_recode = "Gender inclusive ideology",
                               regviol = "Regime violence against campaign",
                               gbvagainst = "Gender-based violence against campaign",
                               migdppc_t1_ln = "GDP per capita, logged (t-1)",
                               miferrat_t1_ln = "Fertility rate, logged (t-1)",
                               yr_sch = "Average years of education",
                               nmembers_ln = "Campaign membership, logged"))


### COLINEARITY ###

vif <- VIF(log2)

### MISSING ###

# MICE imputation #

dta_mice <- subset(dta_reg, select = c(countryvar, bin_extentfrontline, gi_ideol_recode, regviol, 
                                       gbvagainst, e_migdppc_ln, e_miferrat, yr_sch, nmembers_ln))

dta_mice$countryvar <- as.numeric(as.factor(dta_mice$countryvar))

set.seed(123)

mice_imputation <- mice(dta_mice ,m=5,maxit=50,meth='pmm',seed=500)
summary(mice_imputation)


imputed_model <- with(mice_imputation, 
                      glm(bin_extentfrontline ~ gi_ideol_recode + regviol + gbvagainst + e_migdppc_ln + e_miferrat + 
                            yr_sch + nmembers_ln, family = binomial(link = 'logit')))

mice_model3 <- summary(pool(imputed_model))

mice_output <- xtable(mice_model3)

print(mice_output)

#### REPORTED IN APPENDIX D ####

### MULTILEVEL ALL OBS ###

p_c_1 <- ggplot(dta, mapping = aes(x = countryvar)) + 
  geom_bar(fill = "#0B775E") +
  coord_flip() +
  labs(x = "Country",
       y = "Count") +
  theme(text=element_text(family="Times", size=11))

p_c_1

