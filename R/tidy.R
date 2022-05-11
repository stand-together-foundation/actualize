# library(tidycensus)
# library(tidyverse)
# library(lubridate)
# library(labelled)
#
# sample_age <- function(
#
#   table = NA,
#   age_col = NA,
#   alpha = 1.25,
#   beta = 65,
#   missing_codes = "",
#   upper_suffix = "\\+"
#
# ){
#
#   age_col_name <- tidyselect::enquo(age_col)
#
#   dat <- table %>%
#     tidyr::separate(
#       !!age_col_name,
#       fill = "left",extra = "merge",
#       remove = FALSE,
#       sep = '(\\s-\\s|\\sto\\s|[–]|[-]|[-])',
#       into = c("AgeBoundLow", "AgeBoundHigh")
#     ) %>%
#     dplyr::mutate(
#       dplyr::across(
#         .cols = dplyr::starts_with("AgeBound"),
#         .fns = readr::parse_number
#       ), AgeBoundLow = dplyr::case_when(
#         stringr::str_ends(
#           !!age_col_name,
#           upper_suffix,
#           negate = TRUE
#         ) ~ AgeBoundLow,
#         stringr::str_ends(
#           !!age_col_name,
#           upper_suffix,
#           negate = FALSE
#         ) ~ AgeBoundHigh
#       ),
#       AgeBoundHigh = dplyr::case_when(
#         stringr::str_ends(
#           !!age_col_name,
#           upper_suffix,
#           negate = TRUE
#         ) ~ AgeBoundHigh,
#         stringr::str_ends(
#           !!age_col_name,
#           upper_suffix,
#           negate = FALSE
#         ) ~ beta*alpha)) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       Age = runif(
#         n = 1,
#         min = AgeBoundLow,
#         max = AgeBoundHigh
#       ),
#       Age = round(!!age_col_name, 0)
#     ) %>%
#     dplyr::ungroup()
#   return(dat)
# }
#
# sample_income <- function(
#   table = NA,
#   income_col = NA,
#   alpha = 2,
#   beta = 160000,
#   missing_codes = "Refused",
#   upper_suffix = "or higher",
#   lower_prefix = "Less than"
# ){
#
#   income_col_name <- tidyselect::enquo(income_col)
#
#   dat <- table %>%
#     separate(
#       !!income_col_name,
#       fill = "left",
#       remove = FALSE,
#       sep = glue::glue(
#         '(\\s-\\s|\\sto\\s|',
#         {gsub(" ","\\\\s",lower_prefix)},
#         '|',
#         {paste0(
#           gsub(" ","\\\\s",missing_codes),
#           collapse = '|'
#         )},
#         ')'
#       ),
#       into = c("IncomeBoundLow", "IncomeBoundHigh")
#     ) %>%
#     mutate(
#       across(
#         .cols = starts_with("IncomeBound"),
#         .fns = parse_number
#       ),
#       IncomeBoundLow = case_when(
#         stringr::str_starts(
#           !!income_col_name,
#           lower_prefix,
#           negate = TRUE
#         ) ~ IncomeBoundLow,
#         stringr::str_starts(
#           !!income_col_name,
#           lower_prefix,
#           negate = FALSE
#         ) ~ 0
#       ),
#       IncomeBoundLow = case_when(
#         stringr::str_ends(
#           !!income_col_name,
#           upper_suffix,
#           negate = TRUE
#         ) ~ IncomeBoundLow,
#         stringr::str_ends(
#           !!income_col_name,
#           upper_suffix,
#           negate = FALSE
#         ) ~ IncomeBoundHigh
#       ),
#       IncomeBoundHigh = case_when(
#         stringr::str_ends(
#           !!income_col_name,
#           upper_suffix,
#           negate = TRUE
#         ) ~ IncomeBoundHigh,
#         stringr::str_ends(
#           !!income_col_name,
#           upper_suffix,
#           negate = FALSE
#         ) ~ beta*(alpha/(alpha-1))
#       )
#     ) %>%
#     rowwise() %>%
#     mutate(
#       IncomeSample1 = runif(
#         n = 1,
#         min = IncomeBoundLow,
#         max = IncomeBoundHigh
#       ),
#       IncomeSample2 = runif(
#         n = 1,
#         min = IncomeBoundLow,
#         max = IncomeBoundHigh
#       ),
#       IncomeSample3 = runif(
#         n = 1,
#         min = IncomeBoundLow,
#         max = IncomeBoundHigh
#       ),
#       IncomeSample = (IncomeSample1 + IncomeSample2 + IncomeSample3)/3
#     ) %>%
#     ungroup()
#   return(dat)
# }
#
# library(digest)
#
# PEOPLE_DIM_national1 <- national1 %>%
#   select(
#     UID,
#     Age,
#     Race,
#     Gender
#   ) %>%
#   sample_age() %>%
#   mutate(
#     vendor = "i360",
#     survey_date = lubridate::ymd(20200301),
#     respondent_id = openssl::md5(
#       paste0(UID, vendor, survey_date)
#     ),
#     Hispanic = case_when(
#       Race == "Hispanic" ~ "Yes",
#       TRUE ~ "No"
#     ),
#     Race = case_when(
#       Race == "Hispanic" ~ "Other",
#       Race == "Unknown" ~ NA_character_,
#       TRUE ~ Race
#     )
#   ) %>%
#   group_by(
#     respondent_id
#   ) %>%
#   summarise(
#     Age = max(Age),
#     Race = min(Race, na.rm = TRUE),
#     Hispanic = max(Hispanic),
#     Gender = max(Gender)
#   )
#
# PEOPLE_DIM_national2 <- national2 %>%
#   select(
#     caseid,
#     birthyr,
#     race,
#     Q2,
#     hispanic_origin_1,
#     hispanic_origin_2,
#     hispanic_origin_3,
#     hispanic_origin_4
#   ) %>%
#   to_factor() %>%
#   mutate(
#     vendor = "YouGov",
#     survey_date = ymd(20211215),
#     respondent_id = openssl::md5(
#       paste0(caseid, vendor, survey_date)
#     ),
#     Age = trunc((ymd(paste0(birthyr,"0701")) %--% ymd(20211215)) / years(1)),
#     race_char = as.character(race),
#     Race = case_when(
#       race_char == "Middle Eastern" ~ "Other",
#       race_char == "Two or more races" ~ "Other",
#       race_char == "Native American" ~ "Other",
#       race_char == "Hispanic" ~ "Other",
#       race_char == "not asked" ~ NA_character_,
#       race_char == "skipped" ~ NA_character_,
#       TRUE ~ race_char
#     ),
#     Hispanic = case_when(
#       hispanic_origin_1 == "selected" ~ "Yes",
#       hispanic_origin_2 == "selected" ~ "Yes",
#       hispanic_origin_3 == "selected" ~ "Yes",
#       hispanic_origin_4 == "selected" ~ "Yes",
#       TRUE ~ "No"
#     ),
#     Gender = case_when(
#       Q2 == "Male" ~ "M",
#       Q2 == "Female" ~ "F",
#       Q2 %in% c("Non-binary", "Other",
#                 "Prefer not to say",
#                 "skipped", "not asked") ~ NA_character_
#     )
#   ) %>%
#   select(
#     respondent_id,
#     Age,
#     Race,
#     Hispanic,
#     Gender
#   )
#
# PEOPLE_DIM_local1 <- local1 %>%
#   select(
#     sessionid,
#     gender,
#     age_raw,
#     ends_with("survey_race")
#   ) %>%
#   mutate(
#     vendor = "i360",
#     survey_date = ymd(20211215),
#     respondent_id = openssl::md5(
#       paste0(sessionid, vendor, survey_date)
#     )
#   ) %>%
#   rowwise() %>%
#   mutate(
#     race_count = sum(!is.na(c_across(ends_with("survey_race"))))
#   ) %>%
#   pivot_longer(
#     cols =  ends_with("survey_race"), values_to = "race"
#   ) %>%
#   mutate(
#     Hispanic = case_when(
#       race == "Hispanic or Latino" ~ "Yes",
#       TRUE ~ "No"
#     ),
#     Race = case_when(
#       race == "Hispanic or Latino" ~ "Other",
#       race == "Black or African American" ~ "Black",
#       race == "Asian or Asian-American" ~ "Asian",
#       race == "Middle Eastern" ~ "Other",
#       race == "Native American" ~ "Other",
#       race == "Other" ~ "Other",
#       race == "White" ~ "White"
#     )
#   ) %>%
#   group_by(
#     respondent_id
#   ) %>%
#   summarise(
#     Age = max(age_raw),
#     Race = min(Race, na.rm = TRUE),
#     Hispanic = max(Hispanic),
#     Gender = max(gender)
#   ) %>%
#   select(
#     respondent_id,
#     Age,
#     Race,
#     Hispanic,
#     Gender
#   )
#
# PEOPLE_DIM <- bind_rows(
#   PEOPLE_DIM_local1,
#   PEOPLE_DIM_national1,
#   PEOPLE_DIM_national2
# )
#
#
#
#
#
#
# integrated_data <- bind_rows(
#   national1 %>%
#     filter(
#       questionnumber == "q3"
#     ) %>%
#     select(
#       Answer2,
#       State_abbr = State,
#       populationdesc,
#       UID,
#       Age,
#       Race,
#       Gender,
#       `Household Income (group)`
#     ) %>%
#     sample_income(
#       table = .,
#       income_col = `Household Income (group)`,
#       alpha = 2,
#       beta = 160000,
#       missing_codes = "Refused",
#       upper_suffix = "or higher",
#       lower_prefix = "Less than"
#     ) %>%
#     left_join(
#       tibble(
#         State = state.name,
#         State_abbr = state.abb
#       ),
#       by = "State_abbr"
#     ) %>%
#     mutate(
#       Period = "2020, Spring",
#       Locality = case_when(
#         populationdesc == "Zip Code 49507 Adults" ~ "Grand Rapids",
#         populationdesc == "Grand Rapids Adults" ~ "Grand Rapids",
#         TRUE ~ "Outside of Grand Rapids"
#       ),
#       survey_id = as.character(UID),
#       `Cantril Present` = as.numeric(Answer2),
#       Source = "i360, Spring 2020"
#     ) %>%
#     select(
#       survey_id,
#       `Cantril Present`,
#       State,
#       Locality,
#       Period,
#       IncomeSample,
#       Source
#     ),
#   national2 %>%
#     select(
#       `Cantril Present` = Q14,
#       State = inputstate,
#       caseid,
#       faminc_new
#     ) %>%
#     to_factor() %>%
#     sample_income(
#       table = .,
#       income_col = faminc_new,
#       alpha = 2,
#       beta = 500000,
#       missing_codes = c("Prefer not to say",
#                         "skipped",
#                         "not asked"),
#       upper_suffix = "or more",
#       lower_prefix = "Less than"
#     ) %>%
#     mutate(
#       Period = "2021, Late Fall",
#       Locality = "Outside of Grand Rapids",
#       survey_id = as.character(caseid),
#       `Cantril Present` = as.numeric(`Cantril Present`),
#       Source = "YouGov, Fall 2021"
#     ) %>%
#     select(
#       survey_id,
#       `Cantril Present`,
#       State,
#       Locality,
#       Period,
#       IncomeSample,
#       Source
#     ),
#
#   local1 %>%
#     select(
#       `Cantril Present` = cantril_present,
#       zip5,
#       geography,
#       sessionid,
#       hh_income,
#       ends_with("survey_race")
#     ) %>%
#     sample_income(
#       table = .,
#       income_col = hh_income,
#       alpha = 2,
#       beta = 250000,
#       missing_codes = c("Refused"),
#       upper_suffix = "and above",
#       lower_prefix = "Less than"
#     ) %>%
#     left_join(
#       ZipCodes %>%
#         mutate(
#           zip = as.character(zip)
#         ),
#       c("zip5" = "zip")
#     ) %>%
#     mutate(
#       Period = "2021, Late Fall",
#       Locality = case_when(
#         geography == "Grand Rapids" ~ "Grand Rapids",
#         TRUE ~ "Outside of Grand Rapids"
#       ),
#       survey_id = as.character(sessionid),
#       `Cantril Present` = as.numeric(`Cantril Present`),
#       Source = "i360, Fall 2021"
#     ) %>%
#     rowwise() %>%
#     mutate(
#       race_count = sum(!is.na(c_across(ends_with("survey_race"))))
#     ) %>%
#     pivot_longer(
#       cols =  ends_with("survey_race"), values_to = "race"
#     ) %>%
#     fill(
#       is_hispanic,
#       .direction = "downup"
#     ) %>%
#     ungroup() %>%
#     mutate(
#       race = case_when(
#         race == "Hispanic or Latino" ~ "Other",
#         race != "Hispanic or Latino" ~ race
#       )
#     ) %>%
#     filter(
#       !is.na(race)
#     ) %>%
#     group_by(
#       sessionid
#     ) %>%
#     arrange(
#       desc(is_hispanic), race, .group_by = TRUE
#     ) %>%
#     distinct(
#       survey_id,
#       .keep_all = TRUE
#     ) %>%
#     mutate(
#       survey_hash = digest::digest(c(survey_id, source), algo="md5", serialize=TRUE),
#       race = case_when(
#         race_count > 1 ~ "Two or More Races",
#         race == "Hispanic or Latino" ~ "Other",
#         race_count == 1 ~ race,
#         race_count == 0 ~ NA_character_
#       )
#     ) %>%
#     select(
#       survey_hash,
#       race,
#       race_count,
#       is_hispanic,
#       `Cantril Present`,
#       State = AdminName1,
#       Locality,
#       Period,
#       Income,
#       Source
#     ) -> test
#
#
#
#
#   #%>%
#   inner_join(
#     fips_codes %>%
#       select(
#         state, state_name
#       ) %>%
#       distinct(),
#     by = c("State" = "state_name")
#   )
#
#   ggplot(integrated_data, aes(x=IncomeSample, color=Source, fill=Source)) +
#     geom_histogram(aesfe(y=..density..), position="identity", alpha=0.5, bins = 25) +
#     geom_density(alpha=0.6) +
#     geom_vline(data=mu, aes(xintercept=grp.mean, color=Source),
#                linetype="dashed")
#
#   g <- ggplot(integrated_data %>% filter(!is.na(Income)), aes(Income))
#   g + geom_bar()
#
#   acs5_income <- get_acs(
#     geography = "state",
#     variables = c(medincome = "B19013_001"),
#     year = 2020
#   ) %>%
#     mutate(
#       tile3 = ntile(estimate, 3),
#       `State Wealth` = case_when(
#         tile3 == 3 ~ "High Income State",
#         tile3 == 2 ~ "Middle Income State",
#         tile3 == 1 ~ "Low Income State"
#       )
#     )
#
#   present_cantril_median_by_state <- left_join(
#     integrated_data,
#     acs5_income,
#     by = c("State" = "NAME")
#   ) %>%
#     select(
#       `Cantril Present`,
#       State,
#       Locality,
#       Period,
#       `State Wealth`,
#       GEOID
#     ) %>%
#     mutate(
#       current = ifelse(
#         `Cantril Present` >= 7,
#         TRUE,
#         FALSE
#       )
#     ) %>%
#     group_by(
#       State, Period
#     ) %>%
#     summarise(
#       `Present Cantril` = median(
#         `Cantril Present`,
#         na.rm = TRUE
#       )
#     )
#
#   present_cantril_median_by_state_gr <- left_join(
#     integrated_data,
#     acs5_income,
#     by = c("State" = "NAME")
#   ) %>%
#     select(
#       `Cantril Present`,
#       State,
#       Locality,
#       Period,
#       `State Wealth`,
#       GEOID
#     ) %>%
#     mutate(
#       current = ifelse(
#         `Cantril Present` >= 7,
#         TRUE,
#         FALSE
#       )
#     ) %>%
#     filter(Locality == "Grand Rapids",
#            State == "Michigan") %>%
#     group_by(
#       State, Period
#     ) %>%
#     summarise(
#       `Present Cantril` = median(
#         `Cantril Present`,
#         na.rm = TRUE
#       )
#     ) %>%
#     mutate(
#       State = "Grand Rapids"
#     )
#
#
#   present_cantril_median_by_income <- left_join(
#     present_cantril_median_by_state,
#     acs5_income,
#     by = c("State" = "NAME")
#   ) %>%
#     select(
#       `Present Cantril`,
#       State,
#       `State Wealth`,
#       Period
#     ) %>%
#     mutate(
#       current_positive = ifelse(
#         `Present Cantril` >= 7,
#         TRUE,
#         FALSE
#       )
#     ) %>%
#     group_by(
#       `State Wealth`, Period
#     ) %>%
#     summarise(
#       `Present Cantril` = sum(current_positive, na.rm = TRUE)/n()
#     ) %>%
#     filter(!is.na(`State Wealth`))
#
#   ggplot(bind_rows(
#     present_cantril_median_by_income,
#     present_cantril_median_by_income_gr
#   ), aes(x = Period, y = `Present Cantril`,
#          group = `State Wealth`,
#          colour = `State Wealth`)) +
#     geom_line()
#
#   library(rgdal)
#   library(rgeos)
#
#   url = "https://gist.githubusercontent.com/hrbrmstr/51f961198f65509ad863/raw/219173f69979f663aa9192fbe3e115ebd357ca9f/us_states_hexgrid.geojson"
#
#   us <- readOGR(
#     dsn = url
#   )
#
#   us@data <- us@data %>%
#     left_join(
#       integrated_data %>%
#         group_by(state) %>%
#         summarise(respondent_cnt = n()),
#       by = c("iso3166_2" = "state")
#     ) %>%
#     mutate(
#       id = iso3166_2
#     )
#
#   centers <- bind_cols(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
#
#   us_map <- fortify(us, region = "iso3166_2")
#
#   ggplot() +
#     geom_map(
#       data = us_map,
#       map = us_map,
#       aes(x=long, y=lat, map_id=id),
#       color="white", size=0.5) +
#     coord_map() +
#     theme_bw() +
#     theme(panel.border=element_blank()) +
#     theme(panel.spacing=unit(3, "lines")) +
#     theme(panel.grid=element_blank()) +
#     theme(axis.ticks=element_blank()) +
#     theme(axis.text=element_blank()) +
#     theme(strip.background=element_blank()) +
#     theme(strip.text=element_text(face="bold", hjust=0, size=14)) +
#     theme(legend.position=c(.75, 0)) +
#     theme(legend.direction="vertical") +
#     theme(legend.title.align=1) +
#     ylab("") +
#     xlab("") +
#     ggtitle("This is a map") +
#     geom_map(data=us@data, map=us_map,
#              aes(fill=respondent_cnt, map_id=id)) +
#     scale_fill_distiller(name="Respondent\nCount", palette="BuPu", na.value="#7f7f7f") +
#     geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=4)
#
#
#
#   # Plot filled polygons ----------------------------------------------------
#
#   colour_breaks <- c(.05, .5, .95)
#   max_value <- 1
#   current_max <- max(present_cantril_by_state$`Present Cantril`)
#   relative_max <- max_value / current_max
#
#
#   gg <- gg + geom_map(
#     data=present_cantril_by_state,
#     map=us_map,
#     aes(fill=`Present Cantril`, map_id=GEOID)
#   ) +
#     scale_fill_gradientn(limits  = range(0, 1),
#                          colours = colours,
#                          values = scales::rescale(colour_breaks, to = c(0, relative_max), from = c(0, max_value)),
#                          name="Deaths",na.value="#101010")
#
#   # Remove chart junk for the “map" -----------------------------------------
#
#   gg <- gg + labs(x=NULL, y=NULL)
#   gg <- gg + theme_bw()
#   gg <- gg + theme(panel.border=element_blank())
#   gg <- gg + theme(panel.grid=element_blank())
#   gg <- gg + theme(axis.ticks=element_blank())
#   gg <- gg + theme(axis.text=element_blank())
#
#
#
#   %>%
#     ggplot(
#       data = .,
#
#     )
#
#
#
#
#
#
#
#   # [837] "On which step did you stand one year ago?"
#   # [945] "On which step of the ladder would you say you personally feel you stand at this time?"
#   # [954] "On which step do you think you will stand about five years from now?"
