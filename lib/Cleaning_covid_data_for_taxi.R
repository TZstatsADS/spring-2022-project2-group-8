# Import the cases-by-day data
cases_by_day_masterdata <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/cases-by-day.csv")
deaths_by_day_masterdataå <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/deaths-by-day.csv")
hosps_by_day_masterdata <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/hosp-by-day.csv")

# AVERAGE CASE COUNT

# Get cases by day data (case count)
cases_by_day_data <- cases_by_day_masterdata[,c("date_of_interest", "CASE_COUNT", "BX_CASE_COUNT", "BK_CASE_COUNT",
         "MN_CASE_COUNT", "QN_CASE_COUNT", "SI_CASE_COUNT")]

# Filter the cases by day data (case count)
cases_by_day_data$Year <- substring(cases_by_day_data$date_of_interest, 7, 10)
cases_by_day_data$Month <- substring(cases_by_day_data$date_of_interest, 1, 2)
cases_by_day_data <- cases_by_day_data[,c("Year", "Month", "CASE_COUNT", "BX_CASE_COUNT", "BK_CASE_COUNT",
                                                "MN_CASE_COUNT", "QN_CASE_COUNT", "SI_CASE_COUNT")]

# Group by Year, Month and calculate mean for each borouigh
cases_by_month_boro <- cases_by_day_data %>%
  group_by(Year, Month) %>%
  dplyr::summarize(avg_case_count = mean(CASE_COUNT), 
                   avg_case_count_bx = mean(BX_CASE_COUNT),
                   avg_case_count_bk = mean(BK_CASE_COUNT), 
                   avg_case_count_mn = mean(MN_CASE_COUNT), 
                   avg_case_count_qn = mean(QN_CASE_COUNT), 
                   avg_case_count_si = mean(SI_CASE_COUNT))

# Select range we are interested in
cases_by_month_boro <- cases_by_month_boro[1:17,]

# Append 0s for the Year, Months missing since Oct 2019
cases_by_month_boro <- rbind(data.frame(Year = "2020", 
                                        Month = "01", 
                                        avg_case_count = 0,
                                        avg_case_count_bx = 0,
                                        avg_case_count_bk = 0,
                                        avg_case_count_mn = 0,
                                        avg_case_count_qn = 0,
                                        avg_case_count_si =0), cases_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
cases_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "12", 
                                        avg_case_count = 0,
                                        avg_case_count_bx = 0,
                                        avg_case_count_bk = 0,
                                        avg_case_count_mn = 0,
                                        avg_case_count_qn = 0,
                                        avg_case_count_si =0), cases_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
cases_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "11", 
                                        avg_case_count = 0,
                                        avg_case_count_bx = 0,
                                        avg_case_count_bk = 0,
                                        avg_case_count_mn = 0,
                                        avg_case_count_qn = 0,
                                        avg_case_count_si =0), cases_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
cases_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "10", 
                                        avg_case_count = 0,
                                        avg_case_count_bx = 0,
                                        avg_case_count_bk = 0,
                                        avg_case_count_mn = 0,
                                        avg_case_count_qn = 0,
                                        avg_case_count_si =0), cases_by_month_boro)

# Change year, month to numeric
cases_by_month_boro$Year <- as.numeric(cases_by_month_boro$Year)
cases_by_month_boro$Month <- as.numeric(cases_by_month_boro$Month)

# AVERAGE DEATH COUNT

# Get cases by day data (case count)
deaths_by_day_data <- deaths_by_day_masterdata[,c("date_of_interest", "DEATH_COUNT", "BX_DEATH_COUNT", "BK_DEATH_COUNT",
                                                "MN_DEATH_COUNT", "QN_DEATH_COUNT", "SI_DEATH_COUNT")]

# Filter the cases by day data (case count)
deaths_by_day_data$Year <- substring(deaths_by_day_data$date_of_interest, 7, 10)
deaths_by_day_data$Month <- substring(deaths_by_day_data$date_of_interest, 1, 2)
deaths_by_day_data <- deaths_by_day_data[,c("Year", "Month", "DEATH_COUNT", "BX_DEATH_COUNT", "BK_DEATH_COUNT",
                                          "MN_DEATH_COUNT", "QN_DEATH_COUNT", "SI_DEATH_COUNT")]

# Group by Year, Month and calculate mean for each borough
deaths_by_month_boro <- deaths_by_day_data %>%
  group_by(Year, Month) %>%
  dplyr::summarize(avg_death_count = mean(DEATH_COUNT), 
                   avg_death_count_bx = mean(BX_DEATH_COUNT),
                   avg_death_count_bk = mean(BK_DEATH_COUNT), 
                   avg_death_count_mn = mean(MN_DEATH_COUNT), 
                   avg_death_count_qn = mean(QN_DEATH_COUNT), 
                   avg_death_count_si = mean(SI_DEATH_COUNT))

# Select range we are interested in
deaths_by_month_boro <- deaths_by_month_boro[1:17,]

# Append 0s for the Year, Months missing since Oct 2019
deaths_by_month_boro <- rbind(data.frame(Year = "2020", 
                                        Month = "01", 
                                        avg_death_count = 0,
                                        avg_death_count_bx = 0,
                                        avg_death_count_bk = 0,
                                        avg_death_count_mn = 0,
                                        avg_death_count_qn = 0,
                                        avg_death_count_si =0), deaths_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
deaths_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "12", 
                                        avg_death_count = 0,
                                        avg_death_count_bx = 0,
                                        avg_death_count_bk = 0,
                                        avg_death_count_mn = 0,
                                        avg_death_count_qn = 0,
                                        avg_death_count_si =0), deaths_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
deaths_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "11", 
                                        avg_death_count = 0,
                                        avg_death_count_bx = 0,
                                        avg_death_count_bk = 0,
                                        avg_death_count_mn = 0,
                                        avg_death_count_qn = 0,
                                        avg_death_count_si =0), deaths_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
deaths_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "10", 
                                        avg_death_count = 0,
                                        avg_death_count_bx = 0,
                                        avg_death_count_bk = 0,
                                        avg_death_count_mn = 0,
                                        avg_death_count_qn = 0,
                                        avg_death_count_si =0), deaths_by_month_boro)

# Change year, month to numeric
deaths_by_month_boro$Year <- as.numeric(deaths_by_month_boro$Year)
deaths_by_month_boro$Month <- as.numeric(deaths_by_month_boro$Month)

# AVERAGE HOSP COUNT

# Get cases by day data (case count)
hosps_by_day_data <- hosps_by_day_masterdata[,c("date_of_interest", "HOSPITALIZED_COUNT", "BX_HOSPITALIZED_COUNT", "BK_HOSPITALIZED_COUNT",
                                                  "MN_HOSPITALIZED_COUNT", "QN_HOSPITALIZED_COUNT", "SI_HOSPITALIZED_COUNT")]

# Filter the cases by day data (case count)
hosps_by_day_data$Year <- substring(hosps_by_day_data$date_of_interest, 7, 10)
hosps_by_day_data$Month <- substring(hosps_by_day_data$date_of_interest, 1, 2)
hosps_by_day_data <- hosps_by_day_data[,c("Year", "Month", "HOSPITALIZED_COUNT", "BX_HOSPITALIZED_COUNT_COUNT", "BK_HOSPITALIZED_COUNT_COUNT",
                                          "MN_HOSPITALIZED_COUNT_COUNT", "QN_HOSPITALIZED_COUNT_COUNT", "SI_HOSPITALIZED_COUNT_COUNT")]

# Group by Year, Month and calculate mean for each borough
hosps_by_month_boro <- hosps_by_day_data %>%
  group_by(Year, Month) %>%
  dplyr::summarize(avg_hosp_count = mean(HOSPITALIZED_COUNT), 
                   avg_hosp_count_bx = mean(BX_HOSPITALIZED_COUNT),
                   avg_hosp_count_bk = mean(BK_HOSPITALIZED_COUNT), 
                   avg_hosp_count_mn = mean(MN_HOSPITALIZED_COUNT), 
                   avg_hosp_count_qn = mean(QN_HOSPITALIZED_COUNT), 
                   avg_hosp_count_si = mean(SI_HOSPITALIZED_COUNT))

# Select range we are interested in
hosps_by_month_boro <- hosps_by_month_boro[1:17,]

# Append 0s for the Year, Months missing since Oct 2019
hosps_by_month_boro <- rbind(data.frame(Year = "2020", 
                                         Month = "01", 
                                         avg_hosp_count = 0,
                                         avg_hosp_count_bx = 0,
                                         avg_hosp_count_bk = 0,
                                         avg_hosp_count_mn = 0,
                                         avg_hosp_count_qn = 0,
                                         avg_hosp_count_si =0), hosps_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
hosps_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "12", 
                                        avg_hosp_count = 0,
                                        avg_hosp_count_bx = 0,
                                        avg_hosp_count_bk = 0,
                                        avg_hosp_count_mn = 0,
                                        avg_hosp_count_qn = 0,
                                        avg_hosp_count_si =0), hosps_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
hosps_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "11", 
                                        avg_hosp_count = 0,
                                        avg_hosp_count_bx = 0,
                                        avg_hosp_count_bk = 0,
                                        avg_hosp_count_mn = 0,
                                        avg_hosp_count_qn = 0,
                                        avg_hosp_count_si =0), hosps_by_month_boro)

# Append 0s for the Year, Months missing since Oct 2019
hosps_by_month_boro <- rbind(data.frame(Year = "2019", 
                                        Month = "10", 
                                        avg_hosp_count = 0,
                                        avg_hosp_count_bx = 0,
                                        avg_hosp_count_bk = 0,
                                        avg_hosp_count_mn = 0,
                                        avg_hosp_count_qn = 0,
                                        avg_hosp_count_si =0), hosps_by_month_boro)

# Change year, month to numeric
hosps_by_month_boro$Year <- as.numeric(hosps_by_month_boro$Year)
hosps_by_month_boro$Month <- as.numeric(hosps_by_month_boro$Month)

# Merge all the data 
covid_data_by_month_boro <- merge(cases_by_month_boro, deaths_by_month_boro)
covid_data_by_month_boro <- merge(covid_data_by_month_boro, hosps_by_month_boro)

# Export
write.csv(covid_data_by_month_boro, "../data/Taxi/cleaned/covid_data_by_month_boro.csv")