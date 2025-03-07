# Requirements and setwd
list.of.packages <- c("data.table", "Hmisc", "reshape2", "splitstackshape", "httr", "rvest", "lsa", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("/git/MD-Innovation-Team-Interview/")

# Function for Calculating Maryland FY
maryland_fy = function(date_col, numeric=F){
  date_months = month(date_col)
  date_years = year(date_col)
  fy_adjustment = (date_months <= 6) * 1
  fy_start = date_years - fy_adjustment
  fy_end = fy_start + 1
  fy_string = paste(fy_start, fy_end, sep="-")
  if(numeric){
    return(fy_start)
  }
  return(fy_string)
}

# Weekly Claims and Extended Benefits Trigger Data ####
# Instructions https://www.dol.gov/sites/dolgov/files/ETA/handbooks/2017/ETHand401_5th.pdf#page=33
# These data refer to claims activity under the state UI and Federal UCFE (federal emp) and UCX (ex-service member) programs
# (refer to Introduction, section K for program classification). When applicable, data pertaining to
# Federal/State extended benefit (EB) activity should be reported as identified in items 11 and 12. When
# appropriate, data pertaining to state additional benefits programs should be reported as identified in items 13
# and 14. States which have an STC (short-term comp) or workshare program should report appropriate data in items 4, 5, 9, and
# 10. If a particular program does not apply to your state, those items should be zero filled.
if(!file.exists("input/ar539.csv")){
  ar539_url = "https://oui.doleta.gov/unemploy/csv/ar539.csv"
  ar539 = fread(ar539_url)
  
  # Codenames via https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA539
  ar539_colnames = c(
    "st"="state",
    "rptdate"="report_for_period_ending",
    "c1"="week_number",
    "c2"="reflected_week_ending",
    "c3"="ic", # State UI Initial Claims, less intrastate transitional
    "c4"="fic", # UCFE-no UI Initial Claims.
    "c5"="xic", # UCX only Initial Claims
    "c6"="wsic", # STC or workshare total initial claims
    "c7"="wseic", # STC or workshare equivalent initial claims
    "c8"="cw", # State UI adjusted continued weeks claimed
    "c9"="fcw", # UCFE-no UI adjusted continued weeks claimed
    "c10"="xcw", # UCX only adjusted continued weeks claimed
    "c11"="wscw", # STC or workshare total continued weeks claimed
    "c12"="wsecw", # STC or workshare equivalent continued weeks claimed
    "c13"="ebt", # Total continued weeks claimed under the Federal/State Extended Benefit Program--includes all intrastate and interstate continued weeks claimed filed from an agent state under the state UI, UCFE and UCX programs.
    "c14"="ebui", # That part of EBT which represents only state UI weeks claimed under the Federal/State EB program.
    "c15"="abt", # Total continued weeks claimed under a state additional benefit program for those states which have such a program. (Includes UCFE and UCX.)
    "c16"="abui", # That part of ABT which represents only state UI additional continued weeks claimed for those states which have such a program.
    "c17"="at", # Average adjusted Total Continued Weeks Claimed. (13 week average)
    "c18"="ce", # Covered Employment. Report the 12-month average monthly covered employment for the first 4 of the last 6 completed calendar quarters prior to the end of the last week of the current 13-week period to which the insured unemployment data relate. This figure will change only once each quarter, as specified in section F.9. above.
    "c19"="r", # Rate of Insured Unemployment. Report the rate of insured unemployment for the current 13-week period. The insured unemployment rate for a 13-week period is the result of dividing the average adjusted total continued weeks claimed (AT) by the covered employment (CE);
    "c20"="ar", # Average Rate of Insured Unemployment in Prior Two Years. Report the average of the rates of insured unemployment for the corresponding 13-week periods in the prior 2 years (See E.6. above). The average rates in each of the 2 prior years is computed as the sum of the two rates divided by 2
    "c21"="p", # Current Rate as Percent of Average Rate in Prior Two Years. P = R / AR
    "c22"="status", # Indicate the beginning (B) or ending (E) of a state extended benefit period.
    "c23"="status_change_date" # If Status has changed since the prior week, enter the date the change is effective.
  )
  names(ar539)[which(names(ar539) %in% names(ar539_colnames))] =
    ar539_colnames[names(ar539)[which(names(ar539) %in% names(ar539_colnames))]]
  
  # Parse dates
  ar539$report_for_period_ending = as.Date(
    ar539$report_for_period_ending, format="%m/%d/%Y"
  )
  ar539$reflected_week_ending = as.Date(
    ar539$reflected_week_ending, format="%m/%d/%Y"
  )
  ar539$status_change_date = as.Date(
    ar539$status_change_date, format="%m/%d/%Y"
  )
  
  # Subset to relevant dates
  ar539 = subset(ar539, year(reflected_week_ending) >= 2019)
  ar539$md_fy_num = maryland_fy(ar539$reflected_week_ending, numeric=T)
  ar539$md_fy_str = maryland_fy(ar539$reflected_week_ending, numeric=F)
  
  # Some EDA
  # ar539_md = subset(ar539, state=="MD")
  # plot(ic~reflected_week_ending, data=ar539_md, type="l")
  # abline(v=unique(ar539_md$status_change_date), col="red")
  
  # Write
  fwrite(
    ar539,
    "input/ar539.csv"
  )
}

# Time Lapse For All First Payments Excluding Workshare ####
# Instructions https://www.dol.gov/sites/dolgov/files/ETA/handbooks/2017/ETHand401_5th.pdf#page=226
# Enter in each column and time lapse interval all first payments made during the report period for Intrastate
# and Interstate claims. Workshare first payments are reported separately.
if(!file.exists("input/ar9050.csv")){
  ar9050_url = "https://oui.doleta.gov/unemploy/csv/ar9050.csv"
  ar9050 = fread(ar9050_url)
  
  # Codenames via https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA9050
  ar9050_colnames = c(
    "st"="state",
    "rptdate"="report_for_period_ending"
  )
  ar9050_c_name_cols = c(
    "intra-total",
    "intra-ui",
    "intra-ucfe",
    "intra-ucx",
    "inter-total",
    "inter-ui",
    "inter-ucfe",
    "inter-ucx"
  )
  ar9050_c_name_rows = c(
    "total",
    "0-7",
    "8-14",
    "15-21",
    "22-28",
    "29-35",
    "36-42",
    "43-49",
    "50-56",
    "57-63",
    "64-70",
    "71-+"
  )
  c_index = 1
  for(row in ar9050_c_name_rows){
    for(col in ar9050_c_name_cols){
      colname = paste(col, row, sep="_")
      c_code = paste0("c", c_index)
      ar9050_colnames[c_code] = colname
      c_index = c_index + 1
    }
  }
  names(ar9050)[which(names(ar9050) %in% names(ar9050_colnames))] =
    ar9050_colnames[names(ar9050)[which(names(ar9050) %in% names(ar9050_colnames))]]
  
  # Parse dates
  ar9050$report_for_period_ending = as.Date(
    ar9050$report_for_period_ending, format="%m/%d/%Y"
  )
  
  # Subset to relevant dates
  ar9050 = subset(ar9050, year(report_for_period_ending) >= 2019)
  
  # Reshape
  ar9050_long = melt(ar9050, id.vars=c("state", "report_for_period_ending"))
  ar9050_long = cSplit(ar9050_long, splitCols="variable", sep="_")
  ar9050_wide = dcast(ar9050_long, state+report_for_period_ending+variable_1~variable_2)
  
  # Transform
  ar9050_wide$`14days_or_fewer` = ar9050_wide$`0-7` + ar9050_wide$`8-14`
  ar9050_wide$`21days_or_fewer` = ar9050_wide$`0-7` + ar9050_wide$`8-14` + ar9050_wide$`15-21`
  # Via https://oui.doleta.gov/unemploy/content/sigpros/2010-2019/January2017.pdf
  # Non-waiting week states use 14 days
  non_waiting_week_states = c(
    "CT", "DE", "GA", "IA", "KY","MD", "MI", "MO", "NV", "NJ", "TN", "TX", "WY"
  )
  ar9050_wide$non_waiting_week = ar9050_wide$state %in% non_waiting_week_states
  ar9050_wide$first_payment_promptness = ar9050_wide$`21days_or_fewer` / ar9050_wide$total
  ar9050_wide$first_payment_promptness[which(ar9050_wide$state %in% non_waiting_week_states)] = 
    ar9050_wide$`14days_or_fewer`[which(ar9050_wide$state %in% non_waiting_week_states)] / 
    ar9050_wide$total[which(ar9050_wide$state %in% non_waiting_week_states)]
  ar9050_wide$first_payment_promptness[which(is.nan(ar9050_wide$first_payment_promptness))] = 0
  ar9050_wide$md_fy_num = maryland_fy(ar9050_wide$report_for_period_ending, numeric=T)
  ar9050_wide$md_fy_str = maryland_fy(ar9050_wide$report_for_period_ending, numeric=F)
  setnames(ar9050_wide, "variable_1", "indicator_type")
  
  # EDA
  # ar9050_md = subset(ar9050_wide, state=="MD" & indicator_type=="intra-total")
  # plot(first_payment_promptness~report_for_period_ending, data=ar9050_md, type="l")
  # ar9050_md_2024 = subset(ar9050_md, year(report_for_period_ending)==2024)
  # sum(ar9050_md_2024$`21days_or_fewer`) / sum(ar9050_md_2024$total)
  
  # Write
  fwrite(
    ar9050_wide,
    "input/ar9050.csv"
  )
}


# Nonmonetary Determination Time Lapse, Detection Date ####
# Instructions https://www.dol.gov/sites/dolgov/files/ETA/handbooks/2017/ETHand401_5th.pdf#page=243
# Enter in each column and time lapse interval the number of nonmonetary determinations made during the
# report period representing the number of days from the date an issue is first detected on a claim to the date on
# the determination.
if(!file.exists("input/ar9052.csv")){
  ar9052_url = "https://oui.doleta.gov/unemploy/csv/ar9052.csv"
  ar9052 = fread(ar9052_url)
  
  # Codenames via https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA9052
  ar9052_colnames = c(
    "st"="state",
    "rptdate"="report_for_period_ending"
  )
  ar9052_c_names_table_types = c(
    "separation",
    "nonseparation"
  )
  ar9052_c_name_cols = c(
    "intra-total",
    "intra-ui",
    "intra-ucfe",
    "intra-ucx",
    "inter-total",
    "inter-ui",
    "inter-ucfe",
    "inter-ucx"
  )
  ar9052_c_name_rows = c(
    "total",
    "0-7",
    "8-14",
    "15-21",
    "22-28",
    "29-35",
    "36-42",
    "43-49",
    "50-56",
    "57-63",
    "64-70",
    "71-+"
  )
  ar9052_c_name_claimant_cols = c(
    "total",
    "multiClaimantLaborDispute",
    "multiClaimantOther"
  )
  c_index = 1
  for(table_type in ar9052_c_names_table_types){
    for(row in ar9052_c_name_rows){
      for(col in ar9052_c_name_cols){
        colname = paste(table_type, col, row, sep="_")
        c_code = paste0("c", c_index)
        ar9052_colnames[c_code] = colname
        c_index = c_index + 1
      }
    }
  }
  for(row in ar9052_c_name_rows){
    for(col in ar9052_c_name_claimant_cols){
      colname = paste("issues", col, row, sep="_")
      c_code = paste0("c", c_index)
      ar9052_colnames[c_code] = colname
      c_index = c_index + 1
    }
  }
  
  names(ar9052)[which(names(ar9052) %in% names(ar9052_colnames))] =
    ar9052_colnames[names(ar9052)[which(names(ar9052) %in% names(ar9052_colnames))]]
  
  # Parse dates
  ar9052$report_for_period_ending = as.Date(
    ar9052$report_for_period_ending, format="%m/%d/%Y"
  )
  
  # Subset to relevant dates
  ar9052 = subset(ar9052, year(report_for_period_ending) >= 2019)
  
  # Reshape
  ar9052_long = melt(ar9052, id.vars=c("state", "report_for_period_ending"))
  ar9052_long = cSplit(ar9052_long, splitCols="variable", sep="_")
  ar9052_long = subset(ar9052_long, variable_1!="issues")
  ar9052_wide = dcast(
    ar9052_long, 
    state+report_for_period_ending+variable_2~variable_3,
    fun.aggregate = sum # Add separation and non-separation
  )
  
  # Transform
  ar9052_wide$`21days_or_fewer` = ar9052_wide$`0-7` + ar9052_wide$`8-14` + ar9052_wide$`15-21`
  ar9052_wide$nonmonetary_determination_time_lapse = ar9052_wide$`21days_or_fewer` / ar9052_wide$total
  ar9052_wide$nonmonetary_determination_time_lapse[which(is.nan(ar9052_wide$nonmonetary_determination_time_lapse))] = 0
  ar9052_wide$md_fy_num = maryland_fy(ar9052_wide$report_for_period_ending, numeric=T)
  ar9052_wide$md_fy_str = maryland_fy(ar9052_wide$report_for_period_ending, numeric=F)
  setnames(ar9052_wide, "variable_2", "indicator_type")
  
  # EDA
  # ar9052_md = subset(ar9052_wide, state=="MD" & indicator_type=="intra-total")
  # plot(nonmonetary_determination_time_lapse~report_for_period_ending, data=ar9052_md, type="l")
  
  # Write
  fwrite(
    ar9052_wide,
    "input/ar9052.csv"
  )
}


# Quality scrape ####
# curl 'https://oui.doleta.gov/unemploy/ranking/rankingrpt.asp' \
# -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7' \
# -H 'Accept-Language: en-US,en;q=0.9' \
# -H 'Cache-Control: no-cache' \
# -H 'Connection: keep-alive' \
# -H 'Content-Type: application/x-www-form-urlencoded' \
# -H 'Cookie: cookieOUI=cookieOUIValue; mycookie=cookie_value' \
# -H 'Origin: https://oui.doleta.gov' \
# -H 'Pragma: no-cache' \
# -H 'Referer: https://oui.doleta.gov/unemploy/ranking.asp' \
# -H 'Sec-Fetch-Dest: document' \
# -H 'Sec-Fetch-Mode: navigate' \
# -H 'Sec-Fetch-Site: same-origin' \
# -H 'Sec-Fetch-User: ?1' \
# -H 'Upgrade-Insecure-Requests: 1' \
# -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36' \
# -H 'sec-ch-ua: "Google Chrome";v="131", "Chromium";v="131", "Not_A Brand";v="24"' \
# -H 'sec-ch-ua-mobile: ?0' \
# -H 'sec-ch-ua-platform: "Linux"' \
# --data-raw 'category%5B%5D=4&category%5B%5D=5&strtqtr=01%2F01&strtyear=2024&endqtr=12%2F31&endyear=2024&submit=Submit'
if(!file.exists("input/quality.csv")){
  headers = add_headers(
    'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'Accept-Language' = 'en-US,en;q=0.9',
    'Cache-Control' = 'no-cache',
    'Connection' = 'keep-alive',
    'Content-Type' = 'application/x-www-form-urlencoded',
    'Cookie' = 'cookieOUI=cookieOUIValue; mycookie=cookie_value',
    'Origin' = 'https://oui.doleta.gov',
    'Pragma' = 'no-cache',
    'Referer' = 'https://oui.doleta.gov/unemploy/ranking.asp',
    'Sec-Fetch-Dest' = 'document',
    'Sec-Fetch-Mode' = 'navigate',
    'Sec-Fetch-Site' = 'same-origin',
    'Sec-Fetch-User' = '?1',
    'Upgrade-Insecure-Requests' = '1',
    'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36',
    'sec-ch-ua' = '"Google Chrome";v="131", "Chromium";v="131", "Not_A Brand";v="24"',
    'sec-ch-ua-mobile' = '?0',
    'sec-ch-ua-platform' = '"Linux"'
  )
  
  start_end_quarter_options = list(
    c("01/01", "03/31"),
    c("04/01", "06/30"),
    c("07/01", "09/30"),
    c("10/01", "12/31")
  )
  years = 2018:2024
  
  quality_data_list = list()
  quality_data_index = 1
  for(year in years){
    for(start_end_quarter_option in start_end_quarter_options){
      start_quarter = start_end_quarter_option[1]
      message(year,"-",start_quarter)
      end_quarter = start_end_quarter_option[2]
      res <- POST("https://oui.doleta.gov/unemploy/ranking/rankingrpt.asp",
                  headers,
                  body = list(
                    "category[]" = 4, # Nonmonetary Separation Quality
                    "category[]" = 5, # Nonmonetary Nonseparation Quality
                    "strtqtr" = start_quarter, 
                    "strtyear" = year, 
                    "endqtr" = end_quarter,
                    "endyear" = year,
                    "submit" = "Submit"
                  ),
                  encode = "form"
      )
      res_text = content(res, as="text")
      res_html = read_html(res_text)
      html_tables = html_table(res_html)
      sep_quality_table = html_tables[[1]]
      names(sep_quality_table) = c("state", "nonmonetary_separation_quality_percent")
      sep_quality_table = sep_quality_table[2:nrow(sep_quality_table),]
      nonsep_quality_table = html_tables[[2]]
      names(nonsep_quality_table) = c("state", "nonmonetary_nonseparation_quality_percent")
      nonsep_quality_table = nonsep_quality_table[2:nrow(nonsep_quality_table),]
      quality_dat = merge(sep_quality_table, nonsep_quality_table, all=T)
      quality_dat$year = year
      quality_dat$start_quarter = start_quarter
      quality_dat$end_quarter = end_quarter
      quality_data_list[[quality_data_index]] = quality_dat
      quality_data_index = quality_data_index + 1
      Sys.sleep(5)
    }
  }
  all_quality_dat = rbindlist(quality_data_list)
  
  # Clean up data flags
  all_quality_dat$nonmonetary_separation_quality_percent = as.numeric(
    gsub(
      "*",
      "",
      gsub("^","",all_quality_dat$nonmonetary_separation_quality_percent, fixed=T),
      fixed=T
    )
  )
  all_quality_dat$nonmonetary_nonseparation_quality_percent = as.numeric(
    gsub(
      "*",
      "",
      gsub("^","",all_quality_dat$nonmonetary_nonseparation_quality_percent, fixed=T),
      fixed=T
    )
  )
  
  # Clean up dates
  all_quality_dat$start_date = as.Date(
    paste(all_quality_dat$start_quarter, all_quality_dat$year, sep="/"), format="%m/%d/%Y"
  )
  all_quality_dat$end_date = as.Date(
    paste(all_quality_dat$end_quarter, all_quality_dat$year, sep="/"), format="%m/%d/%Y"
  )
  
  all_quality_dat$md_fy_num = maryland_fy(all_quality_dat$end_date, numeric=T)
  all_quality_dat$md_fy_str = maryland_fy(all_quality_dat$end_date, numeric=F)
  all_quality_dat[,c("start_quarter", "end_quarter", "year")] = NULL
  
  # Write
  fwrite(
    all_quality_dat,
    "input/quality.csv"
  )
}

# Quality annual
if(!file.exists("input/quality_annual_fy.csv")){
  headers = add_headers(
    'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'Accept-Language' = 'en-US,en;q=0.9',
    'Cache-Control' = 'no-cache',
    'Connection' = 'keep-alive',
    'Content-Type' = 'application/x-www-form-urlencoded',
    'Cookie' = 'cookieOUI=cookieOUIValue; mycookie=cookie_value',
    'Origin' = 'https://oui.doleta.gov',
    'Pragma' = 'no-cache',
    'Referer' = 'https://oui.doleta.gov/unemploy/ranking.asp',
    'Sec-Fetch-Dest' = 'document',
    'Sec-Fetch-Mode' = 'navigate',
    'Sec-Fetch-Site' = 'same-origin',
    'Sec-Fetch-User' = '?1',
    'Upgrade-Insecure-Requests' = '1',
    'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36',
    'sec-ch-ua' = '"Google Chrome";v="131", "Chromium";v="131", "Not_A Brand";v="24"',
    'sec-ch-ua-mobile' = '?0',
    'sec-ch-ua-platform' = '"Linux"'
  )
  
  start_end_quarter = c("07/01", "06/30")
  years = 2018:2024
  
  quality_data_list = list()
  quality_data_index = 1
  for(year in years){
      start_quarter = start_end_quarter[1]
      message(year,"-",start_quarter)
      end_quarter = start_end_quarter[2]
      end_year = year + 1
      res <- POST("https://oui.doleta.gov/unemploy/ranking/rankingrpt.asp",
                  headers,
                  body = list(
                    "category[]" = 4, # Nonmonetary Separation Quality
                    "category[]" = 5, # Nonmonetary Nonseparation Quality
                    "strtqtr" = start_quarter, 
                    "strtyear" = year, 
                    "endqtr" = end_quarter,
                    "endyear" = end_year,
                    "submit" = "Submit"
                  ),
                  encode = "form"
      )
      res_text = content(res, as="text")
      res_html = read_html(res_text)
      html_tables = html_table(res_html)
      sep_quality_table = html_tables[[1]]
      names(sep_quality_table) = c("state", "nonmonetary_separation_quality_percent")
      sep_quality_table = sep_quality_table[2:nrow(sep_quality_table),]
      nonsep_quality_table = html_tables[[2]]
      names(nonsep_quality_table) = c("state", "nonmonetary_nonseparation_quality_percent")
      nonsep_quality_table = nonsep_quality_table[2:nrow(nonsep_quality_table),]
      quality_dat = merge(sep_quality_table, nonsep_quality_table, all=T)
      quality_dat$year = year
      quality_dat$end_year = end_year
      quality_dat$start_quarter = start_quarter
      quality_dat$end_quarter = end_quarter
      quality_data_list[[quality_data_index]] = quality_dat
      quality_data_index = quality_data_index + 1
      Sys.sleep(5)
  }
  all_quality_dat = rbindlist(quality_data_list)
  
  # Clean up data flags
  all_quality_dat$nonmonetary_separation_quality_percent = as.numeric(
    gsub(
      "*",
      "",
      gsub("^","",all_quality_dat$nonmonetary_separation_quality_percent, fixed=T),
      fixed=T
    )
  )
  all_quality_dat$nonmonetary_nonseparation_quality_percent = as.numeric(
    gsub(
      "*",
      "",
      gsub("^","",all_quality_dat$nonmonetary_nonseparation_quality_percent, fixed=T),
      fixed=T
    )
  )
  
  # Clean up dates
  all_quality_dat$start_date = as.Date(
    paste(all_quality_dat$start_quarter, all_quality_dat$year, sep="/"), format="%m/%d/%Y"
  )
  all_quality_dat$end_date = as.Date(
    paste(all_quality_dat$end_quarter, all_quality_dat$end_year, sep="/"), format="%m/%d/%Y"
  )
  
  all_quality_dat$md_fy_num = maryland_fy(all_quality_dat$end_date, numeric=T)
  all_quality_dat$md_fy_str = maryland_fy(all_quality_dat$end_date, numeric=F)
  all_quality_dat[,c("start_quarter", "end_quarter", "year")] = NULL
  
  # Write
  fwrite(
    all_quality_dat,
    "input/quality_annual_fy.csv"
  )
}

# Reemployment of UI Benefit Recipients ####
# Instructions https://www.dol.gov/sites/dolgov/files/ETA/handbooks/2017/ETHand401_5th.pdf#page=195

if(!file.exists("input/ar9047.csv")){
  ar9047_url = "https://oui.doleta.gov/unemploy/csv/ar9047.csv"
  ar9047 = fread(ar9047_url)
  
  # Codenames via https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA9047
  ar9047_colnames = c(
    "st"="state",
    "rptdate"="report_for_period_ending",
    "c1"="non_exempt_1st_payment",
    "c2"="non_exempt_intrastate_matches",
    "c3"="non_exempt_interstate_matches",
    "c4"="exempt_1st_payment", 
    "c5"="exempt_intrastate_matches",
    "c6"="exempt_interstate_matches"
  )
  names(ar9047)[which(names(ar9047) %in% names(ar9047_colnames))] =
    ar9047_colnames[names(ar9047)[which(names(ar9047) %in% names(ar9047_colnames))]]
  
  # Parse dates
  ar9047$report_for_period_ending = as.Date(
    ar9047$report_for_period_ending, format="%m/%d/%Y"
  )
  
  # Subset to relevant dates
  ar9047 = subset(ar9047, year(report_for_period_ending) >= 2019)
  ar9047$md_fy_num = maryland_fy(ar9047$report_for_period_ending, numeric=T)
  ar9047$md_fy_str = maryland_fy(ar9047$report_for_period_ending, numeric=F)
  
  # Transform
  ar9047$reemployment_rate = 
    (ar9047$non_exempt_intrastate_matches + ar9047$non_exempt_interstate_matches) /
    ar9047$non_exempt_1st_payment
  
  # Some EDA
  # ar9047_md = subset(ar9047, state=="MD")
  # plot(reemployment_rate~report_for_period_ending, data=ar9047_md, type="l")

  # Write
  fwrite(
    ar9047,
    "input/ar9047.csv"
  )
}

# Characteristics of the Insured Unemployed ####
# Instructions https://www.dol.gov/sites/dolgov/files/ETA/handbooks/2017/ETHand401_5th.pdf#page=152
if(!file.exists("input/ar203.csv")){
  ar203_url = "https://oui.doleta.gov/unemploy/csv/ar203.csv"
  ar203 = fread(ar203_url)
  
  # Codenames via https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA203
  ar203_colnames = c(
    "st"="state",
    "rptdate"="report_for_period_ending",
    "c1"="sample_pop",
    "c2"="sex_male",
    "c3"="sex_female",
    "c4"="sex_ina",
    "c40"="eth_hispanic_latino",
    "c41"="eth_not_hispanic_latino",
    "c42"="eth_ina",
    "c43"="race_native_am_alaskan_native",
    "c44"="race_asian",
    "c45"="race_black_african_am",
    "c46"="race_native_hi_pac_island",
    "c47"="race_white",
    "c48"="race_ina",
    "c12"="age_lt22",
    "c13"="age_22-24",
    "c14"="age_25-34",
    "c15"="age_35-44",
    "c16"="age_45-54",
    "c17"="age_55-59",
    "c18"="age_60-64",
    "c19"="age_gt65",
    "c20"="age_ina",
    "c49"="ind_agri_forest_fish_hunt",
    "c50"="ind_mining",
    "c51"="ind_utilities",
    "c52"="ind_construction",
    "c53"="ind_manufacturing",
    "c54"="ind_wholesaletrade",
    "c55"="ind_retail_trade",
    "c56"="ind_trans_and_warehouse",
    "c57"="ind_information",
    "c58"="ind_finance_insurance",
    "c59"="ind_real_estate_rental_lease",
    "c60"="ind_prof_sci_tech",
    "c61"="ind_management",
    "c62"="ind_admin_waste_remedial",
    "c63"="ind_education",
    "c64"="ind_healthcare_social",
    "c65"="ind_arts_entertain_rec",
    "c66"="ind_accommodation_food",
    "c67"="ind_other",
    "c68"="ind_public_admin",
    "c69"="ind_ina",
    "c70"="occ_management",
    "c71"="occ_business_finance",
    "c72"="occ_computer_math",
    "c73"="occ_architecture_engineering",
    "c74"="occ_life_physical_social",
    "c75"="occ_comm_social_serv",
    "c76"="occ_legal",
    "c77"="occ_edu_train_library",
    "c78"="occ_art_design_enter_sport_media",
    "c79"="occ_healthcare_technical",
    "c80"="occ_healthcare_support",
    "c81"="occ_protective_services",
    "c82"="occ_food_prep_serving",
    "c83"="occ_building_grounds_cleaning_maint",
    "c84"="occ_personal_care_services",
    "c85"="occ_sales",
    "c86"="occ_office_admin",
    "c87"="occ_farm_fish_forest",
    "c88"="occ_construct_extract",
    "c89"="occ_install_maint_repair",
    "c90"="occ_production",
    "c91"="occ_trans_material_moving",
    "c92"="occ_military",
    "c93"="occ_ina"
  )
  names(ar203)[which(names(ar203) %in% names(ar203_colnames))] =
    ar203_colnames[names(ar203)[which(names(ar203) %in% names(ar203_colnames))]]
  
  # Parse dates
  ar203$report_for_period_ending = as.Date(
    ar203$report_for_period_ending, format="%m/%d/%Y"
  )
  
  # Subset to relevant dates
  ar203 = subset(ar203, year(report_for_period_ending) >= 2019)
  ar203$md_fy_num = maryland_fy(ar203$report_for_period_ending, numeric=T)
  ar203$md_fy_str = maryland_fy(ar203$report_for_period_ending, numeric=F)
  
  # Some EDA
  # ar203_md = subset(ar203, state=="MD")
  # describe(ar203_md$sample_pop)
  
  # Write
  fwrite(
    ar203,
    "input/ar203.csv"
  )
}

# State shapefile ####
if(!file.exists("input/cb_2018_us_state_20m/")){
  download.file(
    "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip",
    "input/cb_2018_us_state_20m.zip"
  )
  unzip(
    "input/cb_2018_us_state_20m.zip",
    exdir="input/cb_2018_us_state_20m/"
  )
  file.remove("input/cb_2018_us_state_20m.zip")
}

# BEA GNI per capita ####
# No way to automate presently, but URL:
gni_url = "https://apps.bea.gov/iTable/?reqid=70&step=30&isuri=1&major_area=0&area=xx&year=2023&tableid=21&category=421&area_type=0&year_end=-1&classification=non-industry&state=0&statistic=3&yearbegin=-1&unit_of_measure=levels"


# BLS Industry employment by state, November 2024, seasonally adjusted (in thousands) ####
if(!file.exists("input/state_industry_employment.csv")){
  # Scrape embedded table
  bls_url = "https://www.bls.gov/charts/state-employment-and-unemployment/industry-employment-by-state.htm"
  bls_res = GET(bls_url, add_headers('User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36'))
  bls_html = read_html(content(bls_res, as="text", encoding="utf-8"))
  industry_employment = html_table(bls_html)[[1]]
  
  # Join state abbreviations
  setnames(industry_employment, "State", "state_name")
  state_codes = fread("input/us_states.csv")
  names(state_codes) = c("state_name", "state")
  industry_employment = merge(industry_employment, state_codes, by="state_name", all=T)
  
  # Reshape
  industry_employment_long = melt(industry_employment, id.vars=c("state_name", "state"))
  industry_employment_long$value = as.numeric(gsub(",", "", industry_employment_long$value)) * 1000
  industry_employment_wide = dcast(
    industry_employment_long, 
    state_name+state~variable
  )
  
  # Divide each industry by total nonfarm
  total_nonfarm = industry_employment_wide[,"Total nonfarm"]
  industry_employment_wide[,3:ncol(industry_employment_wide)] = 
    industry_employment_wide[,3:ncol(industry_employment_wide)] / 
    total_nonfarm
  
  # Calculate cosine similarity with Maryland
  maryland_industry = unlist(
    industry_employment_wide[which(industry_employment_wide$state=="MD"),3:ncol(industry_employment_wide)]
  )
  maryland_similarity = c()
  for(i in 1:nrow(industry_employment_wide)){
    state_industry = unlist(
      industry_employment_wide[i,3:ncol(industry_employment_wide)]
    )
    state_industry[which(is.na(state_industry))] = 0
    maryland_similarity = c(maryland_similarity, cosine(maryland_industry, state_industry)[1,1])
  }
  
  industry_employment_wide$maryland_similarity = maryland_similarity
  
  # Write
  fwrite(
    industry_employment_wide,
    "input/state_industry_employment.csv"
  )
}

# Maryland industry specifically ####
# Load saved HTML from https://msa.maryland.gov/msa/mdmanual/01glance/economy/html/labor.html
# Source: Division of Workforce Development & Adult Learning, Department of Labor, Licensing, & Regulation
if(!file.exists("input/md_industry_employment.csv")){
  md_industry_html = read_html("input/Maryland Employment - Workforce.html")
  md_industry_tables = html_table(md_industry_html)
  
  # Combine tables on different rows for total employment
  total_employment = md_industry_tables[[1]][2,1:4]
  names(total_employment) = c("state_name", "2021", "2022", "2023")
  for(i in 2:6){
    new_employment_row = md_industry_tables[[i]][,2:6]
    names(new_employment_row) = new_employment_row[1,]
    new_employment_row = new_employment_row[2,]
    total_employment = cbind(total_employment, new_employment_row)
  }
  total_employment$`NA` = NULL
  
  # Reshape and clean
  total_employment_long = melt(
    total_employment, 
    id.vars="state_name",
    variable.name="year",
    value.name="total_employment"
  )
  total_employment_long$total_employment = as.numeric(
    gsub(",","",total_employment_long$total_employment)
  )
  
  # Combine for industries
  industry_employment = md_industry_tables[[7]][,1:4]
  industry_employment = industry_employment[2:nrow(industry_employment),]
  names(industry_employment) = c("industry", "2021", "2022", "2023")
  for(i in 8:11){
    new_industry_row = md_industry_tables[[i]][,2:6]
    names(new_industry_row) = new_industry_row[1,]
    new_industry_row = new_industry_row[2:nrow(new_industry_row),]
    industry_employment = cbind(industry_employment, new_industry_row)
  }
  
  # Reshape and clean
  industry_employment_long = melt(
    industry_employment, 
    id.vars="industry",
    variable.name="year",
    value.name="employment"
  )
  industry_employment_long$employment = as.numeric(
    gsub(",","",industry_employment_long$employment)
  )
  industry_employment_long = subset(industry_employment_long, !is.na(employment))
  
  # Subset & Merge total employment
  industry_employment_long = merge(
    industry_employment_long,
    total_employment_long,
    by="year"
  )
  industry_employment_long$year = as.numeric(as.character(industry_employment_long$year))
  industry_employment_long = subset(
    industry_employment_long, year >= 2019
  )
  industry_employment_long$percent = 
    industry_employment_long$employment / 
    industry_employment_long$total_employment
  fwrite(industry_employment_long, "input/md_industry_employment.csv")
}


# MD Census Demographics ####
# Source https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-detail.html#v2024
if(!file.exists("input/md_census.csv")){
  md_census = fread("input/sc-est2023-sr11h-24.csv", header=T)
  md_census_long = melt(md_census, id.vars=c("Race"), variable.name="Year")
  md_census_long = data.table(md_census_long)
  md_census_long[,"percent":=value/sum(value), by=.(Year)]
  names(md_census_long) = c("race","year","value","percent")
  fwrite(md_census_long, "input/md_census.csv")
}
