# Requirements and setwd
list.of.packages <- c("data.table","ggplot2","scales","reshape2", "xts", "lubridate", "ggfortify")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
setwd("/git/MD-Innovation-Team-Interview/")

# Chart styles ####
reds = c(
  "#e84439", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13", "#fce3dc", "#fbd7cb", "#f6b0a0", "#ec6250", "#dc372d", "#cd2b2a", "#a21e25", "#6b120a"
)
oranges = c(
  "#eb642b", "#f6bb9d", "#f18e5e", "#d85b31", "#973915", "#fde5d4", "#fcdbbf", "#facbad", "#f3a47c", "#ee7644", "#cb5730", "#ac4622", "#7a2e05"
)
yellows = c(
  "#f49b21", "#fccc8e", "#f9b865", "#e48a00", "#a85d00", "#feedd4", "#fee7c1", "#fedcab", "#fac47e", "#f7a838", "#df8000", "#ba6b15", "#7d4712"
)
pinks = c(
  "#c2135b", "#e4819b", "#d64278", "#ad1257", "#7e1850", "#f9cdd0", "#f6b8c1", "#f3a5b6", "#e05c86", "#d12568", "#9f1459", "#8d0e56", "#65093d"
)
purples = c(
  "#893f90", "#c189bb", "#a45ea1", "#7b3b89", "#551f65", "#ebcfe5", "#deb5d6", "#cb98c4", "#af73ae", "#994d98", "#732c85", "#632572", "#42184c"
)
blues = c(
  "#0089cc", "#88bae5", "#5da3d9", "#0071b1", "#0c457b", "#d3e0f4", "#bcd4f0", "#a3c7eb", "#77adde", "#4397d3", "#105fa3", "#00538e", "#0a3a64"
)
greens = c(
  "#109e68", "#92cba9", "#5ab88a", "#1e8259", "#16513a", "#c5e1cb", "#b1d8bb", "#a2d1b0", "#74bf93", "#3b8c61", "#00694a", "#005b3e", "#07482e"
)
greys = c(
  "#6a6569", "#a9a6aa", "#847e84", "#555053", "#443e42", "#d9d4da", "#cac5cb", "#b3b0b7", "#b9b5bb", "#5a545a", "#736e73", "#4e484c", "#302b2e"
)

chart_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,panel.grid.major.y = element_line(colour = greys[2])
    ,panel.grid.minor.y = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_rect(fill="#f3f0df")
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
    ,legend.background = element_rect(fill="#f3f0df")
  )

donut_style = chart_style +
  theme(
    panel.grid.major.y = element_blank()
    ,axis.line.x = element_blank()
    ,axis.text = element_blank()
  )

rotate_x_text_45 = theme(
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)
rotate_x_text_90 = theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
)

# Unemployment trends in Maryland based on claims data (e.g., initial claims, continued claims, and insured unemployment rate). ####
ar539 = fread("input/ar539.csv")
ar539_md = subset(ar539, state=="MD")

# Initial claims

# Aggregate by FY
ic_md_agg = ar539_md[,.(ic=sum(ic), fic=sum(fic), xic=sum(xic), wseic=sum(wseic)),by=.(
  md_fy_str
)]
ic_md_agg = subset(ic_md_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
ic_md_agg$ic_total = ic_md_agg$ic + ic_md_agg$fic + ic_md_agg$xic + ic_md_agg$wseic
names(ic_md_agg) = c(
  "Maryland Fiscal Year",
  "State initial claims",
  "UCFE initial claims",
  "UCX initial claims",
  "STC equivalent initial claims",
  "Total initial claims"
)
fwrite(ic_md_agg, "output/initial_claims_md_fy.csv")

# Reshape weekly
ar539_md_ic = ar539_md[,c("reflected_week_ending", "ic", "fic", "xic", "wseic")]
ar539_md_ic = subset(ar539_md_ic, year(reflected_week_ending) >= 2020)
names(ar539_md_ic) = c(
  "reflected_week_ending",
  "State UI",
  "UCFE",
  "UCX",
  "STC equivalent"
)
ar539_md_ic_long = melt(ar539_md_ic, id.vars=c("reflected_week_ending"))

# Plot
ggplot(subset(ar539_md_ic_long, variable=="State UI"), aes(x=reflected_week_ending, y=value, group=variable, fill=variable)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(reds[1])) +
  scale_y_continuous(expand = c(0, 0), labels=label_comma()) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable=="State UI")$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Initial claims",
    x="",
    fill="Program"
  )
ggsave("output/md_state_ic.png", width=10, height=5)
ggplot(subset(ar539_md_ic_long, variable=="State UI" & year(reflected_week_ending) >= 2022), aes(x=reflected_week_ending, y=value, group=variable, fill=variable)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(reds[1])) +
  scale_y_continuous(expand = c(0, 0), labels=label_comma()) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable=="State UI" & year(reflected_week_ending) >= 2022)$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Initial claims",
    x="",
    fill="Program"
  )
ggsave("output/md_state_ic_zoom.png", width=10, height=5)
ggplot(subset(ar539_md_ic_long, variable!="State UI"), aes(x=reflected_week_ending, y=value, group=variable, fill=variable)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(purples[1], oranges[1], yellows[1])) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable!="State UI")$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Initial claims",
    x="",
    fill="Program"
  )
ggsave("output/md_nonstate_ic.png", width=10, height=5)

# UCFE initial claims
ggplot(subset(ar539_md_ic_long, variable=="UCFE" & year(reflected_week_ending) >= 2024), aes(x=reflected_week_ending, y=value)) +
  geom_bar(stat="identity", fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), n.break=6) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable=="UCFE")$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Maryland-based federal\nemployee initial UI claims",
    x=""
  )

# Quick LOESS decomp
ic_for_ts = subset(ar539_md_ic_long, variable=="State UI")
ic_ts = ts(data = as.vector(ic_for_ts$value),
          start = decimal_date(min(ic_for_ts$reflected_week_ending)),
          frequency = 365.25/7
)
# set up stl function
fit = stl(ic_ts, s.window = "periodic")
# plot stl
autoplot(fit)

# Continued claims
cw_md_agg = ar539_md[,.(cw=sum(cw), fcw=sum(fcw), xcw=sum(xcw), wsecw=sum(wsecw), ebt=sum(ebt)),by=.(
  md_fy_str
)]
cw_md_agg = subset(cw_md_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
cw_md_agg$cw_total = cw_md_agg$cw + cw_md_agg$fcw + cw_md_agg$xcw + cw_md_agg$wsecw + cw_md_agg$ebt
names(cw_md_agg) = c(
  "Maryland Fiscal Year",
  "State continued weeks",
  "UCFE continued weeks",
  "UCX continued weeks",
  "STC equivalent continued weeks",
  "EBT equivalent continued weeks",
  "Total continued weeks"
)
fwrite(cw_md_agg, "output/continued_weeks_md_fy.csv")

# Reshape weekly
ar539_md_cw = ar539_md[,c("reflected_week_ending", "cw", "fcw", "xcw", "wsecw", "ebt")]
ar539_md_cw = subset(ar539_md_cw, year(reflected_week_ending) >= 2020)
names(ar539_md_cw) = c(
  "reflected_week_ending",
  "State UI",
  "UCFE",
  "UCX",
  "STC equivalent",
  "EBT"
)
ar539_md_cw_long = melt(ar539_md_cw, id.vars=c("reflected_week_ending"))

# Plot
ggplot(subset(ar539_md_cw_long, variable=="State UI"), aes(x=reflected_week_ending, y=value, group=variable, fill=variable)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(reds[1])) +
  scale_y_continuous(expand = c(0, 0), labels=label_comma()) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable=="State UI")$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Continued weeks",
    x="",
    fill="Program"
  )
ggsave("output/md_state_cw.png", width=10, height=5)
ggplot(subset(ar539_md_cw_long, variable=="State UI" & year(reflected_week_ending) >= 2022), aes(x=reflected_week_ending, y=value, group=variable, fill=variable)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(reds[1])) +
  scale_y_continuous(expand = c(0, 0), labels=label_comma()) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable=="State UI" & year(reflected_week_ending) >= 2022)$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Continued weeks",
    x="",
    fill="Program"
  )
ggsave("output/md_state_cw_zoom.png", width=10, height=5)
ggplot(subset(ar539_md_cw_long, variable!="State UI"), aes(x=reflected_week_ending, y=value, group=variable, fill=variable)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(purples[1], oranges[1], yellows[1], pinks[1])) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable!="State UI")$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Continued weeks",
    x="",
    fill="Program"
  )
ggsave("output/md_nonstate_cw.png", width=10, height=5)

ggplot(subset(ar539_md_cw_long, variable=="UCFE" & year(reflected_week_ending) >= 2024), aes(x=reflected_week_ending, y=value)) +
  geom_bar(stat="identity", fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), n.breaks=6) +
  expand_limits(y=c(0, max(subset(ar539_md_ic_long, variable=="UCFE")$value*1.1))) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Maryland-based federal\nemployee UI continued weeks",
    x=""
  )

# Insured unemployment rate
r_md_agg = ar539_md[,.(at=sum(at), ce=sum(ce)),by=.(
  md_fy_str
)]
r_md_agg$r = r_md_agg$at / r_md_agg$ce
r_md_agg = subset(r_md_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
r_md_agg[,c("at", "ce")] = NULL
names(r_md_agg) = c(
  "Maryland Fiscal Year",
  "Insured unemployment rate"
)
fwrite(r_md_agg, "output/insured_unemployment_md_fy.csv")

# Subset
ar539_md_r = ar539_md[,c("reflected_week_ending", "r")]
ar539_md_r = subset(ar539_md_r, year(reflected_week_ending) >= 2020)
ar539_md_r$r = ar539_md_r$r / 100

# Plot
ggplot(ar539_md_r, aes(x=reflected_week_ending, y=r)) +
  geom_area(fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar539_md_r$r)*1.1)) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Rate of Insured Unemployment",
    x=""
  )
ggsave("output/md_rate_insured_unemployment.png", width=10, height=5)

# Evaluation of Maryland's unemployment insurance program performance using benefit timeliness and quality data (e.g., first payment promptness) ####
# compared to both UI PERFORMS [Core Measures](https://oui.doleta.gov/unemploy/pdf/Core_Measures.pdf) and to peer states. 
# You may choose how to define "peer states" but please explain your assumptions.

# Define/calculate peers
us_states = fread("input/us_states.csv")
setnames(us_states, c("State", "Abbreviation"), c("state_name", "state"))

regional_peers = c(
  "PA", # Pennsylvania
  "DE", # Delaware
  "VA", # Virginia
  "WV", # West Virginia
  "DC" # Washington, D.C
)

gni = fread("input/gni_2023_bea_gov.csv")
md_gni = subset(gni, state=="MD")$gni_per_capita_2023
gni$md_diff = abs(gni$gni_per_capita_2023 - md_gni)
gni = gni[order(gni$md_diff)]
economic_peers = gni$state[2:6]
# "VA" Virginia
# "MN" Minnesota
# "SD" South Dakota
# "IL" Illinois
# "ND" North Dakota
gni_out = gni[1:6,c("state_name","gni_per_capita_2023", "md_diff")]
names(gni_out) = c(
  "State",
  "GNI per capita (2023)",
  "Absolute difference from Maryland"
)
fwrite(gni_out, "output/gni_peers.csv")

industry = fread("input/state_industry_employment.csv")
industry = industry[order(-industry$maryland_similarity)]
industry_peers = industry$state[2:6]
# "VA" Virginia
# "NM" New Mexico
# "WA" Washington
# "CA" California
# "CO" Colorado
industry_out = industry[1:6,c(
  "state_name", 
  "Mining, logging, and construction",
  "Manufacturing",
  "Financial activities",
  "Government",
  "maryland_similarity"
)]
fwrite(industry_out, "output/industry_peers.csv")


# First payment promptness >=87%
ar9050 = fread("input/ar9050.csv")
ar9050 = subset(ar9050, indicator_type %in% c("intra-total", "inter-total"))
ar9050_totals = ar9050[,.(
  `14days_or_fewer`=sum(`14days_or_fewer`),
  `21days_or_fewer`=sum(`21days_or_fewer`),
  total=sum(total)
  ),by=.(
    state,
    non_waiting_week,
    report_for_period_ending,
    md_fy_num,
    md_fy_str
  )]
ar9050_totals = subset(ar9050_totals, year(report_for_period_ending) >= 2020)
ar9050_totals$numerator = ar9050_totals$`21days_or_fewer`
ar9050_totals$numerator[which(ar9050_totals$non_waiting_week)] =
  ar9050_totals$`14days_or_fewer`[which(ar9050_totals$non_waiting_week)]
ar9050_totals$first_payment_promptness = ar9050_totals$numerator / ar9050_totals$total
ar9050_totals = merge(ar9050_totals, us_states, by="state")

ar9050_totals_agg = ar9050[,.(
  `14days_or_fewer`=sum(`14days_or_fewer`),
  `21days_or_fewer`=sum(`21days_or_fewer`),
  total=sum(total)
),by=.(
  state,
  non_waiting_week,
  md_fy_num,
  md_fy_str
)]
ar9050_totals_agg = subset(ar9050_totals_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
ar9050_totals_agg$numerator = ar9050_totals_agg$`21days_or_fewer`
ar9050_totals_agg$numerator[which(ar9050_totals_agg$non_waiting_week)] =
  ar9050_totals_agg$`14days_or_fewer`[which(ar9050_totals_agg$non_waiting_week)]
ar9050_totals_agg$first_payment_promptness = ar9050_totals_agg$numerator / ar9050_totals_agg$total
ar9050_totals_agg = merge(ar9050_totals_agg, us_states, by="state")
ar9050_totals_agg = ar9050_totals_agg[,c("state_name", "state", "md_fy_str", "non_waiting_week", "first_payment_promptness")]
ar9050_totals_agg_wide = dcast(
  ar9050_totals_agg,
  state_name+state+non_waiting_week~md_fy_str,
  value.var = "first_payment_promptness"
)
# Note: These don't match with premade FPP indicators
# But MD didn't become non-waiting until March 2020, so maybe
# The premade ones haven't switched to 14 days yet
ar9050_totals_agg_wide_regional = subset(ar9050_totals_agg_wide, state %in% c("MD",regional_peers))
fwrite(ar9050_totals_agg_wide_regional,"output/fpp_regional.csv")

ar9050_totals_agg_wide_econ = subset(ar9050_totals_agg_wide, state %in% c("MD", economic_peers))
fwrite(ar9050_totals_agg_wide_econ,"output/fpp_econ.csv")

ar9050_totals_agg_wide_industry = subset(ar9050_totals_agg_wide, state %in% c("MD", industry_peers))
fwrite(ar9050_totals_agg_wide_industry,"output/fpp_industry.csv")

ar9050_totals_regional = subset(ar9050_totals, state %in% c("MD",regional_peers))
ggplot(ar9050_totals_regional, aes(x=report_for_period_ending, y=first_payment_promptness)) +
  geom_line(color=blues[1]) +
  geom_hline(yintercept = 0.87, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9050_totals_regional$first_payment_promptness*1.1))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="First Payment Promptness",
    x="",
    color=""
  )
ggsave("output/fpp_regional.png", width=10, height=5)

ar9050_totals_econ = subset(ar9050_totals, state %in% c("MD",economic_peers))
ggplot(ar9050_totals_econ, aes(x=report_for_period_ending, y=first_payment_promptness)) +
  geom_line(color=blues[1]) +
  geom_hline(yintercept = 0.87, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9050_totals_econ$first_payment_promptness*1.1))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="First Payment Promptness",
    x="",
    color=""
  )
ggsave("output/fpp_econ.png", width=10, height=5)

ar9050_totals_industry = subset(ar9050_totals, state %in% c("MD",industry_peers))
ggplot(ar9050_totals_industry, aes(x=report_for_period_ending, y=first_payment_promptness)) +
  geom_line(color=blues[1]) +
  geom_hline(yintercept = 0.87, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9050_totals_industry$first_payment_promptness*1.1))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="First Payment Promptness",
    x="",
    color=""
  )
ggsave("output/fpp_industry.png", width=10, height=5)

# Nonmonetary Determination Time Lapse >= 80%
ar9052 = fread("input/ar9052.csv")
ar9052 = subset(ar9052, indicator_type %in% c("intra-total", "inter-total"))
ar9052_totals = ar9052[,.(
  `21days_or_fewer`=sum(`21days_or_fewer`),
  total=sum(total)
),by=.(
  state,
  report_for_period_ending,
  md_fy_num,
  md_fy_str
)]
ar9052_totals = subset(ar9052_totals, year(report_for_period_ending) >= 2020)
ar9052_totals$ndtl = ar9052_totals$`21days_or_fewer` / ar9052_totals$total
ar9052_totals = merge(ar9052_totals, us_states, by="state")

ar9052_totals_agg = ar9052[,.(
  `21days_or_fewer`=sum(`21days_or_fewer`),
  total=sum(total)
),by=.(
  state,
  md_fy_num,
  md_fy_str
)]
ar9052_totals_agg = subset(ar9052_totals_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
ar9052_totals_agg$ndtl = ar9052_totals_agg$`21days_or_fewer` / ar9052_totals_agg$total
ar9052_totals_agg = merge(ar9052_totals_agg, us_states, by="state")
ar9052_totals_agg = ar9052_totals_agg[,c("state_name", "state", "md_fy_str", "ndtl")]
ar9052_totals_agg_wide = dcast(
  ar9052_totals_agg,
  state_name+state~md_fy_str,
  value.var = "ndtl"
)

ar9052_totals_agg_wide_regional = subset(ar9052_totals_agg_wide, state %in% c("MD",regional_peers))
fwrite(ar9052_totals_agg_wide_regional,"output/ndtl_regional.csv")

ar9052_totals_agg_wide_econ = subset(ar9052_totals_agg_wide, state %in% c("MD", economic_peers))
fwrite(ar9052_totals_agg_wide_econ,"output/ndtl_econ.csv")

ar9052_totals_agg_wide_industry = subset(ar9052_totals_agg_wide, state %in% c("MD", industry_peers))
fwrite(ar9052_totals_agg_wide_industry,"output/ndtl_industry.csv")

ar9052_totals_regional = subset(ar9052_totals, state %in% c("MD",regional_peers))
ggplot(ar9052_totals_regional, aes(x=report_for_period_ending, y=ndtl)) +
  geom_line(color=blues[1]) +
  geom_hline(yintercept = 0.80, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9052_totals_regional$ndtl*1.1))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Time Lapse",
    x="",
    color=""
  )
ggsave("output/ndtl_regional.png", width=10, height=5)

ar9052_totals_econ = subset(ar9052_totals, state %in% c("MD",economic_peers))
ggplot(ar9052_totals_econ, aes(x=report_for_period_ending, y=ndtl)) +
  geom_line(color=blues[1]) +
  geom_hline(yintercept = 0.80, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9052_totals_econ$ndtl*1.1))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Time Lapse",
    x="",
    color=""
  )
ggsave("output/ndtl_econ.png", width=10, height=5)

ar9052_totals_industry = subset(ar9052_totals, state %in% c("MD",industry_peers))
ggplot(ar9052_totals_industry, aes(x=report_for_period_ending, y=ndtl)) +
  geom_line(color=blues[1]) +
  geom_hline(yintercept = 0.80, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9052_totals_industry$ndtl*1.1))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Time Lapse",
    x="",
    color=""
  )
ggsave("output/ndtl_industry.png", width=10, height=5)

# Nonmonetary Determination Quality - Sep and Nonsep
quality = fread("input/quality.csv")
quality = subset(quality, year(start_date) >= 2020)
quality = merge(quality, us_states, by="state")
quality$nonmonetary_nonseparation_quality_percent =
  quality$nonmonetary_nonseparation_quality_percent / 100
quality$nonmonetary_separation_quality_percent = 
  quality$nonmonetary_separation_quality_percent / 100

quality_annual = fread("input/quality_annual_fy.csv")
quality_annual = subset(quality_annual, !md_fy_str %in% c("2018-2019", "2024-2025"))
quality_annual = merge(quality_annual, us_states, by="state")
quality_annual_nonsep = quality_annual[,c("state_name", "state", "md_fy_str", "nonmonetary_nonseparation_quality_percent")]
quality_annual_nonsep$nonmonetary_nonseparation_quality_percent = quality_annual_nonsep$nonmonetary_nonseparation_quality_percent / 100
quality_annual_sep = quality_annual[,c("state_name", "state", "md_fy_str", "nonmonetary_separation_quality_percent")]
quality_annual_sep$nonmonetary_separation_quality_percent = quality_annual_sep$nonmonetary_separation_quality_percent / 100

quality_annual_nonsep_wide = dcast(
  quality_annual_nonsep,
  state_name+state~md_fy_str,
  value.var = "nonmonetary_nonseparation_quality_percent"
)
quality_annual_sep_wide = dcast(
  quality_annual_sep,
  state_name+state~md_fy_str,
  value.var = "nonmonetary_separation_quality_percent"
)

quality_annual_sep_wide_regional = subset(quality_annual_sep_wide, state %in% c("MD",regional_peers))
fwrite(quality_annual_sep_wide_regional,"output/quality_sep_regional.csv")
quality_annual_nonsep_wide_regional = subset(quality_annual_nonsep_wide, state %in% c("MD",regional_peers))
fwrite(quality_annual_nonsep_wide_regional,"output/quality_nonsep_regional.csv")

quality_annual_sep_wide_econ = subset(quality_annual_sep_wide, state %in% c("MD", economic_peers))
fwrite(quality_annual_sep_wide_econ,"output/quality_sep_econ.csv")
quality_annual_nonsep_wide_econ = subset(quality_annual_nonsep_wide, state %in% c("MD", economic_peers))
fwrite(quality_annual_nonsep_wide_econ,"output/quality_nonsep_econ.csv")

quality_annual_sep_wide_industry = subset(quality_annual_sep_wide, state %in% c("MD", industry_peers))
fwrite(quality_annual_sep_wide_industry,"output/quality_sep_industry.csv")
quality_annual_nonsep_wide_industry = subset(quality_annual_nonsep_wide, state %in% c("MD", industry_peers))
fwrite(quality_annual_nonsep_wide_industry,"output/quality_nonsep_industry.csv")

quality_regional = subset(quality, state %in% c("MD",regional_peers))
ggplot(quality_regional, aes(x=end_date, y=nonmonetary_separation_quality_percent)) +
  geom_bar(stat="identity", fill=blues[1]) +
  geom_hline(yintercept = 0.75, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(quality_regional$nonmonetary_separation_quality_percent*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Quality - Separations",
    x="",
    color=""
  )
ggsave("output/ndqs_regional.png", width=10, height=5)
ggplot(quality_regional, aes(x=end_date, y=nonmonetary_nonseparation_quality_percent)) +
  geom_bar(stat="identity", fill=blues[1]) +
  geom_hline(yintercept = 0.75, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(quality_regional$nonmonetary_nonseparation_quality_percent*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Quality - Nonseparations",
    x="",
    color=""
  )
ggsave("output/ndqn_regional.png", width=10, height=5)

quality_econ = subset(quality, state %in% c("MD",economic_peers))
ggplot(quality_econ, aes(x=end_date, y=nonmonetary_separation_quality_percent)) +
  geom_bar(stat="identity", fill=blues[1]) +
  geom_hline(yintercept = 0.75, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(quality_econ$nonmonetary_separation_quality_percent*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Quality - Separations",
    x="",
    color=""
  )
ggsave("output/ndqs_econ.png", width=10, height=5)
ggplot(quality_econ, aes(x=end_date, y=nonmonetary_nonseparation_quality_percent)) +
  geom_bar(stat="identity", fill=blues[1]) +
  geom_hline(yintercept = 0.75, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(quality_econ$nonmonetary_nonseparation_quality_percent*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Quality - Nonseparations",
    x="",
    color=""
  )
ggsave("output/ndqn_econ.png", width=10, height=5)

quality_industry = subset(quality, state %in% c("MD",industry_peers))
ggplot(quality_industry, aes(x=end_date, y=nonmonetary_separation_quality_percent)) +
  geom_bar(stat="identity", fill=blues[1]) +
  geom_hline(yintercept = 0.75, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(quality_industry$nonmonetary_separation_quality_percent*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Quality - Separations",
    x="",
    color=""
  )
ggsave("output/ndqs_industry.png", width=10, height=5)
ggplot(quality_industry, aes(x=end_date, y=nonmonetary_nonseparation_quality_percent, na.rm=T)) +
  geom_bar(stat="identity", fill=blues[1]) +
  geom_hline(yintercept = 0.75, color=reds[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(quality_industry$nonmonetary_nonseparation_quality_percent*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Nonmonetary Determination Quality - Nonseparations",
    x="",
    color=""
  )
ggsave("output/ndqn_industry.png", width=10, height=5)

# Reemployment rate
ar9047 = fread("input/ar9047.csv")

ar9047_totals = subset(ar9047, year(report_for_period_ending) >= 2020)
ar9047_totals = merge(ar9047_totals, us_states, by="state")

ar9047_totals_agg = ar9047[,.(
  non_exempt_intrastate_matches=sum(non_exempt_intrastate_matches),
  non_exempt_interstate_matches=sum(non_exempt_interstate_matches),
  non_exempt_1st_payment=sum(non_exempt_1st_payment)
),by=.(
  state,
  md_fy_num,
  md_fy_str
)]
ar9047_totals_agg = subset(ar9047_totals_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
ar9047_totals_agg$reemployment_rate = 
  (ar9047_totals_agg$non_exempt_intrastate_matches + ar9047_totals_agg$non_exempt_interstate_matches) /
  ar9047_totals_agg$non_exempt_1st_payment
ar9047_totals_agg = merge(ar9047_totals_agg, us_states, by="state")
ar9047_totals_agg = ar9047_totals_agg[,c("state_name", "state", "md_fy_str", "reemployment_rate")]
ar9047_totals_agg_wide = dcast(
  ar9047_totals_agg,
  state_name+state~md_fy_str,
  value.var = "reemployment_rate"
)

ar9047_totals_agg_wide_regional = subset(ar9047_totals_agg_wide, state %in% c("MD",regional_peers))
fwrite(ar9047_totals_agg_wide_regional,"output/reemployment_rate_regional.csv")

ar9047_totals_agg_wide_econ = subset(ar9047_totals_agg_wide, state %in% c("MD", economic_peers))
fwrite(ar9047_totals_agg_wide_econ,"output/reemployment_rate_econ.csv")

ar9047_totals_agg_wide_industry = subset(ar9047_totals_agg_wide, state %in% c("MD", industry_peers))
fwrite(ar9047_totals_agg_wide_industry,"output/reemployment_rate_industry.csv")

ar9047_totals_regional = subset(ar9047_totals, state %in% c("MD",regional_peers))
ggplot(ar9047_totals_regional, aes(x=report_for_period_ending, y=reemployment_rate)) +
  geom_line(color=blues[1]) +
  geom_point(color=blues[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9047_totals_regional$reemployment_rate*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_90 +
  labs(
    y="Reemployment Rate",
    x="",
    color=""
  )
ggsave("output/reemployment_rate_regional.png", width=10, height=5)

ar9047_totals_econ = subset(ar9047_totals, state %in% c("MD",economic_peers))
ggplot(ar9047_totals_econ, aes(x=report_for_period_ending, y=reemployment_rate)) +
  geom_line(color=blues[1]) +
  geom_point(color=blues[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9047_totals_econ$reemployment_rate*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_90 +
  labs(
    y="Reemployment Rate",
    x="",
    color=""
  )
ggsave("output/reemployment_rate_econ.png", width=10, height=5)

ar9047_totals_industry = subset(ar9047_totals, state %in% c("MD",industry_peers))
ggplot(ar9047_totals_industry, aes(x=report_for_period_ending, y=reemployment_rate)) +
  geom_line(color=blues[1]) +
  geom_point(color=blues[1]) +
  facet_wrap(~state_name) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(ar9047_totals_industry$reemployment_rate*1.1, na.rm=T))) +
  scale_x_date(date_breaks = "9 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Reemployment Rate",
    x="",
    color=""
  )
ggsave("output/reemployment_rate_industry.png", width=10, height=5)

# Insights related to claimant demographics or characteristics, ####
# including claimant industry or sector, that may help the Innovation Team 
# and MDL develop tailored initiatives in the future.
ar203 = fread("input/ar203.csv")
ar203_md = subset(ar203, state=="MD")
unique(ar203_md$sample_pop)
ar203_md$sample_pop = NULL

ar203_md_long = melt(
  ar203_md, id.vars=c(
    "state",
    "report_for_period_ending",
    "md_fy_num",
    "md_fy_str"
  )
)
ar203_md_long$variable = as.character(ar203_md_long$variable)
ar203_md_long$variable_type = sapply(
  sapply(
    ar203_md_long$variable,
    strsplit,
    split="_"
  ),
  `[[`,
  1
)
ar203_md_long = data.table(ar203_md_long)
ar203_md_long[,"percent":=value/sum(value, na.rm=T), by=.(variable_type, report_for_period_ending)]

ar203_md_long_agg = ar203_md_long[,.(
  value=sum(value, na.rm=T)
), by=.(state, md_fy_num, md_fy_str, variable, variable_type)]
ar203_md_long_agg[,"percent":=value/sum(value, na.rm=T), by=.(variable_type, md_fy_num)]
ar203_md_long_agg = subset(ar203_md_long_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
ar203_md_long = subset(ar203_md_long, year(report_for_period_ending) >= 2020)

ar203_md_wide_agg = ar203_md_long_agg[,c("md_fy_str", "variable", "variable_type", "percent")]
ar203_md_wide_agg = dcast(
  ar203_md_wide_agg,
  variable+variable_type~md_fy_str,
  value.var="percent"
)
ar203_md_wide_agg$five_yr_change =
  ar203_md_wide_agg$`2023-2024` - ar203_md_wide_agg$`2019-2020`
ar203_md_wide_agg = ar203_md_wide_agg[order(-ar203_md_wide_agg$five_yr_change),]
for(tmp_variable_type in unique(ar203_md_wide_agg$variable_type)){
  tmp = subset(ar203_md_wide_agg, variable_type==tmp_variable_type)
  fwrite(tmp, paste0("output/demographic_",tmp_variable_type,".csv"))
}


ar203_md_long_sex = subset(ar203_md_long, variable_type=="sex")
ggplot(ar203_md_long_sex, aes(x=report_for_period_ending, y=percent, group=variable, fill=variable)) +
  geom_area(alpha=0.9)
ar203_md_long_age = subset(ar203_md_long, variable_type=="age")
ggplot(ar203_md_long_age, aes(x=report_for_period_ending, y=percent, group=variable, fill=variable)) +
  geom_area(alpha=0.9)
ar203_md_long_eth = subset(ar203_md_long, variable_type=="eth")
ggplot(ar203_md_long_eth, aes(x=report_for_period_ending, y=percent, group=variable, fill=variable)) +
  geom_area(alpha=0.9)
ar203_md_long_occ = subset(ar203_md_long, variable_type=="occ")
ggplot(ar203_md_long_occ, aes(x=report_for_period_ending, y=percent, group=variable, fill=variable)) +
  geom_area(alpha=0.9)
ar203_md_long_race = subset(ar203_md_long, variable_type=="race")
race_labels = c(
  "race_asian"="Asian",
  "race_black_african_am"="Black or African American",
  "race_white"="White",
  "race_ina"="Not available"
)
ar203_md_long_race$label = race_labels[ar203_md_long_race$variable]
ar203_md_long_race$label[which(is.na(ar203_md_long_race$label))] = "Other"
ar203_md_long_race = ar203_md_long_race[,.(
  percent=sum(percent)
), by=.(report_for_period_ending, label)]
ar203_md_long_race$label = factor(
  ar203_md_long_race$label,
  levels=c(
    "Not available",
    "Other",
    "Asian",
    "Black or African American",
    "White"
  )
)
ggplot(ar203_md_long_race, aes(x=report_for_period_ending, y=percent, group=label, fill=label)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(greys[1], greys[2], reds[1], yellows[4], blues[1])) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, 1)) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Percent of insured unemployed",
    x="",
    fill=""
  )
ggsave("output/demographics_race.png", width=10, height=5)
md_census = fread("input/md_census.csv")
md_census$label = md_census$race
md_census$label[which(!md_census$race %in% race_labels)] = "Other"
md_census = md_census[,.(percent=sum(percent)), by=.(year, label)]
md_census_wide = dcast(md_census, label~year, value.var="percent")
fwrite(md_census_wide, "output/md_census_for_comp.csv")
ggplot(md_census, aes(x=year, y=percent, group=label, fill=label)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(reds[1], yellows[4], greys[2], blues[1])) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(md_census$percent * 1.1))) +
  scale_x_continuous() +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Percent of population",
    x="",
    fill=""
  )
ggsave("output/md_census_for_comp.png", width=10, height=5)

ar203_md_long_ind = subset(ar203_md_long, variable_type=="ind")
ind_labels = c(
  "ind_prof_sci_tech"="Professional/Scientific/Technical Services",
  "ind_retail_trade"="Retail Trade",
  "ind_accommodation_food"="Accommodation and Food Services",
  "ind_ina"="Not available"
)
ar203_md_long_ind$label = ind_labels[ar203_md_long_ind$variable]
ar203_md_long_ind$label[which(is.na(ar203_md_long_ind$label))] = "Other"
ar203_md_long_ind = ar203_md_long_ind[,.(
  percent=sum(percent)
), by=.(report_for_period_ending, label)]
ar203_md_long_ind$label = factor(
  ar203_md_long_ind$label,
  levels=c(
    "Not available",
    "Other",
    "Professional/Scientific/Technical Services",
    "Retail Trade",
    "Accommodation and Food Services"
  )
)
ggplot(ar203_md_long_ind, aes(x=report_for_period_ending, y=percent, group=label, fill=label)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(greys[1], greys[2], reds[1], yellows[4], blues[1])) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, 1)) +
  scale_x_date(date_breaks = "3 months") +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Percent of insured unemployed",
    x="",
    fill=""
  ) + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("output/demographics_ind.png", width=10, height=5)

md_industry = fread("input/md_industry_employment.csv")
md_industry = subset(md_industry,
                     industry %in% c(
                       "professional & business", # "Professional/Scientific/Technical Services",
                       "information", 
                       "trade, transportation, utilities, & Warehousing", # "Retail Trade",
                       "leisure & hospitality" # "Accommodation and Food Services"
                     ))
md_industry_wide = dcast(
  md_industry,
  industry~year,
  value.var="percent"
)
fwrite(md_industry_wide, "output/md_industry_for_comp.csv")

industry_labels = c(
  "professional & business" = "Professional & business",
  "information" = "Information", 
  "trade, transportation, utilities, & Warehousing" = "Trade, transportation,\nutilities, & warehousing",
  "leisure & hospitality"="Leisure and hospitality"
)
md_industry$label = industry_labels[md_industry$industry]
ggplot(md_industry, aes(x=year, y=percent, group=label, fill=label)) +
  geom_area(alpha=0.9) +
  scale_fill_manual(values=c(reds[1], blues[1], reds[5], yellows[4])) +
  scale_y_continuous(expand = c(0, 0), labels=percent) +
  expand_limits(y=c(0, max(md_industry$percent * 1.1))) +
  scale_x_continuous() +
  chart_style +
  rotate_x_text_45 +
  labs(
    y="Percent of workforce",
    x="",
    fill=""
  ) + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("output/md_industry_for_comp.png", width=10, height=5)
