# Requirements and setwd
list.of.packages <- c("data.table","ggplot2","scales","reshape2", "xts", "lubridate", "ggfortify")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
setwd("/home/alex/git/MD-Innovation-Team-Interview/")

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
    ,plot.background = element_blank()
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
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
  geom_area() +
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
  geom_area() +
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
  geom_area() +
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
  geom_area() +
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
  geom_area() +
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
  geom_area() +
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

# Insured unemployment rate
ar539_md$r = ar539_md$r / 100
r_md_agg = ar539_md[,.(r=mean(r)),by=.(
  md_fy_str
)]
r_md_agg = subset(r_md_agg, !md_fy_str %in% c("2018-2019", "2024-2025"))
names(r_md_agg) = c(
  "Maryland Fiscal Year",
  "Average insured unemployment rate"
)
fwrite(r_md_agg, "output/insured_unemployment_md_fy.csv")

# Subset
ar539_md_r = ar539_md[,c("reflected_week_ending", "r")]
ar539_md_r = subset(ar539_md_r, year(reflected_week_ending) >= 2020)

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
gni = subset(gni, state!="MD")
gni = gni[order(gni$md_diff)]
economic_peers = gni$state[1:5]
# "VA" Virginia
# "MN" Minnesota
# "SD" South Dakota
# "IL" Illinois
# "ND" North Dakota

industry = fread("input/state_industry_employment.csv")
industry = subset(industry, state!="MD")
industry = industry[order(-industry$maryland_similarity)]
industry_peers = industry$state[1:5]
# "VA" Virginia
# "NM" New Mexico
# "WA" Washington
# "CA" California
# "CO" Colorado