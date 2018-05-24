tidy_tges_datalibrary(devtools)
devtools::install_github("almartin82/njschooldata")
knitr::opts_chunk$set(echo = TRUE)
library(devtools)
devtools::install_github("almartin82/njschooldata")
library(njschooldata)
source('~/GitHub/njschooldata/R/tges.R', echo=TRUE)
raw_tges <- get_raw_tges(c(1999:2015))
raw_tges <- get_raw_tges(c(1999:2017))
raw_tges <- get_raw_tges(1999)
indicator_fields <- list(
"exp" = "Total Expenditures, actual costs",
"ade" = "Average Daily Enrollment plus Sent Pupils",
"pp" = "Per Pupil Total Expenditures",
"rk" = "Per Pupil Rank",
"boty" = "Budget / Operating type"
)
tges <- tges_name_cleaner(raw_tges, indicator_fields)
tges
many_tges <- fetch_many_tges(c(1999:2015))
glimpse(many_tges$`1999`$CSG1R)
glimpse(many_tges$`1999`$CSG2)
glimpse(many_tges$`1999`$CSG3)
glimpse(many_tges$`1999`$CSG4)
glimpse(many_tges$`1999`$CSG5)
glimpse(many_tges$`1999`$CSG6)
glimpse(many_tges$`1999`$CSG7)
glimpse(many_tges$`1999`$CSG8)
glimpse(many_tges$`1999`$CSG9)
glimpse(many_tges$`2015`$CSG9)
glimpse(many_tges$`2000`$CSG9)
glimpse(many_tges$`2000`$CSG10)
glimpse(many_tges$`2001`$CSG9)
glimpse(many_tges$`2002`$CSG9)
glimpse(raw_tges$CSG9)
glimpse(many_tges$`2002`$CSG9)
glimpse(many_tges$`2003`$CSG9)
glimpse(many_tges$`2004`$CSG9)
glimpse(many_tges$`2005`$CSG9)
tidy_admin_salaries <- map(
.x = many_tges,
.f = function(.x) {
.x %>% extract2('CSG9')
}
) %>%
reduce(bind_rows) %>%
left_join(dfg[, c('district_code', 'dfg')], by = c('district_code')) %>%
write_csv(path = "tidy_admin_salaries.csv")
dfg <- fetch_dfg()
tidy_admin_salaries <- map(
.x = many_tges,
.f = function(.x) {
.x %>% extract2('CSG9')
}
) %>%
reduce(bind_rows) %>%
left_join(dfg[, c('district_code', 'dfg')], by = c('district_code')) %>%
write_csv(path = "tidy_admin_salaries.csv")
is.na(tidy_admin_salaries$`District rank`) %>% table(tidy_admin_salaries$end_year)
glimpse(tidy_admin_salaries)
glimpse(raw_tges$CSG9)
source('~/GitHub/njschooldata/R/tges.R', echo=TRUE)
many_tges <- fetch_many_tges(c(1999:2015))
dfg <- fetch_dfg()
tidy_admin_salaries <- map(
.x = many_tges,
.f = function(.x) {
.x %>% extract2('CSG9')
}
) %>%
reduce(bind_rows) %>%
left_join(dfg[, c('district_code', 'dfg')], by = c('district_code')) %>%
write_csv(path = "tidy_admin_salaries.csv")
glimpse(many_tges$`2000`$CSG9)
glimpse(many_tges$`2004`$CSG9)
glimpse(raw_tges$CSG9)
glimpse(dfg)
glimpse(many_tges$`2015`$CSG9)
table(many_tges$`2015`$CSG9$`District rank`)
raw_2015 <- get_raw_tges(2015)
glimpse(raw_2015$CSG9)
glimpse(raw_tges$CSG9)
raw_2014 <- get_raw_tges(2014)
glimpse(raw_2014$CSG9)
raw_2017 <- get_raw_tges(2017)
glimpse(raw_2017$CSG9)
glimpse(raw_2017$CSG1)
glimpse(raw_2017$CSG2)
glimpse(raw_tges$CSG2)
raw_2012 <- get_raw_tges(2012)
glimpse(raw_2012$CSG2)
?grepl
both_years <- !grepl('11a|21a', names(raw_2017))
year_1 <- grepl('11a', names(raw_2017), fixed = TRUE) | both_years
year_1
year_2 <- grepl('21a', names(raw_2017), fixed = TRUE) | both_years
year_2
both_years
y1_df <- raw_2017[, year_1]
raw_2017 <- tges_name_cleaner(raw_2017)
both_years <- !grepl('11a|21a', names(raw_2017$CSG1))
year_1 <- grepl('11a', names(raw_2017$CSG1), fixed = TRUE) | both_years
year_2 <- grepl('21a', names(raw_2017$CSG1), fixed = TRUE) | both_years
y1_df <- df[, year_1]
year_1
year_2
glimpse(raw_2017)
glimpse(raw_2017$CSG1)
year_1 <- grepl('11', names(raw_2017$CSG1), fixed = TRUE) | both_years
year_1
year_1 <- grepl('11ab', names(raw_2017$CSG1), fixed = TRUE) | both_years
year_1
year_1 <- grepl('111ab', names(raw_2017$CSG1), fixed = TRUE) | both_years
year_1
year_1 <- grepl('y', names(raw_2017$CSG1), fixed = TRUE) | both_years
year_1
raw_tges_c <- year_variable_converter(raw_tges, 1999)
glimpse(raw_tges_c$CSG10)
glimpse(raw_tges$CSG10)
end_year <- 1999
old_id <- end_year - 1
old_ids <- c(old_id-2, old_id-1, old_id)
old_ids <- str_sub(old_ids, 3, 4)
on <- names(df)
on <- names(raw_tges$CSG10)
old_ids[3]
a <- on[grepl(old_ids[3], on)]
a
on[grepl(old_ids[3], on)] <- gsub(
pattern = old_ids[3],
replacement = '03',
x = on[grepl(old_ids[3], on)]
)
on[grepl(old_ids[3], on)]
old_ids[3]
on[grepl(old_ids[3], on)] <- gsub(
pattern = old_ids[3],
replacement = rep('03',3)
x = on[grepl(old_ids[3], on)]
)
on[grepl(old_ids[3], on)] <- gsub(
pattern = old_ids[3],
replacement = rep('03',3),
x = on[grepl(old_ids[3], on)]
)
old_ids[3]
x = on[grepl(old_ids[3], on)
}
x = on[grepl(old_ids[3], on)]
x
on[grepl(old_ids[3], on)]
old_ids
on[grepl(old_ids[3], on)]
on
on[grepl(old_ids[2], on)] <- gsub(
pattern = old_ids[2],
replacement = '02',
x = on[grepl(old_ids[2], on)]
)
on
on[grepl(old_ids[1], on)] <- gsub(
pattern = old_ids[1],
replacement = '01',
x = on[grepl(old_ids[1], on)]
)
names(df) <- on
on
glimpse(raw_tges)
glimpse(raw_tges_c$CSG10)
end_year <- 1999
old_id <- end_year - 1
old_ids <- c(old_id-2, old_id-1, old_id)
old_ids <- str_sub(old_ids, 3, 4)
on <- names(raw_tges$CSG10)
on[grepl(old_ids[3], on)] <- gsub(
pattern = old_ids[3],
replacement = '03',
x = on[grepl(old_ids[3], on)]
)
on[grepl(old_ids[2], on)] <- gsub(
pattern = old_ids[2],
replacement = '02',
x = on[grepl(old_ids[2], on)]
)
on[grepl(old_ids[1], on)] <- gsub(
pattern = old_ids[1],
replacement = '01',
x = on[grepl(old_ids[1], on)]
)
on
names(raw_tges_c$CSG10) <- on
glimpse(raw_tges_c$CSG10)
glimpse(raw_2017$CSG10)
all_years <- !grepl('00|01', names(raw_2017$CSG10))
all_years
names(raw_2017$CSG10)
all_years <- grepl('00|01', names(raw_2017$CSG10))
raw_2011 <- get_raw_tges(2011)
glimpse(raw_2011$CSG10)
glimpse(raw_2017$CSG10)
glimpse(raw_2017_c$CSG10)
glimpse(raw_tges_c$CSG10)
tidy <- tidy_total_spending_per_pupil(raw_tges_c$CSG10, 1999)
glimpse(tidy)
glimpse(tidy)
tidy2 <- tidy_total_spending_per_pupil(raw_2017$CSG10, 2017)
glimpse(tidy2)
