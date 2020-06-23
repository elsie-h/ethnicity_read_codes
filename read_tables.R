library(dplyr)
library(stringr)
library(rvest)

vision_url <- 'http://help.visionhealth.co.uk/visiondatahub/clinical%20portal/Content/G_Full%20Help%20Topics/Reporting/Ethnicity%20Definitions.htm'
caliber_url <- 'https://www.caliberresearch.org/portal/show/ethnic_gprd'
opensafely_url <- 'https://codelists.opensafely.org/codelist/opensafely/ethnicity/#full-list'
clinicalcodes_res56_url <- 'https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity/'

# read tables
for (var in c('vision_url', 'caliber_url', 'opensafely_url', 'clinicalcodes_res56_url')) {
  url <- eval(parse(text = var))
  rc_table <- read_html(url) %>%
    html_nodes('table') 
  rc_tables <- html_table(rc_table)
  n_rows <- unlist(lapply(rc_tables, function(t) dim(t)[1]))
  rc_table <- rc_tables[[which.max(n_rows)]]
  colnames(rc_table) <- str_c('X', 1:dim(rc_table)[2])
  rc_tibble <- as_tibble(rc_table)
  rc_name <- str_replace(var, '_url', '_tibble')
  assign(rc_name, rc_tibble)
}

# clean tables
# vision
col_names <- slice(vision_tibble, 1) %>% unlist() %>% unname()
names(vision_tibble) <- col_names
vision_tibble <- vision_tibble %>%
  slice(-1) %>%
  select(read_code = `Read Code`,
         read_term_vision = `Read Term`,
         cat_vision_1 = Category) %>%
  filter(read_code != '') %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  mutate(vision = 1)

# caliber
caliber_tibble <- caliber_tibble %>%
  select(read_code = X2,
         read_term_caliber = X3,
         cat_caliber_1 = X1) %>%
  mutate(code_caliber1 = str_extract(cat_caliber_1, '(\\d+)'),
         cat_caliber_1 = str_trim(str_remove(cat_caliber_1, '\\W\\d+\\W'), side = 'right')) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  mutate(caliber = 1)
  
# opensafely
opensafely_tibble <- opensafely_tibble %>%
  select(read_code = X2,
         read_term_opensafely = X1,
         code_opensafely_1 = X3,
         code_opensafely_2 = X4) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  mutate(opensafely = 1)
  
# clincalcodes
clinicalcodes_res56_tibble <- clinicalcodes_res56_tibble %>%
  select(read_code = X1,
         read_term_res56 = X3,
         cat_res56_1 = X6) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  mutate(res56 = 1)

rc_ethnicity <- vision_tibble %>%
  full_join(caliber_tibble, by = 'read_code') %>%
  full_join(opensafely_tibble, by = 'read_code') %>%
  full_join(clinicalcodes_res56_tibble, by = 'read_code')
  
# check categories
sapply(select(rc_ethnicity, starts_with('cat'), starts_with('code')), n_distinct)

addmargins(table(rc_ethnicity$cat_vision_1, rc_ethnicity$cat_res56_1, useNA = 'always'))
addmargins(table(rc_ethnicity$code_opensafely_2, rc_ethnicity$cat_res56_1, useNA = 'always'))


res56 <- read_csv("https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity/download/")
opensafely <- read_csv("https://codelists.opensafely.org/codelist/opensafely/ethnicity/2020-04-27/download.csv")
