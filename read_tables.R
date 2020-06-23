
# Notes
# I have assumed that there are no errors in the read_terms,
# when they differ I assume that this is subtle differences, but that
# they have the same meaning

library(tidyverse)
library(rvest)

vision_1_url <- 'http://help.visionhealth.co.uk/visiondatahub/clinical%20portal/Content/G_Full%20Help%20Topics/Reporting/Ethnicity%20Definitions.htm'
vision_2_url <- 'http://help.visionhealth.co.uk/reporting/1.3/Content/ExpRep%20Help%20Topics/5%20-%20Definitions/Ethnicity%20Codes.htm'
caliber_url <- 'https://www.caliberresearch.org/portal/show/ethnic_gprd'
phenotype_url <- 'https://phenotype.id/phenotypes/ethnic-status'
opensafely_url <- 'https://codelists.opensafely.org/codelist/opensafely/ethnicity/#full-list'
clinicalcodes_res56_url <- 'https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity/'

# read tables
for (var in c('vision_1_url', 'vision_2_url', 'caliber_url', 'phenotype_url', 'opensafely_url', 'clinicalcodes_res56_url')) {
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

# clean
# vision_1
col_names <- slice(vision_1_tibble, 1) %>% unlist() %>% unname()
names(vision_1_tibble) <- col_names
vision_1_tibble <- vision_1_tibble %>%
  slice(-1) %>%
  select(read_code = `Read Code`,
         read_term_vision_1 = `Read Term`,
         cat_vision_1 = Category) %>%
  filter(read_code != '') %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  arrange(read_code, read_term_vision_1) %>%
  distinct(read_code, .keep_all = TRUE)

# vision_2
col_names <- slice(vision_2_tibble, 1) %>% unlist() %>% unname()
names(vision_2_tibble) <- col_names
vision_2_tibble <- vision_2_tibble %>%
  slice(-1)  %>%
  mutate(V2_1 = str_split(`Read V2`, " , ", simplify = TRUE)[, 1],
         V2_2 = str_split(`Read V2`, " , ", simplify = TRUE)[, 2],
         V2_3 = str_split(`Read V2`, " , ", simplify = TRUE)[, 3],
         V2_4 = str_split(`Read V2`, " , ", simplify = TRUE)[, 4],
         V3_1 = str_split(`CTV3`, " , ", simplify = TRUE)[, 1],
         V3_2 = str_split(`CTV3`, " , ", simplify = TRUE)[, 2],
         V3_3 = str_split(`CTV3`, " , ", simplify = TRUE)[, 3],
         V3_4 = str_split(`CTV3`, " , ", simplify = TRUE)[, 4]) %>%
  select(-`Read V2`, -CTV3) %>%
  mutate_at(vars(starts_with('V')), list(~ if_else(.=='', NA_character_, .))) %>%
  gather('key', 'value', -Description) %>%
  select(cat_vision_2 = Description,
         read_code = value) %>% 
  filter(!is.na(read_code)) 

# caliber
caliber_tibble <- caliber_tibble %>%
  select(read_code = X2,
         read_term_caliber = X3,
         cat_caliber_1 = X1) %>%
  mutate(cat_caliber_1 = str_trim(str_remove(cat_caliber_1, '\\W\\d+\\W'), side = 'right')) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  arrange(read_code, read_term_caliber) %>%
  distinct(read_code, .keep_all = TRUE)

# phenotype
phenotype_tibble <- phenotype_tibble %>%
  select(read_code = X1,
         read_term_phenotype = X2,
         cat_phenotype_1 = X3) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  arrange(read_code, read_term_phenotype) %>%
  distinct(read_code, .keep_all = TRUE)
  
# opensafely
opensafely_codes_2 <- tribble(~code_opensafely_2, ~cat_opensafely_2,
                            1, 'White',
                            2, 'Mixed',
                            3, 'Asian British',
                            4, 'Black',
                            5, 'Other')

opensafely_tibble <- opensafely_tibble %>%
  select(read_code = X2,
         read_term_opensafely = X1,
         # code_opensafely_1 = X3, drop as I can't find the labels
         code_opensafely_2 = X4
         ) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  arrange(read_code, read_term_opensafely) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  left_join(opensafely_codes_2, by = 'code_opensafely_2') %>%
  select(-code_opensafely_2)
  
# clincalcodes
clinicalcodes_res56_tibble <- clinicalcodes_res56_tibble %>%
  select(read_code = X1,
         read_term_res56 = X3,
         cat_res56_1 = X6) %>%
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  arrange(read_code, read_term_res56) %>%
  distinct(read_code, .keep_all = TRUE)

rc_ethnicity <- vision_1_tibble %>%
  full_join(vision_2_tibble, by = c('read_code')) %>%
  full_join(caliber_tibble, by = c('read_code')) %>%
  full_join(phenotype_tibble, by = c('read_code')) %>%
  full_join(opensafely_tibble, by = c('read_code')) %>%
  full_join(clinicalcodes_res56_tibble, by = c('read_code')) %>%
  mutate(read_term = case_when(!is.na(read_term_vision_1) ~ read_term_vision_1,
                               !is.na(read_term_caliber) ~ read_term_caliber,
                               !is.na(read_term_opensafely) ~ read_term_opensafely,
                               !is.na(read_term_res56) ~ read_term_res56,
                               TRUE ~ NA_character_)) %>%
  select(-starts_with('read_term_'))

write_csv(rc_ethnicity, path = 'rc_ethnicity.csv')

  
# # check categories
# # https://github.com/opensafely/codelist-development/issues/7#issuecomment-620206708
# 
# opensafely_codes <- tribble(~code, ~description,
#                             1, 'White',
#                             2, 'Mixed',
#                             3, 'Asian British',
#                             4, 'Black',
#                             5, 'Other')
# 
# lapply(select(rc_ethnicity, starts_with('cat'), starts_with('code')), function(x) levels(as.factor(x)))
# 
# rc_ethnicity %>%
#   mutate(cat_final = case_when(cat_vision_1 %in% c('White'
#                                                    'Black',
#                                                    'Mixed',
#                                                    'Other') ~ cat_vision_1,
#                                cat_vision_1 %in% 'Unknown' ~ NA_character_,
#                                ####
#                                str_detect(cat_vision_2, '^Asian or Asian British') ~ 'Asian British',
#                                str_detect(cat_vision_2, '^Black') ~ 'Black',
#                                str_detect(cat_vision_2, '^Mixed') ~ 'Mixed',
#                                str_detect(cat_vision_2, '^Other') ~ 'Other',
#                                str_detect(cat_vision_2, '^White') ~ 'White',
#                                ####
#                                cat_caliber_1 %in% c('Bangladeshi',
#                                                     'Indian',
#                                                     'Pakistani') ~ 'Asian British',
#                                cat_caliber_1 %in% c('Black African',
#                                                     'Black Caribbean',
#                                                     'Black Other') ~ 'Black',
#                                cat_caliber_1 %in% c('Chinese',
#                                                     'Other Asian',
#                                                     'Other ethnic group') ~ 'Other',
#                                str_detect(cat_caliber_1, '^Mixed') ~ 'Mixed',
#                                str_detect(cat_caliber_1, '^White') ~ 'White',
#                                ####
#                                cat_phenotype_1 %in% c('Bangladeshi',
#                                                     'Indian',
#                                                     'Pakistani') ~ 'Asian British',
#                                cat_phenotype_1 %in% c('Black African',
#                                                     'Black Caribbean',
#                                                     'Black Other') ~ 'Black',
#                                cat_phenotype_1 %in% c('Chinese',
#                                                     'Other Asian',
#                                                     'Other ethnic group') ~ 'Other',
#                                str_detect(cat_phenotype_1, '^Mixed') ~ 'Mixed',
#                                str_detect(cat_phenotype_1, '^White') ~ 'White',
#                                ####
#                                cat_res56_1 == 'Asian/British Asian' ~ 'Asian British',
#                                
#                                ))
#   
# 
# sapply(select(rc_ethnicity, starts_with('cat'), starts_with('code')), n_distinct)
# 
# 
# 
# 
# addmargins(table(rc_ethnicity$cat_vision_1, rc_ethnicity$cat_caliber_1, useNA = 'always'))
# 
# 
# addmargins(table(rc_ethnicity$cat_vision_2, rc_ethnicity$cat_caliber_1, useNA = 'always'))
# 
# addmargins(table(rc_ethnicity$cat_vision_1, rc_ethnicity$cat_res56_1, useNA = 'always'))
# addmargins(table(rc_ethnicity$code_opensafely_2, rc_ethnicity$cat_res56_1, useNA = 'always'))
# 
# 
# res56 <- read_csv("https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity/download/")
# opensafely <- read_csv("https://codelists.opensafely.org/codelist/opensafely/ethnicity/2020-04-27/download.csv")
