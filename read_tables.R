# This script reads Read code tables from a number of different sources
# and prepares a tibble of the ethnicity Read codes, their corresponding
# term and the categorisation used by the source.

# Note that I have assumed that there are no errors in the read_terms,
# i.e. that there are no read terms corresponding to the wrong read codes

# load libraries
library(tidyverse)
library(rvest)

# urls
vision_1_url <- 'http://help.visionhealth.co.uk/visiondatahub/clinical%20portal/Content/G_Full%20Help%20Topics/Reporting/Ethnicity%20Definitions.htm'
vision_2_url <- 'http://help.visionhealth.co.uk/reporting/1.3/Content/ExpRep%20Help%20Topics/5%20-%20Definitions/Ethnicity%20Codes.htm'
caliber_url <- 'https://www.caliberresearch.org/portal/show/ethnic_gprd'
phenotype_url <- 'https://phenotype.id/phenotypes/ethnic-status'
opensafely_url <- 'https://codelists.opensafely.org/codelist/opensafely/ethnicity/#full-list'
clinicalcodes_res56_url <- 'https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity/'

# read tables and save as tibbles
for (var in c('vision_1_url', 
              'vision_2_url', 
              'caliber_url', 
              'phenotype_url', 
              'opensafely_url', 
              'clinicalcodes_res56_url')) {
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

# clean each of the tibbles
# vision_1
vision_1_tibble <- vision_1_tibble %>%
  # remove first row (column names)
  slice(-1) %>%
  select(read_code = X1,
         read_term_vision_1 = X2,
         cat_vision_1 = X4) %>%
  filter(read_code != '') %>%
  # keep only first 5 characters of Read code
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  # order by Read code and term, and only keep top term where multiple
  # terms for one Read code
  arrange(read_code, read_term_vision_1) %>%
  distinct(read_code, .keep_all = TRUE)

# vision_2
vision_2_tibble <- vision_2_tibble %>%
  # remove first row (column names)
  slice(-1)  %>%
  mutate(V2_1 = str_split(X2, " , ", simplify = TRUE)[, 1],
         V2_2 = str_split(X2, " , ", simplify = TRUE)[, 2],
         V2_3 = str_split(X2, " , ", simplify = TRUE)[, 3],
         V2_4 = str_split(X2, " , ", simplify = TRUE)[, 4],
         V3_1 = str_split(X3, " , ", simplify = TRUE)[, 1],
         V3_2 = str_split(X3, " , ", simplify = TRUE)[, 2],
         V3_3 = str_split(X3, " , ", simplify = TRUE)[, 3],
         V3_4 = str_split(X3, " , ", simplify = TRUE)[, 4]) %>%
  select(-X2, -X3) %>%
  mutate_at(vars(starts_with('V')), list(~ if_else(.=='', 
                                                   NA_character_, 
                                                   .))) %>%
  gather('key', 'value', -X1) %>%
  select(cat_vision_2 = X1,
         read_code = value) %>% 
  # keep only first 5 characters of Read code
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  filter(!is.na(read_code)) %>%
  distinct(read_code, .keep_all = TRUE)

# caliber
caliber_tibble <- caliber_tibble %>%
  select(read_code = X2,
         read_term_caliber = X3,
         cat_caliber_1 = X1) %>%
  mutate(cat_caliber_1 = str_trim(str_remove(cat_caliber_1, '\\W\\d+\\W'), 
                                  side = 'right')) %>%
  # keep only first 5 characters of Read code
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  # order by Read code and term, and only keep top term where multiple
  # terms for one Read code
  arrange(read_code, read_term_caliber) %>%
  distinct(read_code, .keep_all = TRUE)

# phenotype
phenotype_tibble <- phenotype_tibble %>%
  select(read_code = X1,
         read_term_phenotype = X2,
         cat_phenotype_1 = X3) %>%
  # keep only first 5 characters of Read code
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  # order by Read code and term, and only keep top term where multiple
  # terms for one Read code
  arrange(read_code, read_term_phenotype) %>%
  distinct(read_code, .keep_all = TRUE)
  
# opensafely
# labels from here:
# https://github.com/opensafely/codelist-development/issues/7#issuecomment-620206708  
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
  # keep only first 5 characters of Read code
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  # order by Read code and term, and only keep top term where multiple
  # terms for one Read code
  arrange(read_code, read_term_opensafely) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  # join with the labels for the ethnicity codes
  left_join(opensafely_codes_2, by = 'code_opensafely_2') %>%
  select(-code_opensafely_2)
  
# clincalcodes
clinicalcodes_res56_tibble <- clinicalcodes_res56_tibble %>%
  select(read_code = X1,
         read_term_res56 = X3,
         cat_res56_1 = X6) %>%
  # keep only first 5 characters of Read code
  mutate_at('read_code', list(~ str_extract(., '.{5}'))) %>%
  # order by Read code and term, and only keep top term where multiple
  # terms for one Read code
  arrange(read_code, read_term_res56) %>%
  distinct(read_code, .keep_all = TRUE)

# join all the cleaned tibbles
rc_ethnicity <- vision_1_tibble %>%
  full_join(vision_2_tibble, by = c('read_code')) %>%
  full_join(caliber_tibble, by = c('read_code')) %>%
  full_join(phenotype_tibble, by = c('read_code')) %>%
  full_join(opensafely_tibble, by = c('read_code')) %>%
  full_join(clinicalcodes_res56_tibble, by = c('read_code')) 

# check for cases in which there are multiple read_terms for one Read code
code_dictionary <- rc_ethnicity %>% 
  select(read_code, starts_with('read_term')) %>% 
  gather('key', 'value', -read_code) %>% 
  filter(!is.na(value)) %>% 
  distinct(read_code, value) %>% 
  group_by(read_code) %>% 
  mutate(rn = str_c('read_term_', row_number())) %>%
  spread(key = rn, value = value) 

rc_ethnicity <- rc_ethnicity %>%
  select(-starts_with('read_term')) %>%
  full_join(code_dictionary, by = 'read_code')

write_csv(rc_ethnicity, path = 'rc_ethnicity.csv')
