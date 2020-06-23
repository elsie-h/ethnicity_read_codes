Untitled
================
Elsie Horne
23/06/2020

R Markdown
----------

``` r
library(tidyverse)
```

``` r
rc_ethnicity <- read_csv('rc_ethnicity.csv')
```

test
----

I use the ethnicity codes specified by OpenSafely: <https://github.com/opensafely/codelist-development/issues/7#issuecomment-620206708>

``` r
rc_ethnicity %>%
  filter(!is.na(cat_vision_2), !is.na(cat_opensafely_2)) %>%
  select(read_code, cat_vision_2, cat_opensafely_2) %>%
  arrange(cat_opensafely_2) %>% 
  print(n=50)
```

    ## # A tibble: 33 x 3
    ##    read_code cat_vision_2                                       cat_opensafely_2
    ##    <chr>     <chr>                                              <chr>           
    ##  1 Xactg     Asian or Asian British: Indian                     Asian British   
    ##  2 Xacth     Asian or Asian British: Pakistani                  Asian British   
    ##  3 Xacti     Asian or Asian British: Bangladeshi                Asian British   
    ##  4 Xactk     Asian or Asian British: Any other Asian background Asian British   
    ##  5 XaJR2     Asian or Asian British: Indian                     Asian British   
    ##  6 XaJR3     Asian or Asian British: Pakistani                  Asian British   
    ##  7 XaJR4     Asian or Asian British: Bangladeshi                Asian British   
    ##  8 Xactl     Black or African or Caribbean or Black British: A… Black           
    ##  9 Xactm     Black or African or Caribbean or Black British: C… Black           
    ## 10 Xactn     Black or African or Caribbean or Black British: A… Black           
    ## 11 XactL     Mixed or multiple ethnic groups: White and Black … Mixed           
    ## 12 Xactd     Mixed or multiple ethnic groups: White and Black … Mixed           
    ## 13 Xacte     Mixed or multiple ethnic groups: White and Asian   Mixed           
    ## 14 Xactf     Mixed or multiple ethnic groups: Any other Mixed … Mixed           
    ## 15 XaJQy     Mixed or multiple ethnic groups: White and Black … Mixed           
    ## 16 XaJR0     Mixed or multiple ethnic groups: White and Asian   Mixed           
    ## 17 XaJRL     Asian or Asian British: Chinese                    Mixed           
    ## 18 Xactj     Asian or Asian British: Chinese                    Other           
    ## 19 Xacto     Other ethnic group: Arab                           Other           
    ## 20 Xactp     Other ethnic group: Any other ethnic group         Other           
    ## 21 XaJSS     Other ethnic group: Arab                           Other           
    ## 22 XaJSg     Other ethnic group: Any other ethnic group         Other           
    ## 23 XaJR9     Asian or Asian British: Chinese                    Other           
    ## 24 XaJRA     Other ethnic group: Any other ethnic group         Other           
    ## 25 XactH     White: English or Welsh or Scottish or Northern I… White           
    ## 26 XactI     White: Irish                                       White           
    ## 27 XactJ     White: Gypsy or Irish Traveller                    White           
    ## 28 XactK     White: Any other White background                  White           
    ## 29 XaQEa     White: English or Welsh or Scottish or Northern I… White           
    ## 30 XaQEb     White: Irish                                       White           
    ## 31 XaJSD     White: Gypsy or Irish Traveller                    White           
    ## 32 XaJSB     White: Gypsy or Irish Traveller                    White           
    ## 33 XaJSC     White: Gypsy or Irish Traveller                    White

These correspond nicely, so cat\_version\_2 can be used to categorise version 2 Read codes, which are not in the OpenSafely list (OpenSafely only covers version 3).

``` r
rc_ethnicity <- rc_ethnicity %>%
  mutate(cat_final = case_when(!is.na(cat_opensafely_2) ~ cat_opensafely_2,
                               cat_vision_2 == 'Asian or Asian British: Chinese' ~ 'Other',
                               str_detect(cat_vision_2, '^Asian') ~ 'Asian British',
                               str_detect(cat_vision_2, '^Black') ~ 'Black',
                               str_detect(cat_vision_2, '^Other') ~ 'Other',
                               str_detect(cat_vision_2, '^White') ~ 'White',
                               TRUE ~ NA_character_
                               ))
```

Try joining on Read codes, in case any read\_term matches, but not read\_code (i.e. different versions of Read code). Remove the census year.

``` r
read_term_join <- rc_ethnicity %>% 
  rename(read_term = read_term_1) %>%
  # rows which have a final catego
  filter(!is.na(cat_final)) %>%
  select(read_term, cat_final) %>%
  # remove the census year
  mutate(join_var = str_trim(str_remove(read_term, '20..'), side = 'right')) %>%
  select(join_cat_final = cat_final, join_var)

rc_ethnicity <- rc_ethnicity %>% 
  rename(read_term = read_term_1) %>%
  # remove the census year
  mutate(join_var = str_trim(str_remove(read_term, '20..'), side = 'right')) %>%
  left_join(read_term_join, by = 'join_var') %>%
  mutate_at('cat_final', list(~ case_when(is.na(cat_final) & !is.na(join_cat_final) ~ join_cat_final,
                                          TRUE ~ .))) %>%
  select(-starts_with('join'))
```

Check the remaining ones.

``` r
rc_ethnicity %>%
  filter(is.na(cat_final), !is.na(read_term)) %>%
  select(read_term) %>%
  print(n=20)
```

    ## # A tibble: 15 x 1
    ##    read_term                                                  
    ##    <chr>                                                      
    ##  1 Patient ethnicity unknown                                  
    ##  2 Ethnic category - 2001 census                              
    ##  3 Mid East (excl Israeli  Iranian & Arab) - eth cat 2001 cens
    ##  4 Ethnic category not stated - 2001 census                   
    ##  5 Ethnic groups (census)                                     
    ##  6 Ethnic group not given - patient refused                   
    ##  7 Ethnic group not recorded                                  
    ##  8 Ethnic groups (census) NOS                                 
    ##  9 Ethnic category - 2011 census                              
    ## 10 Ethnic category - 2011 census England and Wales            
    ## 11 Ethnic category - 2011 census Northern Ireland             
    ## 12 Ethnic category - 2011 census Scotland                     
    ## 13 Asian: Indian Indian Scot/Indian Brit- Scotland 2011 census
    ## 14 Bangladeshi Bangladeshi Scot or Bangladeshi Brit- Scot 2011
    ## 15 Ethnicity and other related nationality data

Manually sort out the reamaining ones.

``` r
rc_ethnicity <- rc_ethnicity %>%
  mutate_at('cat_final', list(~ case_when(!is.na(cat_final) ~ .,
                                          is.na(cat_final) & read_term %in% c('Mid East (excl Israeli  Iranian & Arab) - eth cat 2001 cens') ~ 'Other',
                                          is.na(cat_final) & read_term %in% c('Asian: Indian Indian Scot/Indian Brit- Scotland 2011 census',
                                                           'Bangladeshi Bangladeshi Scot or Bangladeshi Brit- Scot 2011') ~ 'Asian British',
                                          TRUE ~ NA_character_))) %>%
  filter(!is.na(cat_final))
```
