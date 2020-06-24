Preparing an ethnicity Read code list
================
Elsie Horne
23/06/2020

The following script loads `rc_ethnicity.csv`, the lists of Read codes scraped in `read_tables.R`. All Read codes are then mapped to the [categories used in the OpenSafely project](https://github.com/opensafely/codelist-development/issues/7#issuecomment-620206708)

``` r
library(tidyverse)
```

``` r
rc_ethnicity <- read_csv('rc_ethnicity.csv') %>%
  mutate(cat_final = cat_opensafely_2)
```

View the data where the Read codes are in the OpenSafely list:

``` r
rc_ethnicity %>%
  filter(!is.na(cat_final)) %>%
  select(read_code, read_term_1, read_term_2, cat_final)
```

    ## # A tibble: 266 x 4
    ##    read_code read_term_1                read_term_2                    cat_final
    ##    <chr>     <chr>                      <chr>                          <chr>    
    ##  1 9S1..     White                      White - ethnic group           White    
    ##  2 9S2..     Black Caribbean            <NA>                           Black    
    ##  3 9S3..     Black African              <NA>                           Black    
    ##  4 9S4..     Black other non-mixed ori… Black, other, non-mixed origin Black    
    ##  5 9S41.     Black British              <NA>                           Black    
    ##  6 9S42.     Black Caribbean            Black Caribbean &/or W.I. &/o… Black    
    ##  7 9S43.     Black Arab                 Black N African &/or Arab &/o… Black    
    ##  8 9S44.     Black - other African cou… <NA>                           Black    
    ##  9 9S45.     Black E Afric Asia/Indo-C… Black E Afric Asia &/or Indo-… Black    
    ## 10 9S46.     Black Indian sub-continent <NA>                           Black    
    ## # … with 256 more rows

View the data where the Read codes are *not* in the OpenSafely list:

``` r
rc_ethnicity %>%
  filter(is.na(cat_final)) %>%
  select(read_code, read_term_1, read_term_2, cat_final)
```

    ## # A tibble: 178 x 4
    ##    read_code read_term_1                                   read_term_2 cat_final
    ##    <chr>     <chr>                                         <chr>       <chr>    
    ##  1 916E.     Patient ethnicity unknown                     <NA>        <NA>     
    ##  2 9i...     Ethnic category - 2001 census                 <NA>        <NA>     
    ##  3 9i0..     British or mixed British - ethnic category 2… <NA>        <NA>     
    ##  4 9i00.     White British - ethnic category 2001 census   <NA>        <NA>     
    ##  5 9i1..     Irish - ethnic category 2001 census           <NA>        <NA>     
    ##  6 9i10.     White Irish - ethnic category 2001 census     <NA>        <NA>     
    ##  7 9i2..     Other White background - ethnic category 200… <NA>        <NA>     
    ##  8 9i20.     English - ethnic category 2001 census         <NA>        <NA>     
    ##  9 9i21.     Scottish - ethnic category 2001 census        <NA>        <NA>     
    ## 10 9i22.     Welsh - ethnic category 2001 census           <NA>        <NA>     
    ## # … with 168 more rows

As the OpenSafely Read codes are all version 3 Read codes, I map the categories (`cat_opensafely_2`) to the version 2 Read codes by joining on the Read term (`read_term`). I remove the census year from `read_term` before doing so. There are two key assumptions here:
1. All Read terms correspond to the correct Read code (i.e. there are no errors in the tables that I scraped from the websites).
2. The 2001 and 2011 census Read codes should be mapped to the same category if identical other than the year.

I do this first for `read_term_1`, and will repeat for `read_term_2` if necessary.

``` r
read_term_join <- rc_ethnicity %>% 
  rename(read_term = read_term_1) %>%
  # rows which have a final category
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

Check the read\_codes which have not yet been assigned a cat\_final category.

``` r
rc_ethnicity %>%
  filter(is.na(cat_final)) %>%
  select(read_code, read_term) %>%
  print(n=20)
```

    ## # A tibble: 16 x 2
    ##    read_code read_term                                                  
    ##    <chr>     <chr>                                                      
    ##  1 916E.     Patient ethnicity unknown                                  
    ##  2 9i...     Ethnic category - 2001 census                              
    ##  3 9iFB.     Mid East (excl Israeli  Iranian & Arab) - eth cat 2001 cens
    ##  4 9iG..     Ethnic category not stated - 2001 census                   
    ##  5 9S...     Ethnic groups (census)                                     
    ##  6 9SD..     Ethnic group not given - patient refused                   
    ##  7 9SE..     Ethnic group not recorded                                  
    ##  8 9SZ..     Ethnic groups (census) NOS                                 
    ##  9 9t...     Ethnic category - 2011 census                              
    ## 10 9t0..     Ethnic category - 2011 census England and Wales            
    ## 11 9t1..     Ethnic category - 2011 census Northern Ireland             
    ## 12 9t2..     Ethnic category - 2011 census Scotland                     
    ## 13 9t28.     Asian: Indian Indian Scot/Indian Brit- Scotland 2011 census
    ## 14 9t29.     Bangladeshi Bangladeshi Scot or Bangladeshi Brit- Scot 2011
    ## 15 XaJRB     <NA>                                                       
    ## 16 9T...     Ethnicity and other related nationality data

Manually sort out these reamaining ones. The missing term for Read code (XaJRB) is 'Ethnic category not stated - 2001 census'.

``` r
rc_ethnicity <- rc_ethnicity %>%
  mutate_at('cat_final', list(~ case_when(!is.na(cat_final) ~ .,
                                          is.na(cat_final) & read_term %in% c('Mid East (excl Israeli  Iranian & Arab) - eth cat 2001 cens') ~ 'Other',
                                          is.na(cat_final) & read_term %in% c('Asian: Indian Indian Scot/Indian Brit- Scotland 2011 census',
                                                           'Bangladeshi Bangladeshi Scot or Bangladeshi Brit- Scot 2011') ~ 'Asian British',
                                          TRUE ~ NA_character_))) %>%
  filter(!is.na(cat_final)) %>%
  select(read_code, read_term, cat_final)
```

``` r
rc_ethnicity %>%
  group_by(cat_final) %>%
  count()
```

    ## # A tibble: 5 x 2
    ## # Groups:   cat_final [5]
    ##   cat_final         n
    ##   <chr>         <int>
    ## 1 Asian British    71
    ## 2 Black            76
    ## 3 Mixed            56
    ## 4 Other            95
    ## 5 White           147

``` r
write_csv(rc_ethnicity, path = 'rc_ethnicity_final.csv')
```
