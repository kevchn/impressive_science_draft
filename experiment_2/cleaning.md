---
title: "Data cleaning Experiment 2"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)     # create plots with ggplot, manipulate data, etc.
library(broom.mixed)   # convert regression models into nice tables
library(modelsummary)  # combine multiple regression models into a single table
library(lme4)          # model specification / estimation 
library(lmerTest)      # provides p-values in the output
library(ggpubr)        # stile feature of ggplot
library(gghalves)      # do special plots in ggplot
library(kableExtra)    # for tables
```

## Import data

```r
d <- read_csv("./data/qualtrics.csv")
names(d)
```

```
##  [1] "StartDate"             "EndDate"               "Status"               
##  [4] "IPAddress"             "Progress"              "Duration (in seconds)"
##  [7] "Finished"              "RecordedDate"          "ResponseId"           
## [10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
## [13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
## [16] "DistributionChannel"   "UserLanguage"          "consent"              
## [19] "attention"             "archeo_basic_learn"    "archeo_basic_impress" 
## [22] "archeo_basic_comp"     "archeo_basic_trust"    "archeo_basic_cons"    
## [25] "entom_imp_learn"       "entom_imp_impress"     "entom_imp_comp"       
## [28] "entom_imp_trust"       "entom_imp_cons"        "archeo_imp_learn"     
## [31] "archeo_imp_impress"    "archeo_imp_comp"       "archeo_imp_trust"     
## [34] "archeo_imp_cons"       "entom_basic_learn"     "entom_basic_impress"  
## [37] "entom_basic_comp"      "entom_basic_trust"     "entom_basic_cons"     
## [40] "Q1"                    "gender"                "age"                  
## [43] "education"             "PROLIFIC_PID"
```


```r
# inspect
head(d) # you can also use View(d)
```

```
## # A tibble: 6 × 44
##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
## 1 "Start Date" "End D… "Resp… "IP Addr… "Progre… "Duration (in seconds… "Finish…
## 2 "{\"ImportI… "{\"Im… "{\"I… "{\"Impo… "{\"Imp… "{\"ImportId\":\"dura… "{\"Imp…
## 3 "2024-03-07… "2024-… "0"    "82.68.2… "100"    "134"                  "1"     
## 4 "2024-03-07… "2024-… "0"    "90.249.… "100"    "123"                  "1"     
## 5 "2024-03-07… "2024-… "0"    "195.195… "100"    "163"                  "1"     
## 6 "2024-03-07… "2024-… "0"    "86.140.… "100"    "224"                  "1"     
## # ℹ abbreviated name: ¹​`Duration (in seconds)`
## # ℹ 37 more variables: RecordedDate <chr>, ResponseId <chr>,
## #   RecipientLastName <chr>, RecipientFirstName <chr>, RecipientEmail <chr>,
## #   ExternalReference <chr>, LocationLatitude <chr>, LocationLongitude <chr>,
## #   DistributionChannel <chr>, UserLanguage <chr>, consent <chr>,
## #   attention <chr>, archeo_basic_learn <chr>, archeo_basic_impress <chr>,
## #   archeo_basic_comp <chr>, archeo_basic_trust <chr>, …
```

```r
# delete first two rows
d <- d %>% 
  slice(3: nrow(.)) 
```

## Attention check

```r
# attention check
# to see different answers given (i.e.levels), transform into factor
d$attention <- as.factor(d$attention)
# check levels to see different answer types
levels(d$attention) 
```

```
## [1] "CX765PRW"                                                                       
## [2] "i pay attention"                                                                
## [3] "I pay attention"                                                                
## [4] "I pay attention. I really dislike this game, it's the most overrated game ever."
```

```r
table(d$attention)
```

```
## 
##                                                                        CX765PRW 
##                                                                               1 
##                                                                 i pay attention 
##                                                                               6 
##                                                                 I pay attention 
##                                                                              93 
## I pay attention. I really dislike this game, it's the most overrated game ever. 
##                                                                               1
```

There is one failed attention check. 


```r
# filter to only valid attention check responses
d <- d %>% filter(str_detect(attention, "attention"))
```

One participant had NA scores for all outcomes, so we decided to remove them.


```r
d <- d %>% filter(Finished!= 0)
```


## Re-shape data


```r
# check all names and their order
names(d)
```

```
##  [1] "StartDate"             "EndDate"               "Status"               
##  [4] "IPAddress"             "Progress"              "Duration (in seconds)"
##  [7] "Finished"              "RecordedDate"          "ResponseId"           
## [10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
## [13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
## [16] "DistributionChannel"   "UserLanguage"          "consent"              
## [19] "attention"             "archeo_basic_learn"    "archeo_basic_impress" 
## [22] "archeo_basic_comp"     "archeo_basic_trust"    "archeo_basic_cons"    
## [25] "entom_imp_learn"       "entom_imp_impress"     "entom_imp_comp"       
## [28] "entom_imp_trust"       "entom_imp_cons"        "archeo_imp_learn"     
## [31] "archeo_imp_impress"    "archeo_imp_comp"       "archeo_imp_trust"     
## [34] "archeo_imp_cons"       "entom_basic_learn"     "entom_basic_impress"  
## [37] "entom_basic_comp"      "entom_basic_trust"     "entom_basic_cons"     
## [40] "Q1"                    "gender"                "age"                  
## [43] "education"             "PROLIFIC_PID"
```



```r
# clean and re-shape
d <- d %>% 
  # add an easy to read participant identifier
  mutate(id = 1:nrow(.)) %>%
  # bring to long format
  pivot_longer(cols = c(starts_with("archeo"), starts_with("entom")), 
               names_to = "condition", values_to = "score") %>% 
  # separate conditions into CONVERGENCE_OUTCOME_STIMULUS
  separate_wider_delim(condition, "_", names = c("discipline", "impressiveness", 
                                                 "outcome")
                       ) %>%
  # necessary because of the way we labeled
  drop_na(score) %>% 
  pivot_wider(names_from = outcome, values_from = score) %>% 
  # create better variable names
  rename(competence = comp, 
         impressed = impress, 
         consensus = cons) %>% 
  # all variables are coded as `character` - make key variables numeric
  mutate(across(c(learn, competence, impressed, trust), as.numeric)
         )
```


## Add demographics

## Recode demographics


```r
prolific_demographics <- read_csv("./data/prolific.csv")

d <- left_join(d, prolific_demographics %>% select(-Age), by = c("PROLIFIC_PID" = "Participant id"))
```



```r
d <- d %>% 
  mutate(gender = case_when(Sex == "Male" ~ "male", 
                            Sex == "Female" ~  "female", 
                            .default = NA)
         ) 
```

## Export data


```r
write_csv(d, "data/cleaned.csv")
```
