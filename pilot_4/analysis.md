---
title: "Analysis Pilot 4"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)     # create plots with ggplot, manipulate data, etc.
```

```
## Warning: package 'stringr' was built under R version 4.2.3
```

```r
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
d <- read_csv("./data/cleaned.csv")
```


```r
# overall
d %>% 
  group_by(impressiveness) %>% 
  summarise(across(c(impressed, learn, know, competence, trust, cons), 
                   list(mean = ~mean(., na.rm=TRUE)), 
                   .names = "{col}_{fn}")) %>% 
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>% 
  kable() %>% 
  kable_classic(full_width = F)
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> impressiveness </th>
   <th style="text-align:right;"> impressed_mean </th>
   <th style="text-align:right;"> learn_mean </th>
   <th style="text-align:right;"> know_mean </th>
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
   <th style="text-align:right;"> cons_mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 2.80 </td>
   <td style="text-align:right;"> 2.61 </td>
   <td style="text-align:right;"> 3.11 </td>
   <td style="text-align:right;"> 2.95 </td>
   <td style="text-align:right;"> 3.36 </td>
   <td style="text-align:right;"> 4.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.09 </td>
   <td style="text-align:right;"> 3.84 </td>
   <td style="text-align:right;"> 4.09 </td>
   <td style="text-align:right;"> 3.77 </td>
   <td style="text-align:right;"> 3.78 </td>
   <td style="text-align:right;"> 3.84 </td>
  </tr>
</tbody>
</table>


```r
# by discipline
d %>% 
  group_by(discipline, impressiveness) %>% 
  summarise(across(c(impressed, learn, know, competence, trust, cons), 
                   list(mean = ~mean(., na.rm=TRUE)), 
                   .names = "{col}_{fn}")) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>% 
  kable() %>% 
  kable_classic(full_width = F)
```

```
## `summarise()` has grouped output by 'discipline'. You can override using the
## `.groups` argument.
## `mutate_if()` ignored the following grouping variables:
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> discipline </th>
   <th style="text-align:left;"> impressiveness </th>
   <th style="text-align:right;"> impressed_mean </th>
   <th style="text-align:right;"> learn_mean </th>
   <th style="text-align:right;"> know_mean </th>
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
   <th style="text-align:right;"> cons_mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> archeo </td>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 3.14 </td>
   <td style="text-align:right;"> 2.59 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 2.59 </td>
   <td style="text-align:right;"> 3.28 </td>
   <td style="text-align:right;"> 4.22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> archeo </td>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 4.05 </td>
   <td style="text-align:right;"> 4.18 </td>
   <td style="text-align:right;"> 3.86 </td>
   <td style="text-align:right;"> 3.92 </td>
   <td style="text-align:right;"> 3.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> entom </td>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 2.45 </td>
   <td style="text-align:right;"> 2.64 </td>
   <td style="text-align:right;"> 3.23 </td>
   <td style="text-align:right;"> 3.32 </td>
   <td style="text-align:right;"> 3.45 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> entom </td>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.05 </td>
   <td style="text-align:right;"> 3.64 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 3.68 </td>
   <td style="text-align:right;"> 3.62 </td>
   <td style="text-align:right;"> 3.92 </td>
  </tr>
</tbody>
</table>


```r
# between versions
d %>% 
  group_by(impressiveness, version) %>% 
summarise(across(c(impressed, learn, know, competence, trust, cons), 
                   list(mean = ~mean(., na.rm=TRUE)), 
                   .names = "{col}_{fn}")) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>% 
  kable() %>% 
  kable_classic(full_width = F)
```

```
## `summarise()` has grouped output by 'impressiveness'. You can override using
## the `.groups` argument.
## `mutate_if()` ignored the following grouping variables:
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> impressiveness </th>
   <th style="text-align:left;"> version </th>
   <th style="text-align:right;"> impressed_mean </th>
   <th style="text-align:right;"> learn_mean </th>
   <th style="text-align:right;"> know_mean </th>
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
   <th style="text-align:right;"> cons_mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:left;"> all questions </td>
   <td style="text-align:right;"> 2.80 </td>
   <td style="text-align:right;"> 2.61 </td>
   <td style="text-align:right;"> 3.11 </td>
   <td style="text-align:right;"> 2.95 </td>
   <td style="text-align:right;"> 3.05 </td>
   <td style="text-align:right;"> 4.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:left;"> trust only </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> 3.75 </td>
   <td style="text-align:right;"> 3.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:left;"> all questions </td>
   <td style="text-align:right;"> 4.09 </td>
   <td style="text-align:right;"> 3.84 </td>
   <td style="text-align:right;"> 4.09 </td>
   <td style="text-align:right;"> 3.77 </td>
   <td style="text-align:right;"> 3.80 </td>
   <td style="text-align:right;"> 4.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:left;"> trust only </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> 3.75 </td>
   <td style="text-align:right;"> 3.61 </td>
  </tr>
</tbody>
</table>




