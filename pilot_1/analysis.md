---
title: "Analysis"
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
  summarise(across(c(impressed, learn, competence, trust), 
                   list(mean = ~mean(.)), 
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
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 3.62 </td>
   <td style="text-align:right;"> 3.54 </td>
   <td style="text-align:right;"> 4.23 </td>
   <td style="text-align:right;"> 4.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.15 </td>
   <td style="text-align:right;"> 4.05 </td>
   <td style="text-align:right;"> 4.41 </td>
   <td style="text-align:right;"> 4.13 </td>
  </tr>
</tbody>
</table>

```r
# by discipline
d %>% 
  group_by(discipline, impressiveness) %>% 
  summarise(across(c(impressed, learn, competence, trust), 
                   list(mean = ~mean(.)), 
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
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ants </td>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 3.90 </td>
   <td style="text-align:right;"> 3.90 </td>
   <td style="text-align:right;"> 4.15 </td>
   <td style="text-align:right;"> 4.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ants </td>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.16 </td>
   <td style="text-align:right;"> 4.26 </td>
   <td style="text-align:right;"> 4.42 </td>
   <td style="text-align:right;"> 4.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> archeo </td>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 3.32 </td>
   <td style="text-align:right;"> 3.16 </td>
   <td style="text-align:right;"> 4.32 </td>
   <td style="text-align:right;"> 3.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> archeo </td>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.15 </td>
   <td style="text-align:right;"> 3.85 </td>
   <td style="text-align:right;"> 4.40 </td>
   <td style="text-align:right;"> 4.25 </td>
  </tr>
</tbody>
</table>





