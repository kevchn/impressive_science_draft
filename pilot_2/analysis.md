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
                   .names = "{col}_{fn}"), 
            n = n()) %>% 
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
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:right;"> 3.25 </td>
   <td style="text-align:right;"> 3.20 </td>
   <td style="text-align:right;"> 2.85 </td>
   <td style="text-align:right;"> 3.15 </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 3.55 </td>
   <td style="text-align:right;"> 3.95 </td>
   <td style="text-align:right;"> 3.70 </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
</tbody>
</table>


```r
# between answer options
d %>% 
  group_by(impressiveness, labels) %>% 
  summarise(across(c(impressed, learn, competence, trust), 
                   list(mean = ~mean(.)), 
                   .names = "{col}_{fn}"), 
            n = n()) %>% 
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
   <th style="text-align:left;"> labels </th>
   <th style="text-align:right;"> impressed_mean </th>
   <th style="text-align:right;"> learn_mean </th>
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:left;"> labels </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> basic </td>
   <td style="text-align:left;"> nolabels </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:left;"> labels </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:left;"> nolabels </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
</tbody>
</table>


```r
# between answer options
d %>% 
  group_by(labels) %>% 
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
   <th style="text-align:left;"> labels </th>
   <th style="text-align:right;"> impressed_mean </th>
   <th style="text-align:right;"> learn_mean </th>
   <th style="text-align:right;"> competence_mean </th>
   <th style="text-align:right;"> trust_mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> labels </td>
   <td style="text-align:right;"> 3.75 </td>
   <td style="text-align:right;"> 3.35 </td>
   <td style="text-align:right;"> 3.35 </td>
   <td style="text-align:right;"> 3.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nolabels </td>
   <td style="text-align:right;"> 3.80 </td>
   <td style="text-align:right;"> 3.40 </td>
   <td style="text-align:right;"> 3.45 </td>
   <td style="text-align:right;"> 3.35 </td>
  </tr>
</tbody>
</table>



