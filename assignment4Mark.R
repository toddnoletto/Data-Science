---
title: "Assignment4"
author: "Todd Noletto"
date: "March 2, 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r}
chooseCRANmirror(graphics=FALSE, ind=1)
library(tidyverse)
library(curl)
library(devtools)
library(MRIcloudT1volumetrics)
install.packages("ggmosaic")
library(ggmosaic)
```

```{r}
healthCare_dat= read_csv("https://raw.githubusercontent.com/jhu-advdatasci/2018/master/data/KFF/healthcare-spending.csv", skip = 2,  n_max = 52)
```



```{r}
newdat= MRIcloudT1volumetrics::readFileList("https://raw.githubusercontent.com/neuroconductor/kirby21.mricloud/master/inst/visit_1/127/kirby_3_1_ax_283Labels_M2_corrected_stats.txt")
levl1_iv = newdat %>% 
    filter (type == 1, level == 1) %>%
    summarise(intracranial_volume = sum(volume))
levl1_iv
```

```{r}
q2_dat = read_tsv("https://raw.githubusercontent.com/bcaffo/ds4ph-bme/master/data/classInterests.txt")
q2_dat %>%
    group_by(Program)%>%
    count(name = "number_of_students") %>%
    ggplot(aes(x = Program, y= number_of_students)) +
    geom_col()
q2_dat %>%
    group_by(Year)%>%
    count(name = "number_of_students") %>%
    ggplot(aes(x = Year, y= number_of_students)) +
    geom_col()
```
```{r}
q2_dat %>%
    ggplot() +
    geom_mosaic(aes(x = product(Year), fill = Year, 
                    conds = product(Program)), offset = 0.005) +
    theme_mosaic() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("")
```

```{r}
expression_dat = read_csv("https://raw.githubusercontent.com/jhu-advdatasci/2018/master/data/GSE5859_exprs.csv") %>%
    column_to_rownames(var = "X1")

row_means_matrix <- expression_dat %>%
    mutate(mean = rowMeans(across(everything()))) %>%
    mutate(across(everything(), ~ mean)) %>%
    select(-mean)
col_means_matrix <- expression_dat %>%
    mutate(across(everything(), ~ mean(.)))
demean_expression_matrix <- expression_dat - row_means_matrix - col_means_matrix
demean_expression_matrix[1:5,1:5]
```

```{r}
healthcare_dat <- read_csv("https://raw.githubusercontent.com/jhu-advdatasci/2018/master/data/KFF/healthcare-spending.csv", skip = 2,  n_max = 52)

healthcare_dat_tidy <- healthcare_dat %>% 
    pivot_longer(cols = `1991__Total Health Spending`:`2014__Total Health Spending`,
                 names_to = "year",
                 values_to = "total_health_spending") %>%
    separate(year, "year", sep = "_") %>%
    mutate(year = as.numeric(year))

healthcare_dat_tidy %>%
    filter(Location != "United States") %>%
    ggplot(aes(x= year, y = (total_health_spending/10^3), color = Location)) + 
    geom_line() +
    geom_point(size = .4) +
    theme_light() +
    ylab("Total Healthcare Spending (Billions of USD)") 
```

```{r}
healthcare_dat_tidy %>%
    filter(Location != "United States") %>%
    group_by(Location) %>%
    summarise(mean_healthcare_spending = (mean(total_health_spending)/10^3),
              sd = sd((total_health_spending/10^3))) %>%
    ggplot(aes(x = fct_reorder(Location, mean_healthcare_spending, .desc = TRUE),
               y =mean_healthcare_spending)) +
    geom_col(fill = "#68ace5") +
    
    xlab("") +
    ylab("Average yearly spending on healthcare (Billions of USD)") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
```




