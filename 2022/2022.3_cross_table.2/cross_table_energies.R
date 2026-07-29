---
title: residentiel_energie 
output: bookdown::word_document2
---
  
```{r setup, include = FALSE}
library(crosstable)
library(flextable)
```

```{r description, echo = FALSE, results = 'asis'}
crosstable(residentiel.2018.énergie,
           where(function(x) is.numeric(x)),
           by = c(nom.territoire),
           funs = c(sum),
           total = "both") %>% 
  as_flextable()
```
