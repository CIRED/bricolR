### Analyses biovallée avec crosstable et tidyverse
library(readxl)
rm(racc.enr)
racc.enr <- raccordements_drome
glimpse(racc.enr)
summary(racc.enr)

crosstable(racc.enr, c(Type.Production, Stockage.Existence), by = Type.Injection) %>% as_flextable(keep_id = FALSE)
