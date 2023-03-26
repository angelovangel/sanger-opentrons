library(dplyr)
library(stringr)

wells_colwise <- lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist()
tuberack_wells <- lapply(1:6, function(x) {str_c(LETTERS[1:4], x)}) %>% unlist()

bcl_primers <- tibble(
  primer_well = tuberack_wells[1:22],
  primer_name = c(
 "M13rev(-29)",
  "M13rev(-49)",
  "M13uni(-21)",
  "M13uni(-43)",
  "M13-pUC-F",
  "M13-pUC-R",
  "pEGFPC1-F",
  "peGFPC1-R",
  "pEGFPN1-F",
  "pEGFPN1-R",
  "pGEX-F",
  "pGEX-R",
  "T3",
  "T7-F",
  "T7-Term",
  "pENTR-F",
  "ElasFwd",
  "ElasRv",
  "EuVH",
  "water",
  "27F",
  "1492R")
)
