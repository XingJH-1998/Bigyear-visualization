#XXL data debug
# Debug: auk::ebird_taxonomy out of date. And many species cannot be matched.
# Species Count >0 otherwise 
library(dplyr)
library(data.table)

ebird_taxonomy <- read.csv("ebird_taxonomy.csv")
xxl.data.raw <- read.csv("/Users/tangerine/Documents/Big_year/xxl_data.csv")
xxl.data.raw.merge <- xxl.data.raw %>% 
  filter(Count>0) %>%
  mutate(Year=year(Date)) %>% 
  filter(Year==2025) %>%
  select(Common.Name, Scientific.Name, State.Province, Location, Date) %>%
  left_join(ebird_taxonomy, by = c("Scientific.Name" = "SCI_NAME"))
xxl.data <- xxl.data.raw %>% 
  filter(Count>0) %>%
  mutate(Year=year(Date)) %>% 
  filter(Year==2025) %>%
  select(Common.Name, Scientific.Name, State.Province, Location, Date) %>%
  left_join(ebird_taxonomy, by = c("Scientific.Name" = "SCI_NAME")) %>%
  filter(CATEGORY %in% c("domestic", "issf", "species"))
#Domestic type will lead to error?

tax_replace <- ebird_taxonomy %>%
  select(
    SPECIES_CODE,
    TAXON_ORDER,
    CATEGORY,
    TAXON_CONCEPT_ID,
    PRIMARY_COM_NAME,
    SCI_NAME,
    ORDER,
    FAMILY,
    SPECIES_GROUP,
    REPORT_AS
  )

xxl.data2 <- xxl.data %>%
  left_join(
    tax_replace,
    by = c("REPORT_AS" = "SPECIES_CODE"),
    suffix = c("", ".rep")
  )

xxl.data2 <- xxl.data2 %>%
  mutate(
    Scientific.Name   = coalesce(SCI_NAME, Scientific.Name),
    PRIMARY_COM_NAME  = coalesce(PRIMARY_COM_NAME.rep, PRIMARY_COM_NAME),
    ORDER             = coalesce(ORDER.rep, ORDER),
    FAMILY            = coalesce(FAMILY.rep, FAMILY),
    TAXON_ORDER       = coalesce(TAXON_ORDER.rep, TAXON_ORDER),
    CATEGORY          = coalesce(CATEGORY.rep, CATEGORY),
    TAXON_CONCEPT_ID  = coalesce(TAXON_CONCEPT_ID.rep, TAXON_CONCEPT_ID),
    SPECIES_GROUP     = coalesce(SPECIES_GROUP.rep, SPECIES_GROUP),
    REPORT_AS         = coalesce(REPORT_AS.rep, REPORT_AS)
  ) %>%
  select(-ends_with(".rep"))

length(unique(xxl.data2$Scientific.Name))
raw.data.list <- unique(xxl.data2$Scientific.Name)


xxl.2025.list <- read.csv("/Users/tangerine/Documents/Big_year/ebird_world_year_2025_list.csv")
table(xxl.2025.list$Category)
#Should be 1977 species
xxl.2025.list.count <- xxl.2025.list %>% filter(Countable==1)
xxl.2025.list.uncount <- xxl.2025.list %>% filter(Countable!=1)
webpage.list <- unique(xxl.2025.list.count$Scientific.Name)

setdiff( webpage.list, raw.data.list) # Webpage species All included!
setdiff(raw.data.list, webpage.list) # 7 more species
# 2 count 0, 5 escapee




