library(readr)
library(tidyverse)
library(ggiraph)

# read tract data
tracts <- read_csv("data_raw/census_tracts.csv", skip=1) |>
  rename(fips=Geo_FIPS, state=Geo_STATE, county=Geo_COUNTY,
         pop_white=SE_B04001_003, pop_black=SE_B04001_004,
         pop_asian=SE_B04001_006, pop_latino=SE_B04001_010,
         labforce_white=SE_A17006A_001, unemp_white=SE_A17006A_003,
         labforce_black=SE_A17006B_001, unemp_black=SE_A17006B_003,
         labforce_asian=SE_A17006D_001, unemp_asian=SE_A17006D_003,
         labforce_latino=SE_A17006H_001, unemp_latino=SE_A17006H_003,
         pov_denom_white=SE_A13001A_001, pov_white=SE_A13001A_002,
         pov_denom_black=SE_A13001B_001, pov_black=SE_A13001B_002,
         pov_denom_asian=SE_A13001D_001, pov_asian=SE_A13001D_002,
         pov_denom_latino=SE_A13001H_001, pov_latino=SE_A13001H_002) |>
  mutate(county=state*1000+county) |>
  select(fips, state, county,
         pop_white, pop_black, pop_asian, pop_latino,
         labforce_white, labforce_black, labforce_asian, labforce_latino,
         unemp_white, unemp_black, unemp_asian, unemp_latino,
         pov_denom_white, pov_denom_black, pov_denom_asian, pov_denom_latino,
         pov_white, pov_black, pov_asian, pov_latino)

crosswalk <- read_csv("data_raw/crosswalk.csv") |>
  rename(county=`County Code`, msa=`MSA Code`, msa_name=`MSA Title`) |>
  select(county, msa, msa_name) |>
  filter(!str_detect(msa_name, "MicroSA") & !str_detect(msa_name, ", PR"))

tracts <- tracts |>
  left_join(crosswalk) |>
  filter(!is.na(msa))

msa_list <- tracts |>
  group_by(msa) |>
  group_split()

msa <- lapply(msa_list, function(x) {
  dissim_wb <- 50*sum(abs(prop.table(x$pop_white)-prop.table(x$pop_black)))
  dissim_wa <- 50*sum(abs(prop.table(x$pop_white)-prop.table(x$pop_asian)))
  dissim_wl <- 50*sum(abs(prop.table(x$pop_white)-prop.table(x$pop_latino)))

  pov_rate_white <- sum(x$pov_white)/sum(x$pov_denom_white)
  pov_rate_black <- sum(x$pov_black)/sum(x$pov_denom_black)
  pov_rate_asian <- sum(x$pov_asian)/sum(x$pov_denom_asian)
  pov_rate_latino <- sum(x$pov_latino)/sum(x$pov_denom_latino)

  pov_ratio_wb <- pov_rate_black/pov_rate_white
  pov_ratio_wa <- pov_rate_asian/pov_rate_white
  pov_ratio_wl <- pov_rate_latino/pov_rate_white

  unemp_rate_white <- sum(x$unemp_white)/sum(x$labforce_white)
  unemp_rate_black <- sum(x$unemp_black)/sum(x$labforce_black)
  unemp_rate_asian <- sum(x$unemp_asian)/sum(x$labforce_asian)
  unemp_rate_latino <- sum(x$unemp_latino)/sum(x$labforce_latino)

  unemp_ratio_wb <- unemp_rate_black/unemp_rate_white
  unemp_ratio_wa <- unemp_rate_asian/unemp_rate_white
  unemp_ratio_wl <- unemp_rate_latino/unemp_rate_white

  return(tibble(msa=x$msa[1], msa_name=x$msa_name[1],
                dissim_wb, dissim_wa, dissim_wl,
                pov_ratio_wb, pov_ratio_wa, pov_ratio_wl,
                unemp_ratio_wb, unemp_ratio_wa, unemp_ratio_wl,
                pop_white=sum(x$pop_white),
                pop_black=sum(x$pop_black),
                pop_asian=sum(x$pop_asian),
                pop_latino=sum(x$pop_latino)))
}) |> bind_rows()


msa <- msa |>
  filter(pop_black>3000 & pop_asian>3000 & pop_latino>3000) |>
  select(msa, msa_name, dissim_wb, dissim_wa, dissim_wl,
         pov_ratio_wb, pov_ratio_wa, pov_ratio_wl,
         unemp_ratio_wb, unemp_ratio_wa, unemp_ratio_wl)

save(msa, file="data_constructed/full_msa.RData")
