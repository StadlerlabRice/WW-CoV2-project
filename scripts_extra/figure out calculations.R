# trying to figure out how quantasoft is calculating the concentrations 
# from the number of negative droplets

# -- relevant for LOD calculations since there is a mismatch in positivity with droplets vs the LOD|concentration stuff


# Infer formula from data ----
# Load recent sheets from complete data and divide the expected formula with the actual data

d1 <- googlesheets4::read_sheet(sheeturls$complete_data, sheet = "112122 Rice WWTP MPX")

d2 <- filter(d1, PositiveDroplets > 0) %>% 
  select(WWTP, Positivity,
         PositiveDroplets, AcceptedDroplets, Copies_per_uL_RNA, Copies_Per_Liter_WW, 
         `Well Position`) %>% 
  
  mutate(expected_copies_ul = 2558.14 * PositiveDroplets/ AcceptedDroplets,
         LOD_ind_cpul = 2558.14 * 3 / AcceptedDroplets,
         LOD_cpul = 2558.14 * 3 * mean(1/d1$AcceptedDroplets),
         expt_ind_pos = expected_copies_ul >= LOD_ind_cpul,
         expt_pos = expected_copies_ul >= LOD_cpul,
         .after = 3)


# Adhoc single well ----

# using data from dd.WW664_101122_Schools2_FLUA+RSV in the raw ddPCR sheet
# row 28 ; C07 RSV

n0 = 20557 # number of negative droplets
N = n0 + 106 # total number of droplets

Cpd = log(N) - log(n0)
Cpd

# the final values -- figure out how these were calculated
CopiesPer20ulwell = 122
Concentration = 6.1

# Getting from Cpd to Copies/20ul well
122/Cpd # this is not the same as N -- its higher by ~300 droplets


Cpd * N

