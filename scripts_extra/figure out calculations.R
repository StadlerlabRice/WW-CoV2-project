# trying to figure out how quantasoft is calculating the concentrations 
# from the number of negative droplets

# -- relevant for LOD calculations since there is a mismatch in positivity with droplets vs the LOD|concentration stuff


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

