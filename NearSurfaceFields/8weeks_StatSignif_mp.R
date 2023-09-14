# M Tsy 2022 Nov
# Stat signif testing: two samples for a number of scores:
# CRPS, spread, ROC area
# 
# Default: AMPT-NOSOIL vs SPPT
# 
# M Tsy 2022б 2023
#----------------------------------------------

source("two_sample_dfr_tests.R")

n_bootstrap = 100000 # btstr sample size

#-------------------------------------------------------------------------------
# CRPS T2m

AMPT= c(1.806498824, 2.148252941, 1.326502941, 1.403334118, 1.50044, 1.183581529, 1.401667059, 1.673847647)
SPPT=c(1.997327059, 2.196547059, 1.348178235, 1.440499412, 1.518195882, 1.179360824, 1.429815882, 1.703330588)

name = "CRPS_T"
CRPS = two_sample_dfr_tests(AMPT, SPPT, n_bootstrap, name)
CRPS$p_Student
CRPS$p_bootstr

mean(AMPT)
mean(SPPT)

#-------------------------------------------------------------------------------
# CRPS V10m
# AMPT-NOSOIL vs SPPT

AMPT= c(
  1.184771188,
  1.44706625,
  1.58637875,
  1.211720688,
  1.347918813,
  1.51604625,
  1.7220275,
  1.35829125
)
SPPT=c(
  1.189833813,
  1.50165125,
  1.627075625,
  1.2372425,
  1.36080175,
  1.52934125,
  1.72624,
  1.40575
)

name = "CRPS_V"
CRPS = two_sample_dfr_tests(AMPT, SPPT, n_bootstrap, name)
CRPS$p_Student
CRPS$p_bootstr

mean(AMPT)
mean(SPPT)


#-------------------------------------------------------------------------------
#  Brier reliability T2m
# AMPT-NOSOIL vs SPPT

AMPT= c(
  0.010168855,
  0.009407756,
  0.003587882,
  0.001294508,
  0.001396718,
  0.000255107,
  0.001233463,
  0.000922584
)
SPPT=c(
  0.015970878,
  0.012345414,
  0.003927564,
  0.001220359,
  0.00162254,
  0.000242823,
  0.00163702,
  0.001358212
)

name = "BrierReliab_T"
BrierR = two_sample_dfr_tests(AMPT, SPPT, n_bootstrap, name)
BrierR$p_Student
BrierR$p_bootstr

mean(AMPT)
mean(SPPT)

#-------------------------------------------------------------------------------
#  RMSE nighttime valid forecasts
# AMPT-SOIL vs SPPT

AMPT= c(
  2.275665,
  2.704184286,
  1.8923825,
  2.01266375,
  1.89454375,
  1.696015,
  2.08306625,
  2.27251625
)
SPPT=c(
  2.57317625,
  2.77049125,
  1.92048875,
  2.03854875,
  1.87418375,
  1.66999,
  2.01813625,
  2.32337125
)

name = "RMSE_T2m_night"
RMSE_N = two_sample_dfr_tests(AMPT, SPPT, n_bootstrap, name)
RMSE_N$p_Student
RMSE_N$p_bootstr

mean(AMPT)
mean(SPPT)


#-------------------------------------------------------------------------------
# spread T2m

AMPT= c(1.061085882,
        0.891739375,
        0.793336529,
        0.825828294,
        0.889668412,
        0.930763824,
        0.899177,
        0.940042438
)
SPPT=c(0.811875294,
       0.687949563,
       0.635158,
       0.670474882,
       0.719014471,
       0.770918706,
       0.692709353,
       0.717519
)

name = "spread"
spread = two_sample_dfr_tests(SPPT, AMPT, n_bootstrap, name) # 1st arg: spread(SPPT) is expected to be lower under H1


#-------------------------------------------------------------------------------
# Brier Score, event T2m_C >0

AMPT= c(0.111322194,
        0.055632594,
        0.05024112,
        0.023145676,
        0.04050375,
        0.01931036,
        0.037782581,
        0.041779431
)
SPPT=c(0.119637444,
       0.059333563,
       0.054322213,
       0.023704451,
       0.039251919,
       0.018479419,
       0.039998438,
       0.0446531
)

plot(AMPT)
lines(SPPT)

name = "BrierScore_T>0"
spread = two_sample_dfr_tests(AMPT, SPPT, n_bootstrap, name) # 1st arg: BS(AMPT) is expected to be lower under H1

#-------------------------------------------------------------------------------
# ROC area, event T2m_C >0

AMPT= c(0.906104,
        0.933776063,
        0.948332267,
        0.975785875,
        0.7442732,
        0.986243875,
        0.971875125,
        0.945622438
)
SPPT=c(0.892162313,
       0.926052,
       0.9369965,
       0.96729125,
       0.700425929,
       0.986436625,
       0.962611875,
       0.937450688
)

plot(AMPT)
lines(SPPT)

name = "ROCA_T>0"
ROCA_T0 = two_sample_dfr_tests(SPPT, AMPT, n_bootstrap, name) # 1st arg: ROCA(SPPT) is expected to be lower under H1

