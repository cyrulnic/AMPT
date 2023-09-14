# Plot RMSE and spread T and Wind_speed for AMPT-NOSOIL & SPPT at 5 levs in the free atm-re
# 
# The input data are averaged over the lead times 12,24,36,48h
# 
# The leves are:
# 50  
# 47  
# 39  
# 31 
# 25 
# 
# The input files contain 5*8 tables, whr
# 5 is 5 vert levels
# 8 is 8 weeks
#
# M Tsy Sep 2023
#=====================================================

library(plot3D)

source("two_sample_dfr_tests.R")

# Start with determining levels' heights (in terms of mu, \approx height above ground)
# Here are the FULL levels. Downwards.

full_levels_downw = c(22000.00, 21000.00, 20028.57,
                 19085.36, 18170.00, 17282.14, 
                 16421.43, 15587.50, 14780.00,
                 13998.57, 13242.86, 12512.50,
                 11807.14, 11126.43, 10470.00,
                 9837.50,  9228.57,  8642.86,
                 8080.00,  7539.64,  7021.43,
                 6525.00,  6050.00,  5596.07,
                 5162.86,  4750.00,  4357.14, 
                 3983.93,  3630.00,  3295.00,
                 2978.57,  2680.36,  2400.00,
                 2137.14,  1891.43,  1662.50,
                 1450.00,  1253.57,  1072.86,
                 907.50,   757.14,   621.43,
                 500.00,   392.50,   298.57, 
                 217.86,   150.00,    94.64,
                 51.43,    20.00,     0.00 )  

nlev_full = length(full_levels_downw)

# T an VU we are interested in are on HALF levels

nlev_half = nlev_full -1
half_levels = ( full_levels_downw[1:nlev_half] + full_levels_downw[2:nlev_full] ) /2

nlev_exam=5
levels = c(1:nlev_exam)
levels[1] = half_levels[50]
levels[2] = half_levels[47]
levels[3] = half_levels[39]
levels[4] = half_levels[31]
levels[5] = half_levels[25]

levels
levels = signif(levels,2)
levels
nlev = length(levels)

#-----------------------------------------------------------------
# Next, calc weeks' weights.
# Start days of the 8 weeks:
# 
# 20140201  
# 20140208   
# 20140215  
# 20140222    
# 20140301   
# 20140308   
# 20140316  -- 5 days  
# 20140324
# 
# Total 8*7-2=54 days, the weeks' weights are 

nweeks = 8
week_weights = c(1:nweeks)
week_weights[c(1:6,8)] = 7/54
week_weights[7] = 5/54
sum(week_weights)

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# T
# 
# read the data

# RMSEs

T_rmse_sppt = as.matrix( read.table(file="meanrmse_TM_SPPT_only") ) # [week,lev]
rownames(T_rmse_sppt) = NULL ; colnames(T_rmse_sppt) = NULL

T_rmse_ampt = as.matrix( read.table(file="meanrmse_TM_NOSOIL_only") )
rownames(T_rmse_sppt) = NULL ; colnames(T_rmse_sppt) = NULL

image2D(T_rmse_ampt - T_rmse_sppt)

# ave over weeks

T_rmse_sppt_wkave = apply(T_rmse_sppt, 2, function(t) sum(t*week_weights))
T_rmse_ampt_wkave = apply(T_rmse_ampt, 2, function(t) sum(t*week_weights))


plot(T_rmse_sppt_wkave)
lines(T_rmse_ampt_wkave)
abline(h=0)


# spreads

T_spread_sppt = as.matrix( read.table(file="meanspread_TM_SPPT_only") )
rownames(T_spread_sppt) = NULL ; colnames(T_spread_sppt) = NULL

T_spread_ampt = as.matrix( read.table(file="meanspread_TM_NOSOIL_only") )
rownames(T_spread_ampt) = NULL ; colnames(T_spread_ampt) = NULL

image2D(T_spread_ampt - T_spread_sppt)

plot(apply(T_spread_ampt - T_spread_sppt, 1, mean))
abline(h=0)

# ave over weeks.
# For spread, calc RMS spread, not the mean spread

T_spread2_sppt_wkave = apply(T_spread_sppt, 2, function(t) sqrt( sum(t^2*week_weights) ))
T_spread2_ampt_wkave = apply(T_spread_ampt, 2, function(t) sqrt( sum(t^2*week_weights) ))

greenish = rgb(0,0.7,0)

mx = max(T_rmse_sppt_wkave, T_rmse_ampt_wkave, T_spread2_sppt_wkave, T_spread2_ampt_wkave)

namefile=paste0("FreeAtmT.png")
png(namefile, width=7.48, height=7.48, units = "in", res=300)
par(mgp=c(2.5, 1, 0))
plot(y=levels, x=T_spread2_sppt_wkave, xlim=c(0,mx), col=greenish, 
     main="Free atmosphere. SPPT vs. AMPT",
     xlab="RMSE/spread. Temperature, K",
     ylab="Elevation above ground, m", type = "b", lty = 1, lwd=3, pch=18,
     xaxs="i", yaxs="i",
     cex=2, cex.main=1.4, cex.axis=1.3, cex.lab=1.5
)
lines(y=levels, x=T_spread2_ampt_wkave, col="goldenrod1", type = "b", lty = 1, lwd=3, cex=2, pch=17)
lines(y=levels, x=T_rmse_ampt_wkave, col="goldenrod1", type = "b", lty = 2, lwd=3, cex=2, pch=17)
lines(y=levels, x=T_rmse_sppt_wkave, col=greenish, type = "b", lty = 2, lwd=3, cex=2, pch=18)

leg.txt<-c("Spread SPPT", "Spread AMPT-NOSOIL", "RMSE SPPT", "RMSE AMPT-NOSOIL") 
leg.col<-c( greenish,  "goldenrod1", greenish,  "goldenrod1")
legend("topright", inset=0, leg.txt, col=leg.col, lwd=3, lty=c(1,1,2,2), pch=c(18,17,18,17),
       pt.lwd=2.5, cex=1.4, pt.cex=2, bg="white")
dev.off()
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Wind Speed

# read the data

# RMSEs

W_rmse_sppt = as.matrix( read.table(file="meanrmse_FF_SPPT_only") ) # [week,lev]
rownames(W_rmse_sppt) = NULL ; colnames(W_rmse_sppt) = NULL

W_rmse_ampt = as.matrix( read.table(file="meanrmse_FF_NOSOIL_only") )
rownames(W_rmse_sppt) = NULL ; colnames(W_rmse_sppt) = NULL

image2D(W_rmse_ampt - W_rmse_sppt)

# ave over weeks

W_rmse_sppt_wkave = apply(W_rmse_sppt, 2, function(t) sum(t*week_weights))
W_rmse_ampt_wkave = apply(W_rmse_ampt, 2, function(t) sum(t*week_weights))


plot(W_rmse_sppt_wkave)
lines(W_rmse_ampt_wkave)
abline(h=0)


# spreads

W_spread_sppt = as.matrix( read.table(file="meanspread_FFM_SPPT_only") )
rownames(W_spread_sppt) = NULL ; colnames(W_spread_sppt) = NULL

W_spread_ampt = as.matrix( read.table(file="meanspread_FFM_NOSOIL_only") )
rownames(W_spread_ampt) = NULL ; colnames(W_spread_ampt) = NULL

image2D(W_spread_ampt - W_spread_sppt)

plot(apply(W_spread_ampt - W_spread_sppt, 1, mean))
abline(h=0)

# ave over weeks.
# For spread, calc RMS spread, not the mean spread

W_spread2_sppt_wkave = apply(W_spread_sppt, 2, function(t) sqrt( sum(t^2*week_weights) ))
W_spread2_ampt_wkave = apply(W_spread_ampt, 2, function(t) sqrt( sum(t^2*week_weights) ))

greenish = rgb(0,0.7,0)

mx = max(W_rmse_sppt_wkave, W_rmse_ampt_wkave, W_spread2_sppt_wkave, W_spread2_ampt_wkave)

namefile=paste0("FreeAtmW.png")
png(namefile, width=7.48, height=7.48, units = "in", res=300)
par(mgp=c(2.5, 1, 0))
plot(y=levels, x=W_spread2_sppt_wkave, xlim=c(0,mx), col=greenish, 
     main="Free atmosphere. SPPT vs. AMPT",
     xlab="RMSE/spread. Wind speed, m/s",
     ylab="Elevation above ground, m", type = "b", lty = 1, lwd=3, pch=18,
     xaxs="i", yaxs="i",
     cex=2, cex.main=1.4, cex.axis=1.3, cex.lab=1.5
     )
lines(y=levels, x=W_spread2_ampt_wkave, col="goldenrod1", type = "b", lty = 1, lwd=3, cex=2, pch=17)
lines(y=levels, x=W_rmse_ampt_wkave, col="goldenrod1", type = "b", lty = 2, cex=2, lwd=3, pch=17)
lines(y=levels, x=W_rmse_sppt_wkave, col=greenish, type = "b", lty = 2, lwd=3, cex=2, pch=18)

leg.txt<-c("Spread SPPT", "Spread AMPT-NOSOIL", "RMSE SPPT", "RMSE AMPT-NOSOIL") 
leg.col<-c( greenish,  "goldenrod1", greenish,  "goldenrod1")
legend("topleft", inset=0, leg.txt, col=leg.col, lwd=3, lty=c(1,1,2,2), pch=c(18,17,18,17), 
       pt.lwd=2.5, cex=1.4, pt.cex=2, bg="white")
dev.off()

# AMPT's Spread is VERY significantly greater that the SPPT's spread at all levs!
# stop("OK")

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Stat signif testing. SPREAD only.

n_bootstrap = 100000 # btstr sample size

nnames = c("Level 50", "Level 47", "Level 39", "Level 31", "Level 25")
sstudent = c(1:nlev)
bbootstr = c(1:nlev)

# T
message("")
message("SIGNIFICANCE TESTING")
message("")
message("Temperature")

for(lev in 1:nlev){
  SPPT=T_spread_sppt[,lev]
  AMPT=T_spread_ampt[,lev]
  name=nnames[lev]
  message("")
  
  
  SIGNIF = two_sample_dfr_tests(SPPT, AMPT, n_bootstrap, name)
  sstudent[lev] = SIGNIF$p_Student
  bbootstr[lev] = SIGNIF$p_bootstr
  
  # message(name)
  # mean(AMPT)
  # mean(SPPT)
}

mn=min(sstudent, bbootstr)
mx=max(sstudent, bbootstr)
plot(sstudent, xlab="Level number", ylim=c(mn,mx))
lines(bbootstr)
sstudent
bbootstr


# Wind speed

message("") ;   message("")
message("Wind speed")

for(lev in 1:nlev){
  SPPT=W_spread_sppt[,lev]
  AMPT=W_spread_ampt[,lev]
  name=nnames[lev]
  message("")
  
  SIGNIF = two_sample_dfr_tests(SPPT, AMPT, n_bootstrap, name)
  sstudent[lev] = SIGNIF$p_Student
  bbootstr[lev] = SIGNIF$p_bootstr
  
  # message(name)
  # mean(AMPT)
  # mean(SPPT)
}

mn=min(sstudent, bbootstr)
mx=max(sstudent, bbootstr)
plot(sstudent, xlab="Level number", ylim=c(mn,mx))
lines(bbootstr)
sstudent
bbootstr

