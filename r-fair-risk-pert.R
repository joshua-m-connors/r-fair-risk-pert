library(mc2d)
library(ggplot2)

# Threat Event Frequency (Min, Likely, Max)
tef_min <- 6
tef_likely <- 12
tef_max <- 24
tef_conf <- 3
### If using TCap and RS comment out Vuln ranges just below them
# Threat Capability (Min, Likely, Max)
tcap_min <- 0.01
tcap_likely <- 0.5
tcap_max <- 0.95
tcap_conf <- 3
# Resistence Strength (Min, Likely, Max)
rs_min <- 0.40
rs_likely <- 0.6
rs_max <- 0.90
rs_conf <- 3
# Vulnerability (Min, Likely, Max)
#vuln_min <- 0.009
#vuln_likely <- 0.01
#vuln_max <- 0.02
#vuln_conf <- 3
# Annual Rate of Occurrence (Min, Likely, Max)
#aro_min <- 12
#aro_likely <- 26
#aro_max <- 75
#aro_conf <- 4
# Primary Single Loss Expectancy (Min, Likely, Max)
psle_min <- 1000
psle_likely <- 1500
psle_max <- 2500
psle_conf <- 2
# Secondary Loss Annual Rate of Occurrence (Min, Likely, Max)
saro_min <- 0.001
saro_likely <- 0.01
saro_max <- 0.1
saro_conf <- 4
# Secondary Single Loss Expectancy (Min, Likely, Max)
ssle_min <- 100000
ssle_likely <- 250000
ssle_max <- 1000000
ssle_conf <- 2
# How Many Simulations?
n <- 10000

set.seed(88881111)
# Calculate Annual Rate of Occurrence
#aro <- rpert(n, aro_min, aro_likely, aro_max, shape = aro_conf)
tef <- rpert(n, tef_min, tef_likely, tef_max, shape = tef_conf)
tcap <- rpert(n, tcap_min, tcap_likely, tcap_max, shape = tcap_conf)
rs <- rpert(n, rs_min, rs_likely, rs_max, shape = rs_conf)
vuln_temp1 <- tcap - rs
vuln_temp <- pmax(vuln_temp1,0)
### If using TCap and RS instead of Vuln uncomment the four rows above this line
### and comment out the line just below this line
#vuln_temp <- rpert(n, vuln_min, vuln_likely, vuln_max, shape = vuln_conf)
vuln <- mean(vuln_temp)
aro <- tef * vuln
# Calculate Single Loss Expectancy
#sle <- rpert(n, sle_min, sle_likely, sle_max, shape = sle_conf)
psle <- rpert(n, psle_min, psle_likely, psle_max, shape = psle_conf)
saro <- rpert(n, saro_min, saro_likely, saro_max, shape = saro_conf)
ssle <- rpert(n, ssle_min, ssle_likely, ssle_max, shape = ssle_conf)
sale <- saro * ssle
sle <- psle + sale

# Annualized Loss = aro * sle
#ale <- aro * psle
ale <- aro * sle

# That's it! Now let's plot it. Need 'scales' to show commas on x-axis.
library(scales)
gg <- ggplot(data.frame(ale), aes(x = ale))
gg <- gg + geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 10000)
gg <- gg + geom_density(fill = "steelblue", alpha = 1/3)
gg <- gg + scale_x_continuous(labels = comma)
gg <- gg + theme_bw()
print(gg)

summary(tef)
summary(vuln)
summary(aro)
summary(psle)
summary(saro)
summary(ssle)
summary(sle)
summary(ale)
dollar(quantile(ale, probs = c(0.1, 0.25, 0.5, 0.75, 0.85, 0.95, 0.99, 0.9999)))
dollar(quantile(ale, probs = c(0.95)))