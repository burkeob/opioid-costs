use "YOUR FILE PATH HERE/diffusion.dta", clear
set more off
gen log_a_rate = log(a_rate)
gen log_a_deaths = log(a_deaths)

gen log_a_rate_local = log(a_rate_local)
gen log_p_rate_local = log(p_rate_local)
gen log_i_rate_local = log(i_rate_local)
gen log_population = log(population)
encode state_abb, gen(st)
xtset region year

reg F.log_a_rate log_a_rate percent_rural unrate i.year i.st if year >= 2008, vce(cluster region)
reg F.log_a_rate log_a_rate log_a_rate_local percent_rural unrate i.year i.st if year >= 2008, vce(cluster region)
reg F.log_a_rate log_a_rate log_i_rate_local percent_rural unrate i.year i.st if year >= 2008, vce(cluster region)
reg F.log_a_rate log_a_rate log_p_rate_local percent_rural unrate i.year i.st, vce(cluster region)
reg F.log_a_rate log_a_rate log_p_rate_local log_i_rate_local percent_rural unrate i.year i.st if year >= 2008, vce(cluster region)
 
reg F.log_a_deaths log_a_deaths percent_rural unrate log_population i.year i.st if year >= 2008, vce(cluster region)
reg F.log_a_deaths log_a_deaths log_a_rate_local percent_rural unrate log_population i.year i.st if year >= 2008, vce(cluster region)
reg F.log_a_deaths log_a_deaths log_i_rate_local percent_rural unrate log_population i.year i.st if year >= 2008, vce(cluster region)
reg F.log_a_deaths log_a_deaths log_p_rate_local percent_rural unrate log_population i.year i.st, vce(cluster region)
reg F.log_a_deaths log_a_deaths log_p_rate_local log_i_rate_local percent_rural unrate log_population i.year i.st if year >= 2008, vce(cluster region)
log close
