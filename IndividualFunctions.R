
#--------------------------------------------------------------------------------------------#
## Functions for calculating routine metabolic rates from Barneche et al. 2014 (Equation 5) ##
#--------------------------------------------------------------------------------------------#
## R Morais Apr2018



# Tc = 20oC + 273.15 in K
# normmet is lnb0Tc, which is the log of the size-corrected metabolic rate at Tc = 20oC
# Er is the activation energy in eV
# k is Boltzmann constant in eV / K
# Barneche et al. (2014) find that Ea is not different from 0,
# thus the term Ea * ((1 / (k*T)) - (1 / (k * Tc))) = 0 and is not considered


routmet <- function (mass, temp, Tc = 293.15, alpha = 0.76, normmet = -5.714, Er = 0.589, 
					 k = 8.62e-05, Ei = 2.035, Topt = 306.31) {
	
	BoltzRel <- (Er * ((1/(k*Tc)) - (1/(k*temp))))
	InnactTerm <- log (1 + (Er/(Ei - Er)) * exp (Ei * ((1 / (k * Topt)) - (1 / (k * temp)))))
	
	Bi <- normmet + (alpha * log (mass)) + BoltzRel - InnactTerm
	
	exp (Bi)
	
}

