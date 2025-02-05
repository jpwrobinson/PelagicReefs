
% This from start_IME_toolbox.m was converted to the R code below
% Stats on Chl - now excluding shared (ie statistics on IMEs, not on islands)
has_IME = islands_climato.has_IME==1 & ~isnan(islands_climato.keep_IME);
has_IME_above10pct = has_IME & Chl_increase_nearby>=0.1;
avg_Chl_increase_nearby=mean(Chl_increase_nearby(has_IME),'omitnan')*100
avg_Chl_increase_perIME=mean((islands_climato.Chl_IME(has_IME)-islands_climato.Chl_REF(has_IME))./islands_climato.Chl_REF(has_IME),'omitnan')*100	
Chl_increase=(islands_climato.Chl_IME-islands_climato.Chl_REF).*islands_climato.area_IME*1E6/1E3;	% gChl/m (area_IME is in km²)
Chl_increase(~has_IME)=NaN;
avg_total_Chl_increase_perIME=mean(Chl_increase(:)/1E6,'omitnan')	% tons/m
total_Chl_increase=mean(sum(Chl_increase,1,'omitnan'))/1E6
Chl_increase(~has_IME_above10pct)=NaN;
total_Chl_increase_IMEabove10pct=mean(sum(Chl_increase,1,'omitnan'))/1E6


% In R
# Logical indexing equivalent in R
has_IME <- with(islands_climato, (has_IME == 1) & !is.na(keep_IME))
has_IME_above10pct <- has_IME & (Chl_increase_nearby >= 0.1)

# Compute mean chlorophyll increase nearby (ignoring NaN values)
avg_Chl_increase_nearby <- mean(Chl_increase_nearby[has_IME], na.rm = TRUE) * 100

# Compute average chlorophyll increase per IME
avg_Chl_increase_perIME <- mean(
  (islands_climato$Chl_IME[has_IME] - islands_climato$Chl_REF[has_IME]) / 
    islands_climato$Chl_REF[has_IME], 
  na.rm = TRUE
) * 100

# Compute total chlorophyll increase
Chl_increase <- (islands_climato$Chl_IME - islands_climato$Chl_REF) * 
                islands_climato$area_IME * 1e6 / 1e3  # gChl/m (area_IME is in km²)
Chl_increase[!has_IME] <- NA

# Compute average total chlorophyll increase per IME
avg_total_Chl_increase_perIME <- mean(Chl_increase / 1e6, na.rm = TRUE)  # tons/m

# Compute total chlorophyll increase
total_Chl_increase <- mean(colSums(Chl_increase, na.rm = TRUE)) / 1e6

# Compute chlorophyll increase only for IMEs above 10% threshold
Chl_increase[!has_IME_above10pct] <- NA
total_Chl_increase_IMEabove10pct <- mean(colSums(Chl_increase, na.rm = TRUE)) / 1e6