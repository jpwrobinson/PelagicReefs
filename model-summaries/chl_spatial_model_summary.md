# Spatial Chl-a Model Summaries

## Overview

Both are multivariate Bayesian models (`MV`) fit with NUTS sampling in brms, sharing a submodel for `ted_mean` (tidally-driven energy, treated as partially missing via `mi()`) predicted by reef area. Convergence is clean across both — all Rhats ≈ 1.00 and ESS values are generally healthy.

---

## Model 1 — Chl-a Enhancement (`m_chl_inc`)

- **Response:** `Chl_increase_nearby` (lognormal)
- **n =** 376 observations, 34 islands
- **Formula:** `Chl_increase_nearby ~ bathymetric_slope + geomorphic_type * reef_area_km2 + land_area_km2 + avg_monthly_mm + mean_chlorophyll + mld + mi(ted_mean) + (1 + mld | island)`

### Key Predictors

| Predictor | Estimate | 95% CI | Interpretation |
|---|---|---|---|
| Reef area | 0.90 | 0.47–1.34 | Strong positive effect; larger reefs drive greater enhancement |
| MLD | −0.27 | −0.35–−0.19 | Deeper mixed layers suppress enhancement |
| Rainfall (`avg_monthly_mm`) | 0.18 | 0.10–0.26 | Likely terrestrial nutrient input |
| Land area | 0.30 | 0.12–0.47 | Positive, probably correlated with runoff |
| Mean background chl-a | 1.05 | −0.64–2.66 | Uncertain — wide CI |
| Geomorphic type × reef area | −0.14 | −0.52–0.25 | Negligible interaction |

### Random Effects (~island)

- Intercept SD = 0.29; MLD slope SD = 0.16
- Intercept–MLD correlation = −0.36: islands with higher baseline enhancement tend to show weaker MLD sensitivity

---

## Model 2 — Maximum Chl-a (`m_chl_max`)

- **Response:** `Chl_max` (lognormal)
- **n =** 408 observations, 34 islands
- **Formula:** `Chl_max ~ bathymetric_slope + geomorphic_type * reef_area_km2 + land_area_km2 + avg_monthly_mm + mld + mi(ted_mean) + (1 + mld | island)`

### Key Predictors

| Predictor | Estimate | 95% CI | Interpretation |
|---|---|---|---|
| MLD | 0.11 | 0.08–0.15 | Positive — deeper mixing brings up nutrients, driving higher peaks |
| Geomorphic type (Island) | 0.42 | −0.05–0.88 | Suggestive but uncertain |
| Reef area | 0.25 | −0.19–0.69 | CI overlaps zero |
| Land area | −0.15 | −0.34–0.05 | CI overlaps zero |
| Rainfall | 0.01 | −0.01–0.03 | No clear effect |

### Random Effects (~island)

- Intercept SD = 0.50; MLD slope SD = 0.07
- Intercept–MLD correlation = +0.71 (CI: 0.25–0.95): islands with higher baseline chl-a maxima also show stronger MLD-driven peaks

---

## Shared ted_mean Submodel

Consistent across both models: reef area negatively predicts TED (β ≈ −0.57, CI: −0.65–−0.48), likely a geomorphic scaling relationship.

---

## Key Contrast

The two responses capture different processes:

- **Enhancement** (relative to background) is driven by reef and island characteristics — size, runoff, shallow mixing suppression
- **Absolute maximum** is more strongly tied to oceanographic forcing (MLD), with island traits mattering less

This suggests that while island geomorphology shapes how much chl-a is elevated above background, the ceiling of chl-a concentrations is set more by physical oceanography.
