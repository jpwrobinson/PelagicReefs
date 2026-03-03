# Temporal Chl-a Maximum Model Summary (Island-Specific Smooths)

## Model Structure

- **Response:** `log(Chl_max)` (Gaussian, identity link)
- **n =** 11,489 observations across 34 islands
- **Fitted with:** `mgcv::bam` (fREML)
- **Formula:**
  ```
  log(Chl_max) ~ s(time_s, by = island, k = 12) +
      s(month, bs = "cc", k = 12, by = island)
  ```

---

## Model Fit

| Metric | Factor-smooth (`bs = "fs"`) | Island-specific (`by = island`) |
|---|---|---|
| Adjusted R² | 0.861 | 0.075 |
| Deviance explained | 86.6% | 8.69% |
| Scale estimate (σ²) | 0.026 | 0.111 |
| Intercept | exp(−2.70) ≈ 0.067 mg m⁻³ | exp(−2.70) ≈ 0.067 mg m⁻³ |

The R² collapse (0.86 → 0.075) is even more dramatic than for the enhancement model and has the same explanation — the factor-smooth was absorbing between-island variance as explained variance. Importantly, the **intercept is identical**, confirming the mean chl-a max estimate is robust. The scale estimate increases ~4-fold (0.026 → 0.111), reflecting that residual variance is now higher without the pooled random structure.

As with the enhancement model, the 7.5% R² here is the more honest estimate of how much temporal and seasonal structure exists **within islands** in chl_max once island mean differences are accounted for.

---

## Long-term Trends in Chl_max

Very few islands show significant long-term trends — the pattern closely mirrors the enhancement model:

### Significant trends (p < 0.05)

| Island | edf | F | p-value | Nature |
|---|---|---|---|---|
| Kingman | 6.02 | 2.46 | 0.014 | Complex nonlinear |
| Palmyra | 5.62 | 2.18 | 0.037 | Complex nonlinear |
| Tutuila | 1.00 | 5.45 | 0.020 | Linear |
| Tau | 1.00 | 3.87 | 0.049 | Linear |

### Marginal (p < 0.15)

| Island | edf | p-value |
|---|---|---|
| Jarvis | 3.25 | 0.112 |
| Kure | 1.00 | 0.113 |

All other islands: edf = 1.00, p >> 0.10 — long-term trends in chl_max are essentially absent.

**Kingman and Palmyra stand out clearly** with complex nonlinear trends (edf ~6), consistent across both this model and the MLD anomaly model. These two equatorial central Pacific islands are the only locations showing genuine multi-year structural change in chl-a maxima.

---

## Seasonal Patterns in Chl_max

Seasonality is broadly significant but with notable differences from the factor-smooth version — several islands that appeared seasonal before lose significance once island-level pooling is removed.

### Strong seasonality (p < 0.001, edf > 4)

| Island | edf | F | p-value |
|---|---|---|---|
| Kingman | 5.37 | 11.10 | < 2e-16 |
| Kure | 6.33 | 15.71 | < 2e-16 |
| Midway | 6.24 | 14.07 | < 2e-16 |
| Pearl & Hermes | 5.23 | 7.31 | < 2e-16 |
| Palmyra | 4.66 | 7.22 | < 2e-16 |
| Tutuila | 3.67 | 3.71 | < 2e-16 |
| Kauai | 3.76 | 3.65 | < 2e-16 |
| Nihoa | 3.74 | 3.86 | < 2e-16 |
| Oahu | 3.45 | 2.87 | 1.15e-06 |
| Niihau | 3.40 | 2.77 | 2.01e-06 |
| Maui | 3.52 | 2.88 | 1.55e-06 |
| Farallon de Pajaros | 4.15 | 2.78 | 2.52e-06 |
| Hawaii | 3.15 | 2.06 | 2.20e-05 |
| Maug | 3.79 | 1.97 | 9.08e-05 |
| Necker | 3.37 | 2.13 | 2.14e-05 |
| Laysan | 3.54 | 2.23 | 1.66e-05 |
| French Frigate | 3.31 | 1.95 | 5.10e-05 |
| Asuncion | 3.49 | 1.56 | 5.79e-04 |

### Moderate seasonality (p < 0.05, edf 1.5–3)

| Island | edf | p-value |
|---|---|---|
| Swains | 3.22 | 2.78e-06 |
| Tau | 2.93 | 4.25e-05 |
| Lisianski | 2.96 | 1.40e-03 |
| Johnston | 2.55 | 3.06e-03 |
| Howland | 1.75 | 0.045 |
| Rota | 1.82 | 0.036 |
| Saipan | 1.92 | 0.037 |
| Agrihan | 2.64 | 0.043 |

### No detectable seasonality (p > 0.05)

| Island | Note |
|---|---|
| Alamagan | edf ≈ 0 |
| Guguan | edf ≈ 0 |
| Pagan | edf ≈ 0 |
| Wake | edf ≈ 0 |
| Jarvis | p = 0.12 |
| Baker | p = 0.084 (marginal) |
| Guam | p = 0.078 (marginal) |
| Sarigan | p = 0.108 |

---

## Key Takeaways

- **Long-term trends in chl_max are rare** — only Kingman, Palmyra, Tutuila, and Tau show significant directional change; the absolute chl-a maximum is stable over time at 30/34 islands
- **Kingman and Palmyra are the consistent outliers** across every temporal model — complex nonlinear long-term trends in MLD anomaly, chl_max, and now this model. These islands warrant specific attention in the discussion
- **Several Marianas islands lose seasonality** in this version (Alamagan, Guguan, Pagan — all edf ≈ 0), confirming the earlier finding that their seasonal chl signal was largely a mean-level phenomenon captured by the island mean rather than genuine within-island seasonal variation
- **Main Hawaiian Islands retain seasonality** (Hawaii, Maui, Oahu, Kauai, Niihau all significant) — consistent with real seasonal oceanographic forcing at these islands rather than an artefact of pooling

---

## Cross-Model Comparison: Long-term Trends

| Island | MLD anomaly trend | Chl_max trend | Enhancement trend | Coherent? |
|---|---|---|---|---|
| Kingman | Nonlinear (p<0.001) | Nonlinear (p=0.014) | Marginal (p=0.061) | Yes — strong |
| Palmyra | Nonlinear (p<0.001) | Nonlinear (p=0.037) | ns | Partial |
| Tutuila | ns | Linear (p=0.020) | Linear (p=0.024) | Yes — chl only |
| Tau | ns | Linear (p=0.049) | Marginal (p=0.102) | Weak |
| Wake | ns | ns | Weakly nonlinear (p=0.036) | Enhancement only |
| Midway | Linear MLD (p=0.006) | ns | ns | MLD only |
| Agrihan | Linear MLD (p=0.001) | ns | ns | MLD only |

**Kingman** is the only island with coherent long-term change across MLD, chl_max, and (marginally) enhancement — a strong candidate for a location experiencing genuine multi-decadal oceanographic change with reef-level consequences. **Tutuila** shows parallel trends in both chl metrics without a corresponding MLD signal, suggesting a driver other than mixing (possibly land use change, river discharge, or fishing pressure altering benthic-pelagic coupling).
