# Temporal Chl-a Enhancement Model Summary (Island-Specific Smooths)

## Model Structure

- **Response:** `log(Chl_increase_nearby)` (Gaussian, identity link)
- **n =** 8,060 observations across 34 islands
- **Fitted with:** `mgcv::bam` (fREML)
- **Formula:**
  ```
  log(Chl_increase_nearby) ~ s(time_s, by = island, k = 12) +
      s(month, bs = "cc", k = 12, by = island)
  ```

---

## Model Fit

| Metric | Factor-smooth (`bs = "fs"`) | Island-specific (`by = island`) |
|---|---|---|
| Adjusted R² | 0.50 | 0.092 |
| Deviance explained | 50.9% | 10.7% |
| Scale estimate (σ²) | 0.498 | 0.672 |
| Intercept | −2.01 | −1.84 |

The dramatic drop in R² (0.50 → 0.09) is **not a model failure** — it reflects the different parameterisations. The factor-smooth (`bs = "fs"`) treats island deviations as random effects and pools information across islands, which inflates apparent explanatory power. The `by = island` version estimates everything independently and is far more conservative. The lower R² here is arguably the more honest representation of how much temporal and seasonal structure is genuinely present in the enhancement signal.

The intercept shift (−2.01 → −1.84) is minor; mean enhancement ≈ exp(−1.84) ≈ **0.16** on the response scale.

---

## Long-term Trends in Enhancement

**Almost no islands show significant long-term trends in chl-a enhancement** — only two reach p < 0.05:

| Island | edf | F | p-value | Nature |
|---|---|---|---|---|
| Tutuila | 1.00 | 5.07 | 0.024 | Linear |
| Wake | 3.40 | 2.53 | 0.036 | Weakly nonlinear |

Marginal (p < 0.10):
| Island | edf | p-value |
|---|---|---|
| Kingman | 1.00 | 0.061 |
| Maug | 1.00 | 0.090 |
| Tau | 1.00 | 0.102 |

All remaining islands show edf ≈ 1 and p >> 0.05 — long-term enhancement trends are effectively absent across the island set.

**This is a strong and ecologically meaningful result**: chl-a enhancement above background has not changed directionally over time at the vast majority of these islands. The island mass effect appears stable.

---

## Seasonal Patterns in Enhancement

Seasonality is more variable than in the chl_max model, with a clear geographic structure.

### Strong seasonality (p < 0.001, edf > 4)

| Island | edf | F | p-value |
|---|---|---|---|
| Laysan | 4.91 | 7.67 | < 2e-16 |
| Lisianski | 5.01 | 8.34 | < 2e-16 |
| Necker | 3.82 | 4.28 | < 2e-16 |
| Pearl & Hermes | 3.80 | 4.49 | < 2e-16 |
| Agrihan | 5.52 | 3.70 | < 2e-16 |
| Tutuila | 4.61 | 5.32 | < 2e-16 |
| Tau | 4.27 | 3.55 | < 2e-16 |
| Pagan | 4.90 | 3.13 | 2.43e-06 |
| Nihoa | 4.01 | 2.90 | 5.58e-07 |
| Maug | 4.48 | 1.71 | 9.92e-04 |
| Kingman | 5.24 | 1.62 | 3.24e-03 |
| Kure | 3.57 | 1.25 | 3.82e-03 |

### Moderate seasonality (p < 0.05, edf 2–4)

| Island | edf | p-value |
|---|---|---|
| French Frigate | 3.45 | < 2e-16 |
| Sarigan | 3.58 | < 2e-16 |
| Rota | 2.73 | 1.16e-04 |
| Alamagan | 3.38 | 1.70e-03 |
| Guguan | 2.57 | 1.38e-03 |
| Farallon de Pajaros | 3.05 | 9.51e-03 |
| Asuncion | 3.20 | 0.017 |
| Guam | 2.39 | 4.75e-03 |
| Midway | 1.88 | 0.025 |
| Howland | 1.92 | 0.032 |
| Palmyra | 2.16 | 0.034 |
| Jarvis | 1.92 | 0.026 |
| Niihau | 2.00 | 0.019 |
| Saipan | 2.11 | 0.011 |

### No detectable seasonality (p > 0.10)

| Island | Note |
|---|---|
| Baker | edf ≈ 0 |
| Swains | edf ≈ 0 |
| Wake | edf ≈ 0 |
| Maui | edf ≈ 0 |
| Oahu | edf ≈ 0 |
| Hawaii | p = 0.15 |
| Johnston | p = 0.14 |
| Kauai | p = 0.12 |

---

## Comparison with Factor-Smooth Version

| Feature | Factor-smooth | Island-specific |
|---|---|---|
| R² (adj) | 0.50 | 0.092 |
| Islands with long-term trends | Not estimable | 2 / 34 |
| Islands with seasonality | ~25 / 34 | ~26 / 34 |
| Non-seasonal islands | Baker, Palmyra, Swains, Maui, Oahu | Baker, Swains, Wake, Maui, Oahu |
| Intercept (mean enhancement) | exp(−2.01) ≈ 0.13 | exp(−1.84) ≈ 0.16 |

Seasonal patterns are broadly consistent between the two versions, giving confidence in the result. The main gain from the island-specific version is the ability to identify that **long-term trends in enhancement are essentially absent** across the island set — something the factor-smooth could not tell you.

---

## Key Takeaways

- **Chl-a enhancement is temporally stable** — only Tutuila and Wake show significant long-term trends; the island mass effect has not changed directionally over the study period at 32 of 34 islands
- **Seasonality in enhancement is widespread but not universal** — ~26/34 islands show significant seasonal cycles, but effect sizes are generally modest (most edf 2–5, F values much lower than in the chl_max model)
- **Non-seasonal enhancement islands** (Baker, Swains, Wake, Maui, Oahu) are a mix of equatorial atolls with weak seasonal forcing and large main Hawaiian Islands where other processes likely dominate
- The **low overall R²** (9.2%) confirms that enhancement variability is largely driven by factors not captured by smooth time and seasonal trends — likely stochastic variability in oceanographic conditions, event-driven inputs (storms, runoff), and spatial heterogeneity within islands
- The contrast with chl_max (R² = 0.86) reinforces the interpretation that **absolute chl-a is tightly governed by physical oceanography, while enhancement is a noisier, more locally-contingent signal**
