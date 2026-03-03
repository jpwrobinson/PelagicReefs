# Temporal MLD Anomaly Model Summary (Island-Specific Smooths)

## Model Structure

- **Response:** `anomaly` (MLD deviation from island mean, seasonal signal removed; Gaussian, identity link)
- **n =** 13,680 observations across 40 islands
- **Fitted with:** `mgcv::bam` (fREML)
- **Formula:**
  ```
  anomaly ~ s(time_num, by = island, k = 12)
  ```

> Seasonal terms excluded by design — this model isolates **inter-annual and longer-term** variability in MLD after removing the intra-annual seasonal signal.

---

## Model Fit

| Metric | Value |
|---|---|
| Intercept | ~0 (p = 0.96, correct for anomaly) |
| Adjusted R² | 0.034 |
| Deviance explained | 3.8% |
| Scale estimate (σ²) | 47.1 |

The low R² is **expected and interpretable**: once seasonality is removed, long-term smooth trends account for only ~3.8% of MLD anomaly variance. The bulk of inter-annual MLD variability is stochastic (year-to-year noise from ENSO, trade wind variability, etc.) rather than driven by a smooth directional trend. Results are consistent with the factor-smooth version (R² = 0.032), confirming robustness to model structure.

---

## Long-term MLD Anomaly Trends by Island

### Significant trends (p < 0.05)

| Island | edf | F | p-value | Nature |
|---|---|---|---|---|
| Kingman | 3.39 | 6.38 | 3.31e-05 | Nonlinear |
| Palmyra | 3.59 | 7.14 | 6.03e-06 | Nonlinear |
| Agrihan | 1.00 | 10.32 | 0.00132 | Linear |
| Alamagan | 1.52 | 6.15 | 0.00680 | Near-linear |
| Guguan | 1.64 | 4.91 | 0.00697 | Near-linear |
| Kure | 3.26 | 4.50 | 0.00115 | Nonlinear |
| Pagan | 1.45 | 7.45 | 0.00351 | Near-linear |
| Midway | 1.00 | 7.67 | 0.00561 | Linear |
| Laysan | 1.00 | 5.11 | 0.02380 | Linear |
| Rose | 1.00 | 5.69 | 0.01710 | Linear |
| Aguijan | 2.04 | 3.85 | 0.01236 | Weakly nonlinear |
| Saipan | 2.12 | 3.32 | 0.02223 | Weakly nonlinear |
| Sarigan | 1.91 | 3.64 | 0.02012 | Near-linear |
| Swains | 2.07 | 3.92 | 0.01271 | Weakly nonlinear |
| Tinian | 2.08 | 3.52 | 0.01752 | Weakly nonlinear |
| Wake | 1.45 | 4.40 | 0.03362 | Near-linear |

### Marginal trends (0.05 < p < 0.10)

| Island | edf | p-value |
|---|---|---|
| Johnston | 1.00 | 0.095 |
| Necker | 1.00 | 0.059 |
| Rota | 1.84 | 0.054 |

### No detectable long-term trend (p > 0.10)

Baker, Farallon de Pajaros, French Frigate, Guam, Hawaii, Howland, Jarvis, Kauai, Lanai, Lisianski, Maug, Maui, Molokai, Nihoa, Niihau, Oahu, Ofu & Olosega, Tau, Tutuila, Asuncion

---

## Geographic Patterns

**Equatorial central Pacific (Kingman, Palmyra)** show the most complex nonlinear MLD anomaly trends (edf ~3.5), consistent with strong ENSO sensitivity in the ITCZ/equatorial counter-current region.

**Northern Marianas (Agrihan, Alamagan, Guguan, Pagan, Sarigan, Aguijan, Saipan, Tinian)** show predominantly near-linear trends (edf 1–2), suggesting a more directional shift in MLD over time — potentially linked to long-term changes in western Pacific trade wind strength or thermocline depth.

**Northwestern Hawaiian Islands (Kure, Midway, Laysan)** show significant trends, while the **main Hawaiian Islands (Hawaii, Maui, Oahu, Kauai, Molokai, Lanai, Niihau)** show none — a consistent contrast across models between the dynamic NWHI and the more stable subtropical MHI.

**Equatorial Line/Phoenix Islands (Baker, Howland, Jarvis)** show no significant long-term MLD anomaly trends, despite their ENSO-sensitive position, suggesting their MLD variability is captured by the seasonal cycle rather than inter-annual drift.

---

## Key Takeaways

- **Inter-annual MLD trends are real but explain little variance** (~3.8%) — seasonal forcing overwhelmingly dominates MLD variability
- **~16 of 40 islands** show significant long-term MLD anomaly trends, concentrated in the Marianas and central equatorial Pacific
- **Linear trends dominate** (edf = 1 for Agrihan, Midway, Laysan, Rose) — where long-term change is occurring it tends to be directional rather than oscillatory
- **Kingman and Palmyra are exceptions** — nonlinear edf ~3.5 trends likely reflect ENSO-driven multi-year cycles superimposed on a longer-term shift
- The disconnect between significant island-level trends and the overall low R² confirms that **most MLD anomaly variance is unpredictable noise** at the timescales modelled

---

## Model Comparison: MLD Anomaly

| Feature | Factor-smooth (`bs = "fs"`) | Island-specific (`by = island`) |
|---|---|---|
| R² (adj) | 0.032 | 0.034 |
| Deviance explained | 3.6% | 3.8% |
| Island-level inference | No | Yes |
| Islands with significant trends | Not estimable | ~16 / 40 |

Results are consistent across formulations. The `by = island` version is preferred for island-level inference.
