# Temporal Chl-a Maximum Anomaly Model Summary (Island-Specific Smooths)

## Model Structure

- **Response:** `chl_max_anom` (deviation from long-term island mean; Gaussian, identity link)
- **n =** 11,489 observations across 34 islands
- **Fitted with:** `mgcv::bam` (fREML)
- **Formula:**
  ```
  chl_max_anom ~ s(time_s, by = island, k = 12) +
      s(month, bs = "cc", k = 12, by = island)
  ```

---

## Model Fit

| Metric | Factor-smooth (`bs = "fs"`) | Island-specific (`by = island`) |
|---|---|---|
| Adjusted R² | 0.353 | 0.378 |
| Deviance explained | 37.2% | 39.2% |
| Scale estimate (σ²) | 0.000242 | 0.000236 |
| Intercept | ~0 (p = 0.932) | ~0 (p = 0.938) |

Notably, this is the **only model where R² increases** when switching from factor-smooth to island-specific smooths (0.353 → 0.378). This makes sense — the anomaly response has no between-island mean differences to absorb, so the island-specific version gains flexibility without losing the pooling benefit that was inflating R² in the other models. The fit is genuinely better here.

---

## Long-term Trends in Chl_max Anomaly

This model reveals a striking geographic pattern — a small cluster of islands drives almost all long-term anomaly structure.

### Significant trends (p < 0.05)

| Island | edf | F | p-value | Nature |
|---|---|---|---|---|
| Baker | 10.54 | 19.56 | < 2e-16 | Highly complex |
| Jarvis | 10.54 | 20.04 | < 2e-16 | Highly complex |
| Howland | 10.44 | 16.03 | < 2e-16 | Highly complex |
| Palmyra | 9.98 | 12.83 | < 2e-16 | Highly complex |
| Kingman | 9.77 | 12.15 | < 2e-16 | Highly complex |
| Kure | 9.68 | 5.93 | < 2e-16 | Highly complex |
| Midway | 8.66 | 3.97 | 2.59e-05 | Complex |
| Pearl & Hermes | 1.00 | 10.09 | 0.001 | Linear |
| Tutuila | 3.77 | 3.29 | 0.007 | Moderately complex |
| Tau | 1.00 | 5.22 | 0.022 | Linear |

### Marginal (p < 0.10)

| Island | edf | p-value |
|---|---|---|
| Lisianski | 1.00 | 0.051 |

All other islands: edf ≈ 1, p >> 0.10 — no detectable long-term anomaly trend.

---

### The equatorial cluster

The three equatorial Line/Phoenix Islands — **Baker, Jarvis, Howland** — all show near-maximal edf (~10.5 out of k=12), indicating highly complex, essentially unconstrained temporal trajectories in their chl_max anomalies. Combined with Kingman and Palmyra (edf ~10), this equatorial central Pacific cluster is collectively showing the most complex and significant long-term chl-a variability in the dataset. These islands sit in or near the Pacific Equatorial Divergence and ITCZ, where ENSO drives large interannual swings in thermocline depth, upwelling, and productivity — complex nonlinear temporal structure here is physically expected.

**Kure and Midway** (NWHI) show complex trends (edf 8.7–9.7) but with lower F-statistics than the equatorial islands, suggesting meaningful but less dramatic long-term anomaly variation at the northern end of the Hawaiian chain.

**Pearl & Hermes and Tau** show simple linear trends (edf = 1.00) — directional change over time rather than oscillatory or ENSO-driven variability.

---

## Seasonal Patterns in Chl_max Anomaly

Broadly consistent with the factor-smooth version. Seasonality is widespread and strong.

### Strong seasonality (p < 0.001, edf > 6)

| Island | edf | F | p-value |
|---|---|---|---|
| Kure | 8.80 | 64.78 | < 2e-16 |
| Midway | 8.71 | 56.16 | < 2e-16 |
| Kingman | 7.98 | 55.90 | < 2e-16 |
| Palmyra | 7.62 | 40.34 | < 2e-16 |
| Pearl & Hermes | 7.84 | 31.54 | < 2e-16 |
| Jarvis | 7.22 | 13.09 | < 2e-16 |
| Howland | 7.83 | 6.94 | < 2e-16 |
| Baker | 7.68 | 5.34 | < 2e-16 |
| Kauai | 5.90 | 13.05 | < 2e-16 |
| Maui | 6.22 | 13.37 | < 2e-16 |
| Oahu | 5.61 | 10.66 | < 2e-16 |
| Nihoa | 5.40 | 10.97 | < 2e-16 |
| Niihau | 5.07 | 8.35 | < 2e-16 |
| Necker | 5.52 | 7.81 | < 2e-16 |
| Hawaii | 5.47 | 7.12 | < 2e-16 |
| French Frigate | 5.69 | 7.09 | < 2e-16 |
| Tutuila | 4.68 | 7.51 | < 2e-16 |
| Laysan | 5.93 | 7.56 | < 2e-16 |
| Lisianski | 5.60 | 6.09 | < 2e-16 |

### Moderate seasonality (p < 0.05, edf 2–5)

| Island | edf | p-value |
|---|---|---|
| Swains | 3.59 | < 2e-16 |
| Tau | 3.17 | 3.46e-06 |
| Farallon de Pajaros | 4.60 | 1.26e-06 |
| Maug | 4.24 | 2.08e-05 |
| Asuncion | 3.89 | 2.36e-04 |
| Johnston | 2.98 | 6.12e-04 |
| Agrihan | 3.42 | 0.019 |
| Saipan | 2.01 | 0.028 |
| Pagan | 3.73 | 0.047 |

### No detectable seasonality (p > 0.05)

| Island | Note |
|---|---|
| Alamagan | edf ≈ 0 |
| Guguan | edf ≈ 0 |
| Wake | edf ≈ 0 |
| Guam | p = 0.087 (marginal) |
| Rota | p = 0.086 (marginal) |
| Sarigan | p = 0.052 (marginal) |

Consistent with earlier versions — Alamagan, Guguan, and Wake persistently show no residual seasonal anomaly structure across all model formulations.

---

## Cross-Model Comparison: Long-term Trends (Island-Specific Versions)

| Island | MLD anomaly | Chl_max | Chl_max_anom | Enhancement | Pattern |
|---|---|---|---|---|---|
| Kingman | Nonlinear *** | Nonlinear * | Highly complex *** | Marginal | **Full coherence** |
| Palmyra | Nonlinear *** | Nonlinear * | Highly complex *** | ns | **Chl + MLD** |
| Baker | ns | ns | Highly complex *** | ns | **Anomaly only** |
| Jarvis | ns (p=0.006 in MLD model) | ns | Highly complex *** | ns | **Anomaly only** |
| Howland | * (MLD model) | ns | Highly complex *** | ns | **Anomaly only** |
| Kure | Nonlinear ** | ns | Complex *** | ns | **MLD + anomaly** |
| Midway | Linear ** | ns | Complex *** | ns | **MLD + anomaly** |
| Pearl & Hermes | * | ns | Linear ** | ns | **MLD + anomaly** |
| Tutuila | ns | Linear * | Moderate ** | Linear * | **Chl + enhancement** |
| Tau | ns | Linear * | Linear * | Marginal | **Chl only** |

---

## Key Takeaways

- **Equatorial central Pacific is the hotspot of long-term chl-a anomaly change** — Baker, Jarvis, Howland, Kingman, Palmyra all show highly complex temporal trends (edf ~10), almost certainly driven by ENSO-mediated variability in equatorial upwelling and thermocline dynamics
- **Baker, Jarvis, and Howland are anomaly-only signals** — they show complex long-term anomaly trends without corresponding signals in chl_max or enhancement, suggesting their total chl-a is stable but *which months deviate from the mean* varies strongly over time — i.e. the timing and magnitude of productivity events is shifting, not the mean level
- **Kingman and Palmyra remain the most coherent multi-variable signal** in the dataset — consistent long-term change across MLD, chl_max, and anomaly models
- **NWHI atolls (Kure, Midway, Pearl & Hermes)** show a MLD + anomaly pattern without a chl_max trend, suggesting MLD is changing but the effect on absolute chl-a maxima is being absorbed by the seasonal cycle rather than driving net change in the mean
- **Tutuila's consistent chl trend without MLD signal** remains an outlier pointing to a non-oceanographic driver
- **Marianas and most MHI are stable** — no long-term anomaly trends, with Marianas seasonal signals largely residing in island means rather than anomaly structure
