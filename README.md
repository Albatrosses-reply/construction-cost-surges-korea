# Construction Cost Surges and the Delayed Contraction of Housing Starts: Evidence from Korea

Replication package for the paper by TaeYoung Kang, SangGun Lee and HoJun Kang.

Archived at Zenodo: https://doi.org/10.5281/zenodo.23312661 (version v1). Code mirror: https://github.com/Albatrosses-reply/construction-cost-surges-korea

`replication.ipynb` reproduces every table and figure of the paper, in the order of the paper, from the data in `data/`.
The notebook is saved with its outputs. A full run takes about two minutes.

## How to run

1. Install R 4.4 with the packages in `requirements.txt`, Python 3 with `pandas`, `jupyter` and `nbconvert`,
   and a LaTeX distribution with TikZ (used for Figure 1).
2. From this folder run `jupyter nbconvert --to notebook --execute --inplace replication.ipynb`,
   or run `code/run_all.sh` to produce the tables and figures without the notebook.
3. Outputs: estimates in `code/out/`, LaTeX tables in `code/tables/`, figures in `code/figures/`.

The province boundaries used for the maps (Figures 6 and 7) are not redistributed here. `code/n8_rest_figures.R`
downloads the Statistics Korea 2018 province boundaries from the public `southkorea/southkorea-maps` repository at run time.

## Tables and figures

| Paper | Script | Output |
|---|---|---|
| Figure 1 | `n11_fig_institutions.py` | `figures/fig_institutions.pdf` |
| Table 1 | `n7_rest_tables.R`, `finalize_tables.py` | `tables/tab_t1_new.tex` |
| Figures 2, 3 | `n01_descriptive_figures.R` | `figures/fig_cost_growth.png`, `figures/fig_event.png` |
| Table 2 | `n7_rest_tables.R` | `tables/tab_t2_new.tex` |
| Table 3 | `n1_estimate.R`, `n10_v2_extras.R` | `tables/tab_t3_main.tex` |
| Figure 4 | `n3_figures.R` | `figures/fig3a_avg_irf_new.png`, `figures/fig3b_regime_irf_new.png` |
| Figure 5 | `n5_placebo_new.R`, `n10_v2_extras.R` | `figures/fig4_placebo_new.png` |
| Table 4 | `n2_tables.R` | `tables/tab_t4_new.tex` |
| Figures 6, 7 | `n8_rest_figures.R` | `figures/fig5_map_evolution_new.png`, `figures/fig6_map_h18_new.png` |
| Table 5 | `n6_rest_estimate.R`, `n7_rest_tables.R` | `tables/tab_t5_new.tex` |
| Table 6 | `n19_cap_timing.R` | `tables/tab_t8_cap_timing.tex` |
| Table 7 | `n20_credit_demand.R` | `tables/tab_t10_paths.tex` |
| Table 8 | `n2_tables.R`, `finalize_tables.py` | `tables/tab_t6_new.tex` |
| Table 9 | `n17_robust_avg_costlevel.R` | `tables/tab_t7_robust_avg.tex` |
| Table A.1 | `n0_markov.R`, `n0b_markov_table.R` | `tables/tab_tA1_markov.tex` |
| Table A.2 | `n7_rest_tables.R` | `tables/tab_tA2_new.tex` |
| Table A.3 | `d7_ic_lags.R`, `n10_v2_extras.R` | `tables/tab_tA5_ic.tex` |
| Table A.4 | `n20_credit_demand.R` | `tables/tab_t9_episode.tex` |
| Table A.5 | `n12_17regions.R` | `tables/tab_tA6_17reg.tex` |

Some scripts also write intermediate results that are not reported in the paper (for example instrumented projections
in `n6_rest_estimate.R` and `n10_v2_extras.R`).

## Data

All series are compiled from public official statistics. Regions: Seoul, Busan, Daegu, Incheon, Gwangju, Daejeon, Ulsan and
Gyeonggi; the seventeen-province panel adds the remaining provinces and Sejong. Variables prefixed `l.` or `l_` are natural
logarithms (`l.start_pri` and `l.permit_pri` are ln(1 + x)); `yoy_` is 100 times the twelve-month log difference.

| File | Content |
|---|---|
| `monthly_series_wide.csv` | Monthly national and regional series, 2005:1-2026:1, one row per month |
| `panel_8_regions.csv` | Estimation panel, eight regions, 2011:1-2026:1 (1,448 region-months) |
| `panel_8_regions_newsale_price.csv` | Same panel restricted to months with the new-sale price, 2013:9-2026:1 |
| `panel_17_provinces.csv` | Seventeen provinces and metropolitan cities, 2011:1-2026:1 (Sejong from 2012) |
| `applyhome_announcements.csv` | Presale announcements in the Applyhome register, February 2020 to July 2026 (fields used by the analysis only) |
| `basic_construction_cost_revisions.csv` | Effective dates of the revisions of the basic construction cost under the presale price cap |

Main variables

| Variable | Definition | Source |
|---|---|---|
| `CCI`, `yoy_CCI` | Construction cost index and its year-over-year growth (%) | Korea Institute of Civil Engineering and Building Technology |
| `regime_dummy` | High-cost regime (Markov-switching smoothed probability above 0.5) | Authors' calculation |
| `start_pri_*`, `l.start_pri` | Private housing starts (units) | Ministry of Land, Infrastructure and Transport (MOLIT) |
| `permit_pri_*`, `l.permit_pri` | Private housing permits (units), as published: totals cumulated from January of each year. The code converts them to monthly flows (`n1_estimate.R`) | MOLIT |
| `unsold_pri_*`, `l.unsold_pri` | Unsold private housing units | MOLIT |
| `apt_trd_*`, `l.apt_trd` | Apartment transactions | Korea Real Estate Board (KREB) |
| `apt_price_idx_*`, `l.apt_price_idx` | Apartment sales price index | KREB |
| `hh_*`, `l.hh` | Registered households | Statistics Korea |
| `average sale price_*`, `l.average_sale_price` | Average presale price per square meter of newly supplied private housing | Korea Housing and Urban Guarantee Corporation (HUG) |
| `rate_k3y`, `spread_corp3y` | Three-year Treasury yield and corporate-Treasury spread (percent) | Bank of Korea (ECOS) |
| `ppi`, `fx_usdkrw` | Producer price index and KRW/USD exchange rate | Bank of Korea (ECOS) |
| `Aluminum`, `Iron ore`, `Copper` | World commodity prices | World Bank Commodity Price Data (CC BY 4.0) |
| `ID_*`, `yoy_CCI_*` | Region indicators and their interactions with cost growth | Authors' calculation |

`applyhome_announcements.csv`: `HOUSE_MANAGE_NO` announcement identifier, `HOUSE_SECD_NM` housing type (APT),
`RENT_SECD_NM` sale or rental (분양주택 = for sale), `HOUSE_DTL_SECD_NM` private or public (민영 = private),
`RCRIT_PBLANC_DE` announcement date, `PARCPRC_ULS_AT` subject to the presale price cap (Y/N),
`TOT_SUPLY_HSHLDCO` units offered. Source: Applyhome subscription register (Korea Real Estate Board), public data portal.

`basic_construction_cost_revisions.csv`: effective and issue dates of the MOLIT notice on the basic construction cost of
price-capped housing, notice number, type (scheduled, extraordinary, add-on items only), construction cost index of the notice
(March 2020 = 1, blank where the notice gives it only as an image), the application rule, and the record number in the
National Law Information Center (law.go.kr).

## License

Code: MIT. Data: CC BY 4.0, with attribution to the original sources above. See `LICENSE`.

## Citation

See `CITATION.cff`.
