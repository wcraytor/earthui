# Merge collision inventory — earthUI + glmnetUI + mgcvUI → one package (earthUI)

Generated 2026-07-03. Execution document for the one-CRAN-package merge.
Suffix convention: `_glmnet` / `_mgcv` on method-specific colliding names.
Canonical copies: earthUI's (newest lineage after the 2026-07-03 back-ports).

## Summary counts

| Package | Top-level function defs (R/) | Notes |
|---|---|---|
| earthUI | 162 | canonical / newest lineage; no `carryforward.R` |
| glmnetUI | 133 | module-based (`mod_*.R`, `*Server/*UI`) |
| mgcvUI | 139 | module-based (`mod_*_server/_ui`) |

- Names defined in >=2 packages: **81**
- In shared-infra files (regproj/regproj_db/locale/trilogy/disclaimer/specials/carryforward): **61** -> auto-resolved by keeping earthUI's copy
- Non-infra collisions needing a verdict: **20**
  - DEDUPE: 5 -> `has_latex_`, `fit_stamp_`, `detect_categoricals_`, `eval_g_function_`, `convert_quarto_file`
  - SUFFIX: 12 groups -> `generate_quarto_report`, `prepare_report_assets`, `render_report`, `g_function_legend`, `plot_actual_vs_predicted`, `import_earth`, `export_knots_csv`, `build_interaction_matrix_`, `settings_db_*` (5 fns)
  - TRUE-CONFLICT: 2 -> `import_data`, `detect_column_types`

## Shared-infra confirmation

| File | Identical across all 3? | Detail |
|---|---|---|
| trilogy.R | YES | byte-identical e/g/m |
| disclaimer.R | YES | byte-identical e/g/m |
| carryforward.R | glmnet==mgcv; **earthUI has NO carryforward.R** | graft from a sibling |
| regproj.R | no | diverges only by the package-named prefs trio |
| regproj_db.R | no | function lists match exactly (whitespace drift only) |
| locale.R | no | function lists match; env name differs (`eui_locale_env_` vs siblings) |
| specials.R | no | function lists match exactly |

Stragglers:
- `earth_carryforward_` (whole carryforward.R) absent from earthUI — MUST graft (glmnet==mgcv identical).
- Prefs trio: keep `earthui_prefs_*`; drop `glmnetui_prefs_*` / `mgcvui_prefs_*` (migration note below).
- Locale env: keep `eui_locale_env_`.

## Function collisions — verdict table

Call-site counts are raw grep counts over R/ + inst/ + tests/ (include the definition line).

| Name | Pkgs (file) | Verdict | Call sites e/g/m | Notes |
|---|---|---|---|---|
| `has_latex_` | e,g,m (export_report.R) | DEDUPE -> earthUI | 3/4/3 | byte-identical all three |
| `fit_stamp_` | e,g,m (e/m: format_results.R, g: utils.R) | DEDUPE -> earthUI/format_results.R | 11/9/10 | identical body |
| `detect_categoricals_` | g,m | DEDUPE (g==m); earth lacks it | 0/2/2 | NOT earthUI's `detect_categoricals` (no underscore) — different function |
| `eval_g_function_` | e,g | DEDUPE -> earthUI (superset) | 46/3/0 | earth adds `response_idx` for multivariate; default keeps 3-arg glmnet calls working |
| `convert_quarto_file` | e,g,m | DEDUPE -> earthUI (trivial merge) | 7/3/3 | keep earth's `execute_params=` signature; adapt 3+3 sibling call sites |
| `generate_quarto_report` | e,g,m | SUFFIX `_glmnet`,`_mgcv` | 5/3/4 | validators + qmd template + package= differ |
| `prepare_report_assets` | e,g,m | SUFFIX `_glmnet`,`_mgcv` | 27/20/3 | method-specific; glmnet body ~508 lines |
| `render_report` | e,g | SUFFIX `_glmnet` (earth keeps) | 26/15/0 | glmnet has quarto/rmarkdown fallback dispatch; mgcv has none |
| `g_function_legend` | e,g,m | SUFFIX `_glmnet`,`_mgcv` | 4/5/5 | method-specific legend math |
| `plot_actual_vs_predicted` | e,m | SUFFIX `_mgcv` (earth keeps) | 12/0/16 | diff 47 lines |
| `import_earth` | g,m | SUFFIX `_glmnet`,`_mgcv` | 0/32/35 | different return shapes (`glmnetUI_earth_import` vs knots/memcheck) |
| `export_knots_csv` | g,m | SUFFIX `_glmnet`,`_mgcv` | 0/7/14 | diff 52 |
| `build_interaction_matrix_` | g,m | SUFFIX `_glmnet`,`_mgcv` | 0/2/2 | different signatures (coef_df vs gam_result) |
| `settings_db_connect_` | e,m | SUFFIX `_mgcv` (earth keeps) | 5/0/10 | mgcv schema is GAM-specific (settings_v2) |
| `settings_db_read_` | e,m | SUFFIX `_mgcv` | 10/0/5 | |
| `settings_db_write_` | e,m | SUFFIX `_mgcv` | 8/0/3 | |
| `settings_db_path_` | e,m | SUFFIX `_mgcv` | 2/0/4 | mgcv reads `getOption("mgcvUI.settings_db_path")` |
| `settings_db_evict_` | e,m | SUFFIX `_mgcv` | 4/0/4 | |
| `import_data` | e,m | TRUE-CONFLICT (manual merge) | 39/0/59 | fold mgcv's `check_memory_for_file_` guard + `clean_names_()` into earth's reader; keep earth signature; re-point mgcv's call sites |
| `detect_column_types` | g,m | TRUE-CONFLICT (manual merge) | 0/20/24 | glmnet has "<=10 unique => factor" rule; mgcv has logical/character branches. Also overlaps earthUI's `detect_types` — reconcile as a family |

Settings-db extras (graft notes): mgcvUI adds `settings_db_read_locale_`/`settings_db_write_locale_`; earthUI adds `settings_db_disconnect_`; glmnetUI has no settings_db.R.

## Non-function collisions

### S3 classes
- `earthUI_result` — earth owns it; glmnetUI also constructs it and both siblings `inherits()` on it. Keep single definition; post-merge confirm `validate_earthUI_result` accepts glmnet-imported instances.
- `glmnetUI_earth_import`, `glmnetUI_earth_knots`, `mgcvUI_result`, `mgcvUI_earth_knots` — single-owner, coexist fine.

### Environments / prefs / options
- Locale env: keep `eui_locale_env_`.
- `R_user_dir("glmnetUI"/"mgcvUI")` prefs orphaned post-merge -> consider one-time import into `R_user_dir("earthUI")`.
- Options: unify `earthUI.trilogy`/`glmnetUI.trilogy`/`mgcvUI.trilogy` -> single `earthUI.trilogy` (shared in-process lock — deliberate behavior decision). `*_regproj_reference_cache` collapses with regproj.R. `mgcvUI.settings_db_path` -> rename with the suffixed settings_db.

### inst/ paths
- Identical content, keep ONE: `inst/app/www/favicon.png`, `www/logo.png`, `inst/extdata/regproj_geo.rds`, `regproj_reference.json`, `inst/quarto/reference.docx`.
- Collisions -> per-method app dirs: `inst/app_earth/`, `inst/app_glmnet/`, `inst/app_mgcv/` (earth+glmnet both ship ui.R/server.R/global.R; mgcv ships app.R; glmnet+mgcv both ship inst/app/sales_grid.R).
- `inst/extdata/Appraisal_1.csv` differs earth vs glmnet -> keep both under distinct names or pick one.
- Quarto templates already distinct: earth_report.qmd / glmnet_report.qmd / gam_report.qmd.

### Custom Shiny messages (only matter if UIs share one session/tab mode)
Overlaps to namespace in the tabs phase: `close_settings_dropdown` (e,g,m), `download_check`, `fitting_start`, `fitting_done`, `trace_line` (e,m), `sale_age_added`, `collect_and_save_defaults` (e,g). Prefixed ones (`eui_*`, `glmnet_*`, `mgcv_*`) fine.

### localStorage / CSS
Already distinct per app (`earthUI_*`/`glmnetUI_*`/`mgcvUI_*`; `eui-`/`glmnet-`/`mgcv-`). No collision while apps have separate sessions.

### testthat basenames
- 3-way: `test-trilogy.R`, `test-locale.R` -> keep one each.
- earth-vs-mgcv: `test-settings_db.R`, `test-plot_results.R`, `test-import_data.R`, `test-format_results.R`, `test-export_report.R`, `test-detect_types.R` -> rename mgcv's with `_mgcv` suffix; merge assertions where code deduped.

## One-package-only renames for clarity

| Current | Pkg | Post-merge | Note |
|---|---|---|---|
| `glmnetUI()` (launcher, port 7879) | g | `launch_glmnet()` | |
| `mgcvUI()` (launcher, port 7880) | m | `launch_mgcv()` | align signature to earth's `launch(port, trilogy, ...)` |
| `launch()` | e | keep (optionally alias `launch_earth`) | |
| `check_sign_warnings` (g) / `check_sign_consistency` (m) | | verify semantic twins; suffix or unify | |
| `to_snake_case` (g) / `snake_token_` (e) / `clean_names_` (m) | | consolidate name-normalizers | |
| glmnet `*Server/*UI` module set | g | prefix `glmnet_` for namespace clarity | no hard collision today |

## Recommended merge order

**Phase 1 — clean graft (no renames):** keep earthUI's 7 shared-infra files; graft `carryforward.R` from a sibling; dedupe the 5 identical helpers (adapt 6 convert_quarto_file sibling call sites); graft all earth-unique files unchanged.

**Phase 2 — suffix pass (mechanical):** glmnet files -> `_glmnet` renames + `launch_glmnet`; mgcv files -> `_mgcv` renames incl. whole settings_db + `launch_mgcv`.

**Phase 3 — manual merges:** `import_data` (fold mgcv guard + clean_names into earth's), `detect_column_types`/`detect_types`/`detect_categoricals*` family reconciliation, unify the trilogy option name.

**Phase 4 — inst/ + tests reshuffle:** per-method app dirs, single-copy shared assets, test file dedupe/suffixing.
