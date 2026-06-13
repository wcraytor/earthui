# Trilogy — Design (v0.2)

A three-method comparative appraisal analysis that runs **earthUI** (MARS),
**glmnetUI** (elastic net), and **mgcvUI** (GAM) on one subject property and
reconciles their value conclusions into a single report — all inside a shared
regProj appraisal project.

> **v0.2 supersedes v0.1.** Key change: the Trilogy is driven by running the
> **existing apps in a "trilogy mode"** launched from a single **Trilogy UI** —
> not by a separate headless orchestrator. Grouping is by a shared **fit
> timestamp**. This sidesteps the headless-extraction work from the audit (now
> optional, §10).

---

## 1. Goal & Scope

- **Appraisal only** (`purpose = "appraisal"`).
- For one **subject property** + comparables: **3 Sales Grids** (one per method)
  and **3 value conclusions** + comparative statistics + a one-page summary.
- **earthUI is the head of the pipeline:** its locked model feeds glmnetUI and
  mgcvUI. glmnet/mgcv are repeatable consumers.

## 2. Architecture — Trilogy UI + "trilogy mode"

- **The Trilogy UI is the single entry point.** It owns `trilogy.json` and is the
  only way to start a trilogy run. The three apps are **never opened standalone
  for a trilogy run** — they are launched *in trilogy mode from the Trilogy UI*,
  which passes the project + trilogy context (e.g. `glmnetUI(trilogy = <ctx>)`).
- Each app, launched in trilogy mode, runs its **own normal UI** (so the user
  still finalizes settings there) but additionally:
  1. shows a prominent **"Trilogy Mode" banner at the top** (§6),
  2. reads the **locked** earth model + locked settings + CQA from `trilogy.json`,
  3. tags all outputs with the **fit timestamp** (§4) and **registers** that fit
     timestamp back into `trilogy.json`.
- **No headless extraction required.** The apps do the work through their UIs, so
  glmnetUI's Shiny-bound fit, etc., are fine as-is (see §10).

```
Trilogy UI  (single entry point; owns trilogy.json)
  ├─ launch earthUI  (trilogy mode) → fit → lock chosen .rds → outputs tagged T_earth
  ├─ launch glmnetUI (trilogy mode) → import locked earth → fit → outputs tagged T_glmnet
  ├─ launch mgcvUI   (trilogy mode) → import locked earth → fit → outputs tagged T_mgcv
  └─ assemble combined report from the grouped (T_earth, T_glmnet, T_mgcv)
```

## 3. Storage Layout

Individual method outputs stay in their **existing** folders (tagged by fit
timestamp). The Trilogy adds one new subtree for coordination + the report:

```
regProj/appr/<project>/
  <os>_in/                       # shared input data                 (existing)
  <os>_out_earth/                # earth outputs, files named <T>_*   (existing dir)
  <os>_out_glmnet/               # glmnet outputs, files named <T>_*  (existing dir)
  <os>_out_mgcv/                 # mgcv outputs, files named <T>_*    (existing dir)
  trilogy/                       # NEW (coordination + report only)
    trilogy.json                 # shared settings + lock + the fit-T grouping
    <os>_out/
      combined/                  # the comparative report bundle + 1-page summary
```

- The per-method runs do **not** copy into `trilogy/`; the combined report
  **references** their files by fit timestamp.

## 4. Fit-Timestamp Convention (R5) — the grouping key

The mechanism that ties a trilogy run together. **A permanent change in all three
apps (applies to standalone runs too):**

- Capture the **time of the fit** `T` (e.g. `20260613-103045`) **when the model is
  fit** — not each file's creation time (today's behavior).
- **Embed `T` in every output filename for that fit:** the sales grid, the
  `conclusion.json`, and the report artifacts `*.qmd`, `*.docx`, `*.html`,
  `*.pdf` all carry the same `T`. So a fit's whole output set is self-identifying.
- A **Trilogy run records the three fit timestamps** `(T_earth, T_glmnet,
  T_mgcv)` in `trilogy.json`; the combined report gathers the grouped files by
  those timestamps.

## 5. The `trilogy.json` Contract

```jsonc
{
  "schema": 2,
  "subject": { "row": 1, "effective_date": "2026-06-13" },

  // Shared, set in earthUI, carried to all three
  "shared": {
    "data_file": "comps_2026.csv",
    "target": "sale_price",
    "cqa_mode": "raw" | "per_sf",          // raw or CQA/SF
    "cqa": { /* per-comparable CQA, entered once in earthUI */ }
  },

  // The lock: the canonical earth fit (one .rds / one fit timestamp)
  "earth_lock": {
    "locked": true,
    "fit_ts": "20260613-101500",
    "rds": "<os>_out_earth/20260613-101500_run3.rds",
    "locked_at": "2026-06-13T10:20:00Z",
    "downstream_locked": { "predictors": [], "factor": [], "linear": [] }
  },

  // Per-app settings: some fields mirror downstream_locked (read-only once
  // earth is locked), the rest are app-specific and user-editable.
  "earth":  {}, "glmnet": {}, "mgcv": {},

  // The grouping that defines this trilogy report (R5)
  "run": {
    "methods": ["earth", "glmnet", "mgcv"],   // any 1–3
    "fit_ts": { "earth": "20260613-101500",
                "glmnet": "20260613-104012",
                "mgcv":   "20260613-105530" }
  }
}
```

- **Editable two ways:** directly as JSON, or via each app's UI (in trilogy mode)
  writing its slice.
- **Locking = today's earth-import lock + extras:** glmnet/mgcv already lock
  Include/Type from an imported earth model; the Trilogy adds **CQA** + the
  **raw/per-SF mode** to what's locked.

## 6. "Trilogy Mode" Indicator

When an app is launched in trilogy mode, it must clearly show that it is in
**Trilogy Mode** so the user can never confuse a trilogy run with a standalone
run. The simplest placement is **in parentheses after the app's title at the
top** — e.g. **earthUI (Trilogy Mode)**, **glmnetUI (Trilogy Mode)**,
**mgcvUI (Trilogy Mode)**. (A small badge/banner is an alternative, but the
title-parenthetical is the baseline.) It may also surface minimal context (the
project; and for glmnet/mgcv, that they are bound to the locked earth model).

## 7. Run Flow (phases)

**Phase 1 — earthUI + RDS lock** (run once; the stable head)
1. From the Trilogy UI, launch **earthUI in trilogy mode**; fit and iterate.
2. **Lock** the chosen earth `.rds` — via earthUI's **"Lock Model Output (for
   glmnetUI/mgcvUI)"** control or the Trilogy UI. Locking records the earth
   `fit_ts`, the `.rds`, `shared` (CQA, raw/SF), and `downstream_locked`.

**Phase 2 — glmnet / mgcv** (repeatable)
3. From the Trilogy UI, launch the selected method(s) **in trilogy mode**; each
   imports the locked earth model, applies locked settings + CQA, the user
   finalizes remaining params, fits, and the app tags outputs with its `fit_ts`
   and registers it under `run.fit_ts`.

**Phase 3 — Combined report** (repeatable)
4. The Trilogy UI assembles the comparative report from the grouped `fit_ts`
   (3 grids + 1-page summary) into `trilogy/<os>_out/combined/`.

> Phases 2–3 re-run freely on top of the single locked Phase 1. glmnet **or**
> mgcv may run alone, provided earth is locked.

## 8. Value Conclusion + Reconciliation

Each app emits a standardized `conclusion.json` (named with its `fit_ts`):

```jsonc
{ "method": "glmnet", "fit_ts": "20260613-104012",
  "subject_value": 512000, "cqa_reconciled_value": 508750,
  "metrics": { "r2": 0.91, "cv_r2": 0.88, "rmse": 18200, "cod": 7.4, "prd": 1.02 },
  "n_comps": 42 }
```

**Reconciliation policy: no weighting.** earthUI's value conclusion is *the*
conclusion, and its Sales Grid is given preference. glmnetUI's and mgcvUI's
values are presented only as **corroborating support (or lack of)** — the
appraiser decides whether to give them weight. The 1-page summary leads with the
earthUI conclusion and lists the glmnet/mgcv values beside it as comparison
points, **never** computing an averaged/weighted final.

## 9. Trilogy UI Responsibilities

- Create/open a trilogy run for an appraisal project; own `trilogy.json`.
- Show **lock status** and set/clear the earth lock (alternative to earthUI's
  button).
- **Launch** each app in trilogy mode (the only path to a trilogy run).
- **Method selection:** all 3, or any 1–2 (glmnet/mgcv enabled only once earth is
  locked).
- Track registered `fit_ts` per method; **assemble + render** the combined report.
- **Initial output (deliberately minimal):** the 3 per-method output sets (incl.
  sales grids) + the 1-page conclusions summary.

## 10. Headless-API Audit (now optional)

Because trilogy mode runs the apps through their own UIs, the headless extraction
is **no longer on the critical path**. Recorded for the optional future of a
fully headless/batch runner or a shared core:

| Capability | earthUI | glmnetUI | mgcvUI |
|---|---|---|---|
| Pure fit | `fit_earth()` ✓ | Shiny-bound in `modelingServer` | `fit_gam()` ✓ |
| Sales grid (pure) | `build_sales_grid` + `select_sales_grid_comps` ✓ | `inst/app/sales_grid.R` (internal) | `inst/app/sales_grid.R` (internal) |
| RCA (pure) | `compute_rca_adjustments` ✓ | app-internal | app-internal |
| Report assets / render | ✓ | `prepare_report_assets`/`render_report` ✓ | `prepare_report_assets`/`render_gam_report` ✓ |

*Optional, post-v1:* a shared sales-grid/RCA core (the three currently differ)
would make the grids more directly comparable and cut maintenance.

## 11. Build Order

1. **Shared fit-timestamp convention (R5)** in all three apps — capture fit `T`,
   name all outputs (grid, conclusion, qmd/docx/html/pdf) with `T`. Foundational;
   independent of the rest; benefits standalone use too.
2. **Trilogy-mode plumbing** in each app — a launch parameter that enables
   trilogy mode: the **top banner** (§6), reading locked inputs from
   `trilogy.json`, and registering its `fit_ts`.
3. **earthUI lock** — the "Lock Model Output" control writing `earth_lock` +
   `shared` (CQA, raw/SF) + `downstream_locked`.
4. **glmnet/mgcv trilogy reads** — apply the lock (extend today's earth-import
   lock to also apply CQA + raw/SF); emit `conclusion.json`.
5. **Trilogy UI** — entry point, launcher, lock control, method selection,
   grouping, combined-report assembly + Quarto summary.

## Open Questions

- **Launch mechanism** for "app in trilogy mode" from the Trilogy UI (new browser
  tab/process per app, deep-link, or embed?). Affects how the banner + context
  are passed.
- **Exact `trilogy/<os>_…` subfolder naming** to match the existing convention.
- **Filename `T` format** and where exactly `T` sits in each existing filename
  pattern (prefix vs suffix), per app.

## Requirements of record (source)

- R1 — Appraisal-only; 3 sales grids + 3 value conclusions + comparative stats.
- R2 — Under `regProj/appr/<project>/trilogy/`; `trilogy.json` lives under
  `trilogy/`. Individual method outputs stay in existing `<os>_out_<method>/`.
- R3 — Sequenced settings with locking: earth runs first and is locked (one
  `.rds`); locking earth also locks certain glmnet/mgcv settings (+ CQA, raw/SF);
  settings editable via JSON or the apps' UIs; a Combined-Report (Trilogy) UI
  selects which methods to run (1–3); glmnet/mgcv can run alone if earth is
  locked; initial output = 3 method output sets + 1-page summary. Two-phase run:
  (1) earthUI + RDS lock, (2) glmnet/mgcv, repeatable.
- R4 — CQA (raw or per-SF) entered once in earthUI, carried to glmnet/mgcv
  unchanged; per-app CQA is a future enhancement.
- R5 — **Fit-timestamp grouping:** use the fit time (not file-creation time) as
  the canonical timestamp; embed it in all output filenames (incl. qmd/docx/
  html/pdf) in all three apps; the trilogy records the trio of fit timestamps to
  group files for the combined report.
- R6 — **Single entry point + trilogy mode indicator:** the three apps run in
  trilogy mode only when launched from the Trilogy UI, and each shows it is in
  trilogy mode — baseline placement is **"(Trilogy Mode)" in parentheses after
  the app title** (e.g. "earthUI (Trilogy Mode)").
