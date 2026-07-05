# Climate Packs & Market-Area Modules — Design

Status: **design, agreed 2026-07-04** — target: the release after 0.11.0.
The shipped California implementation (`climate_region_for()`,
`climate_feature_priors()`, `inst/prolog/climate_regions_ca.pl`) becomes the
**first jurisdiction pack** of this architecture, with no behavior change.

## The organizing principle

> "The world doesn't have very many Prolog programmers."

Field-supplied knowledge will overwhelmingly be authored by Valuation
Engineers and appraisers **assisted by an LLM** (Claude or similar), in
whatever human language they work in. Therefore:

1. **Field artifacts are data-shaped, never programs.** A pack is a set of
   fact tables (JSON; CSV accepted for assignment tables). All reasoning —
   resolvers, city-overrides-county fallthrough, ordinal ranking, plausibility
   banding — lives in the shipped engine, written once and professionally
   maintained. A VE contributes *what they know*, in table form; they never
   write a line of Prolog.
2. **earthUI generates the Prolog.** The canonical artifact is the pack; the
   Prolog facts consulted by derivation rules AND the R accessor tables are
   both generated from it at load time. This generalizes the existing
   no-drift discipline of `climate_regions_ca.pl`.
3. **The validator is the safety system**, not the author's skill. Every pack
   is machine-checked before it can be enabled (see Validation).
4. **Provenance is mandatory.** An LLM cannot be the responsible party. The
   pack format forces a human name, date, and cited authority onto every
   field-authored artifact; reports cite the pack (id, version, author).

## Knowledge layers and precedence

Consulted lowest-to-highest; later layers may refine earlier ones; the
project always wins.

| Layer | Artifact | Scope | Author |
|---|---|---|---|
| 1. Engine | package code + DCG grammar | global | earthUI maintainers |
| 2. Jurisdiction climate pack | `climate/<jurisdiction>.json` | a state / region / small country | shipped or field |
| 3. Market-area module | `market_areas/<name>.json` | a named market area | field (VE/appraiser) |
| 4. Project rules | `<project>_rules.pl` | one project | field (existing feature) |

Locations: shipped packs in `inst/climate/`; field packs under
`<REGPROJ_ROOT>/climate/` and `<REGPROJ_ROOT>/market_areas/` — the same
shipped-plus-extensible pattern as the geo database, so field knowledge
travels with the project tree. Resolution key: the project's regProj
country + admin levels (variable depth per country: `us_ca`, `de_by`, `sg`).

## Jurisdiction pack schema (draft)

```json
{
  "meta": {
    "id": "us_ca",
    "kind": "climate",
    "version": "1.2",
    "language": "en",
    "authors": [{"name": "W. B. Craytor", "role": "author-of-record"}],
    "date": "2026-07-04",
    "authority": "California Energy Commission 16 building-climate zones",
    "notes": "7 buyer-recognized regions reduced from the CEC 16."
  },
  "zones": [
    {"id": "cool_coast",  "label": "Cool Coast"},
    {"id": "desert",      "label": "Desert"}
  ],
  "assignments": {
    "level": "county",
    "default": [ {"unit": "081", "zone": "cool_coast"} ],
    "overrides": {
      "level": "city",
      "rows": [ {"unit": "ridgecrest", "zone": "desert",
                 "reason": "Kern county spans regions"} ]
    }
  },
  "priors": [
    {"feature": "swamp_cooler", "zone": "cool_coast", "value": "very_low"},
    {"feature": "ac_central",   "zone": "desert",     "value": "very_high"}
  ],
  "labels": { "de": {"swamp_cooler": "Verdunstungskühler"} }
}
```

Rules: `zones[].id`, `priors[].feature`, and all unit codes are **canonical
ASCII identifiers** (they become Prolog atoms, R column fragments, and
formula terms); human-language text lives only in `label`/`labels`
(UTF-8, per-language). Unit codes must match regProj admin codes.
Ordinal vocabulary is fixed: `very_low … very_high` (the engine owns the
rank mapping).

## Market-area module schema (draft)

Named, location-scoped, and also data-shaped:

- `meta` — as above, plus the market-area name and the regProj location(s)
  it applies to.
- `lexicon` — local vocabulary for the remarks grammar: word/phrase →
  canonical feature id (e.g., `"granny flat" -> adu`), per language. This
  makes grammar extension a word-list problem, not a DCG problem.
- `priors` — feature priors that refine the jurisdiction pack for this area.
- `derive` — OPTIONAL raw Prolog `derive/2` rules for the rare case tables
  cannot express; carries a `prolog_reviewed_by` provenance field. This is
  the only place field Prolog can appear, and the UI labels it expert-level.

## Validation (blocking, before a pack can be enabled)

1. **Schema** — structure, required meta incl. author-of-record + authority.
2. **Identifier hygiene** — ASCII ids; no collisions with engine vocabulary.
3. **Coverage** — every admin unit of the jurisdiction assigned (or listed
   in an explicit `unassigned` block); report "7 of 58 counties unassigned".
4. **Referential integrity** — units exist in regProj geo data; zones used
   in priors are declared; ordinal values in vocabulary.
5. **Dry run** — generate the Prolog facts, consult in vProlog, resolve a
   sample of locations end-to-end (the rules editor's syntax-check/dry-run
   pattern, applied to packs).

The validator's failure output is written to be **pasted back into the LLM**
verbatim — itemized, machine-precise, no prose.

## The authoring kit (ships in `docs/` and exports from the app)

1. The schema reference (this document's schema sections, expanded).
2. The complete California pack as the worked example.
3. A checklist (authority to cite, units list for the jurisdiction pulled
   from regProj, review steps).
4. **The prompt template**: a ready-to-paste Claude prompt that embeds the
   schema, the example, the target jurisdiction's admin-unit list, and the
   validation rules — so "draft a climate pack for Upper Bavaria" produces
   a validatable artifact on the first pass.

The loop: VE describes the market area in natural language → LLM drafts the
pack → earthUI validates → failures pasted back → VE reviews the green
result, fills the provenance block, signs.

## Language handling

- Trealla/vProlog is fully UTF-8; atoms in any language *work* — but
  canonical ids stay ASCII because they flow into earth model terms and
  LaTeX reports (the merge's `latex_escape_text_` handles symbols, not
  arbitrary scripts in formula terms).
- Display labels are localized in the pack (`labels.<lang>`); the app's
  locale country supplies the default language.
- Remarks parsing in other languages = lexicon entries in market-area
  modules first; full per-language grammar packs are a later, engine-level
  effort.

## Migration path

1. **CA refactor** — express the existing CA knowledge as pack `us_ca` v1.0;
   generate `climate_regions_ca.pl` and the R tables from it; behavior
   identical; the pack becomes the worked example. (Also aligns atoms to
   regProj codes — an existing TODO.)
2. **Registry + resolution** — `climate_pack_for(country, levels)`;
   shipped-pack discovery.
3. **Field discovery** — `<REGPROJ_ROOT>/climate/` + validator + UI listing.
4. **Market-area modules** — layer 3 with lexicon support + consult order.
5. **Authoring kit + app export**; localized labels.

Non-goals for v1: automatic $ calibration of priors (stays paired-sales
driven, per the RCA protocol); GIS point-in-polygon assignment (Tier 2,
`sf`-based, later); demographic profiles beyond economic variables (legal
constraint — Fair Housing Act / ECOA / USPAP).
