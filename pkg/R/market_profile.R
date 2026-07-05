#' Market Area Profile -- Climate Zones (California)
#'
#' The first market-area profile. Maps a project's location (county + city) to
#' one of seven buyer-recognized climate regions -- reduced from the California
#' Energy Commission's official 16 building-climate zones -- and supplies ordinal
#' contributory-value priors for HVAC / cooling features by region.
#'
#' These tables are the CANONICAL source for both R and Prolog:
#' \code{write_climate_prolog_()} regenerates
#' \code{inst/prolog/climate_regions_ca.pl} from them, so the two never drift.
#'
#' County defaults are the dominant-population region for the county; many
#' California counties span 3-4 regions, so split counties rely on the city
#' overrides and need appraiser review.
#'
#' @name market_profile
NULL

# Region atoms + human labels.
climate_regions_ <- c(
  cool_coast     = "Cool coastal / marine layer",
  warm_coast     = "Warm SoCal coastal basin",
  inland_valley  = "Inland Bay Area & inland coastal valleys",
  central_valley = "Central Valley hot-summer",
  desert         = "Low & high desert",
  mountain       = "Mountain / snow / high elevation",
  foothill       = "Transitional foothill / wildfire belt"
)

# Ordinal value scale -> integer rank (0..6).
value_ranks_ <- c(very_low = 0L, low = 1L, low_moderate = 2L, moderate = 3L,
                  moderate_high = 4L, high = 5L, very_high = 6L)

# Feature-value matrix: region -> (feature = ordinal).
climate_matrix_ <- list(
  cool_coast     = c(heating = "high",          ac = "low_moderate",  swamp_cooler = "very_low",  whole_house_fan = "low",          filtration = "moderate"),
  warm_coast     = c(heating = "moderate",      ac = "moderate_high", swamp_cooler = "low",       whole_house_fan = "low_moderate", filtration = "moderate"),
  inland_valley  = c(heating = "moderate_high", ac = "high",          swamp_cooler = "low_moderate", whole_house_fan = "moderate",  filtration = "high"),
  central_valley = c(heating = "moderate_high", ac = "very_high",     swamp_cooler = "moderate",  whole_house_fan = "moderate",     filtration = "high"),
  desert         = c(heating = "moderate_high", ac = "very_high",     swamp_cooler = "moderate",  whole_house_fan = "low_moderate", filtration = "moderate"),
  mountain       = c(heating = "very_high",     ac = "low_moderate",  swamp_cooler = "very_low",  whole_house_fan = "low",          filtration = "moderate_high"),
  foothill       = c(heating = "high",          ac = "high",          swamp_cooler = "low_moderate", whole_house_fan = "moderate",  filtration = "high")
)

# County default region (dominant-population region; SPLIT counties noted).
climate_county_default_ <- c(
  san_francisco = "cool_coast", san_mateo = "cool_coast", marin = "cool_coast",
  santa_cruz = "cool_coast", monterey = "cool_coast", del_norte = "cool_coast",
  humboldt = "cool_coast", mendocino = "cool_coast",
  orange = "warm_coast", san_diego = "warm_coast", los_angeles = "warm_coast",
  ventura = "warm_coast", santa_barbara = "warm_coast", san_luis_obispo = "warm_coast",
  santa_clara = "inland_valley", alameda = "inland_valley", contra_costa = "inland_valley",
  napa = "inland_valley", sonoma = "inland_valley", san_benito = "inland_valley",
  solano = "inland_valley", riverside = "inland_valley", san_bernardino = "inland_valley",
  sacramento = "central_valley", yolo = "central_valley", sutter = "central_valley",
  yuba = "central_valley", colusa = "central_valley", glenn = "central_valley",
  tehama = "central_valley", butte = "central_valley", shasta = "central_valley",
  san_joaquin = "central_valley", stanislaus = "central_valley", merced = "central_valley",
  madera = "central_valley", fresno = "central_valley", kings = "central_valley",
  tulare = "central_valley", kern = "central_valley",
  imperial = "desert", inyo = "desert",
  alpine = "mountain", sierra = "mountain", plumas = "mountain", lassen = "mountain",
  modoc = "mountain", mono = "mountain", trinity = "mountain", siskiyou = "mountain",
  el_dorado = "foothill", placer = "foothill", nevada = "foothill", amador = "foothill",
  calaveras = "foothill", tuolumne = "foothill", mariposa = "foothill", lake = "foothill"
)

# City overrides for split counties: city -> c(county, region).
climate_city_override_ <- list(
  pacifica = c("san_mateo", "cool_coast"), daly_city = c("san_mateo", "cool_coast"),
  half_moon_bay = c("san_mateo", "cool_coast"), monterey = c("monterey", "cool_coast"),
  pacific_grove = c("monterey", "cool_coast"), carmel = c("monterey", "cool_coast"),
  eureka = c("humboldt", "cool_coast"), arcata = c("humboldt", "cool_coast"),
  fort_bragg = c("mendocino", "cool_coast"), bodega_bay = c("sonoma", "cool_coast"),
  long_beach = c("los_angeles", "warm_coast"), santa_monica = c("los_angeles", "warm_coast"),
  torrance = c("los_angeles", "warm_coast"), oceanside = c("san_diego", "warm_coast"),
  carlsbad = c("san_diego", "warm_coast"), chula_vista = c("san_diego", "warm_coast"),
  livermore = c("alameda", "inland_valley"), walnut_creek = c("contra_costa", "inland_valley"),
  concord = c("contra_costa", "inland_valley"), gilroy = c("santa_clara", "inland_valley"),
  morgan_hill = c("santa_clara", "inland_valley"), santa_rosa = c("sonoma", "inland_valley"),
  paso_robles = c("san_luis_obispo", "inland_valley"), king_city = c("monterey", "inland_valley"),
  ukiah = c("mendocino", "inland_valley"),
  palm_springs = c("riverside", "desert"), palm_desert = c("riverside", "desert"),
  cathedral_city = c("riverside", "desert"), indio = c("riverside", "desert"),
  coachella = c("riverside", "desert"), blythe = c("riverside", "desert"),
  victorville = c("san_bernardino", "desert"), hesperia = c("san_bernardino", "desert"),
  apple_valley = c("san_bernardino", "desert"), barstow = c("san_bernardino", "desert"),
  twentynine_palms = c("san_bernardino", "desert"), yucca_valley = c("san_bernardino", "desert"),
  lancaster = c("los_angeles", "desert"), palmdale = c("los_angeles", "desert"),
  ridgecrest = c("kern", "desert"), mojave = c("kern", "desert"),
  borrego_springs = c("san_diego", "desert"),
  south_lake_tahoe = c("el_dorado", "mountain"), truckee = c("nevada", "mountain"),
  big_bear_lake = c("san_bernardino", "mountain"), big_bear_city = c("san_bernardino", "mountain"),
  idyllwild = c("riverside", "mountain"), mount_shasta = c("siskiyou", "mountain"),
  paradise = c("butte", "foothill"), auburn = c("placer", "foothill"),
  grass_valley = c("nevada", "foothill"), nevada_city = c("nevada", "foothill"),
  sonora = c("tuolumne", "foothill"), placerville = c("el_dorado", "foothill"),
  julian = c("san_diego", "foothill")
)

# Normalize a county/city label to the atom convention (lowercase snake).
norm_place_ <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(x[1L])) return("")
  tolower(gsub("[^a-z0-9]+", "_", tolower(trimws(x[1L]))))
}

#' Climate region for a market area
#'
#' Resolves a county (and optional city) to one of the seven climate regions.
#' City overrides win over the county default; returns \code{"unknown"} if the
#' county is not mapped.
#'
#' @param county County name or code (e.g. \code{"kern"}).
#' @param city Optional city name or code (e.g. \code{"ridgecrest"}).
#' @return A region atom (character), or \code{"unknown"}.
#' @export
climate_region_for <- function(county, city = NULL) {
  cty <- norm_place_(county); cit <- norm_place_(city)
  if (nzchar(cit)) {
    ov <- climate_city_override_[[cit]]
    if (!is.null(ov) && identical(ov[[1L]], cty)) return(ov[[2L]])
  }
  reg <- climate_county_default_[[cty]]
  if (is.null(reg)) "unknown" else unname(reg)
}

#' Feature-value priors for a climate region
#'
#' @param region A region atom (see \code{climate_region_for}).
#' @return A data frame with columns \code{feature}, \code{ordinal}, \code{rank}
#'   (0-6), or \code{NULL} if the region is unknown.
#' @export
climate_feature_priors <- function(region) {
  m <- climate_matrix_[[region]]
  if (is.null(m)) return(NULL)
  data.frame(feature = names(m), ordinal = unname(m),
             rank = unname(value_ranks_[unname(m)]),
             stringsAsFactors = FALSE)
}

#' Full climate market-area profile for a location
#'
#' @inheritParams climate_region_for
#' @return A list with \code{region}, \code{label}, and \code{features}
#'   (the priors data frame).
#' @export
market_area_profile <- function(county, city = NULL) {
  region <- climate_region_for(county, city)
  list(region   = region,
       label    = if (region %in% names(climate_regions_)) unname(climate_regions_[region]) else "Unknown",
       features = climate_feature_priors(region))
}

# Regenerate inst/prolog/climate_regions_ca.pl from the tables above so the
# Prolog side stays identical to the R side. Returns the path written.
write_climate_prolog_ <- function(path = system.file("prolog", "climate_regions_ca.pl",
                                                      package = "earthUI")) {
  L <- function(...) paste0(...)
  esc <- function(x) x
  lines <- c(
    "/* California climate-region classification + feature-value priors.",
    " * GENERATED from R/market_profile.R via write_climate_prolog_() \u2014 DO NOT",
    " * hand-edit; edit the R tables and regenerate. 7 buyer-recognized regions",
    " * reduced from the CEC 16-zone system. County defaults are dominant-region;",
    " * split counties rely on city overrides and need appraiser review.",
    " *",
    " * climate_region(County, City, Region): city override -> county default -> unknown.",
    " * feature_value(Region, Feature, Ordinal); value_rank(Ordinal, 0..6).",
    " */",
    ":- discontiguous(climate_region_city/3).",
    ":- discontiguous(climate_region_county/2).",
    "")
  # City overrides
  lines <- c(lines, "% --- City overrides (City, County) -> Region ---")
  for (cit in names(climate_city_override_)) {
    ov <- climate_city_override_[[cit]]
    lines <- c(lines, L("climate_region_city(", cit, ", ", ov[[1L]], ", ", ov[[2L]], ")."))
  }
  # County defaults
  lines <- c(lines, "", "% --- County defaults County -> Region ---")
  for (cty in names(climate_county_default_)) {
    lines <- c(lines, L("climate_region_county(", cty, ", ", unname(climate_county_default_[cty]), ")."))
  }
  # Resolver
  lines <- c(lines, "",
    "climate_region(County, City, Region) :- climate_region_city(City, County, Region), !.",
    "climate_region(County, _City, Region) :- climate_region_county(County, Region), !.",
    "climate_region(_County, _City, unknown).", "")
  # value_rank
  lines <- c(lines, "% --- Ordinal value -> integer rank ---")
  for (o in names(value_ranks_))
    lines <- c(lines, L("value_rank(", o, ", ", value_ranks_[[o]], ")."))
  # feature_value
  lines <- c(lines, "", "% --- feature_value(Region, Feature, Ordinal) ---")
  for (reg in names(climate_matrix_)) {
    m <- climate_matrix_[[reg]]
    for (f in names(m))
      lines <- c(lines, L("feature_value(", reg, ", ", f, ", ", m[[f]], ")."))
  }
  lines <- c(lines, "",
    "region_feature_rank(County, City, Feature, Rank) :-",
    "    climate_region(County, City, Region), Region \\== unknown,",
    "    feature_value(Region, Feature, Ord), value_rank(Ord, Rank).", "")
  writeLines(lines, path)
  invisible(path)
}
