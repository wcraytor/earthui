/* California climate-region classification + feature-value priors.
 * GENERATED from R/market_profile.R via write_climate_prolog_() — DO NOT
 * hand-edit; edit the R tables and regenerate. 7 buyer-recognized regions
 * reduced from the CEC 16-zone system. County defaults are dominant-region;
 * split counties rely on city overrides and need appraiser review.
 *
 * climate_region(County, City, Region): city override -> county default -> unknown.
 * feature_value(Region, Feature, Ordinal); value_rank(Ordinal, 0..6).
 */
:- discontiguous(climate_region_city/3).
:- discontiguous(climate_region_county/2).

% --- City overrides (City, County) -> Region ---
climate_region_city(pacifica, san_mateo, cool_coast).
climate_region_city(daly_city, san_mateo, cool_coast).
climate_region_city(half_moon_bay, san_mateo, cool_coast).
climate_region_city(monterey, monterey, cool_coast).
climate_region_city(pacific_grove, monterey, cool_coast).
climate_region_city(carmel, monterey, cool_coast).
climate_region_city(eureka, humboldt, cool_coast).
climate_region_city(arcata, humboldt, cool_coast).
climate_region_city(fort_bragg, mendocino, cool_coast).
climate_region_city(bodega_bay, sonoma, cool_coast).
climate_region_city(long_beach, los_angeles, warm_coast).
climate_region_city(santa_monica, los_angeles, warm_coast).
climate_region_city(torrance, los_angeles, warm_coast).
climate_region_city(oceanside, san_diego, warm_coast).
climate_region_city(carlsbad, san_diego, warm_coast).
climate_region_city(chula_vista, san_diego, warm_coast).
climate_region_city(livermore, alameda, inland_valley).
climate_region_city(walnut_creek, contra_costa, inland_valley).
climate_region_city(concord, contra_costa, inland_valley).
climate_region_city(gilroy, santa_clara, inland_valley).
climate_region_city(morgan_hill, santa_clara, inland_valley).
climate_region_city(santa_rosa, sonoma, inland_valley).
climate_region_city(paso_robles, san_luis_obispo, inland_valley).
climate_region_city(king_city, monterey, inland_valley).
climate_region_city(ukiah, mendocino, inland_valley).
climate_region_city(palm_springs, riverside, desert).
climate_region_city(palm_desert, riverside, desert).
climate_region_city(cathedral_city, riverside, desert).
climate_region_city(indio, riverside, desert).
climate_region_city(coachella, riverside, desert).
climate_region_city(blythe, riverside, desert).
climate_region_city(victorville, san_bernardino, desert).
climate_region_city(hesperia, san_bernardino, desert).
climate_region_city(apple_valley, san_bernardino, desert).
climate_region_city(barstow, san_bernardino, desert).
climate_region_city(twentynine_palms, san_bernardino, desert).
climate_region_city(yucca_valley, san_bernardino, desert).
climate_region_city(lancaster, los_angeles, desert).
climate_region_city(palmdale, los_angeles, desert).
climate_region_city(ridgecrest, kern, desert).
climate_region_city(mojave, kern, desert).
climate_region_city(borrego_springs, san_diego, desert).
climate_region_city(south_lake_tahoe, el_dorado, mountain).
climate_region_city(truckee, nevada, mountain).
climate_region_city(big_bear_lake, san_bernardino, mountain).
climate_region_city(big_bear_city, san_bernardino, mountain).
climate_region_city(idyllwild, riverside, mountain).
climate_region_city(mount_shasta, siskiyou, mountain).
climate_region_city(paradise, butte, foothill).
climate_region_city(auburn, placer, foothill).
climate_region_city(grass_valley, nevada, foothill).
climate_region_city(nevada_city, nevada, foothill).
climate_region_city(sonora, tuolumne, foothill).
climate_region_city(placerville, el_dorado, foothill).
climate_region_city(julian, san_diego, foothill).

% --- County defaults County -> Region ---
climate_region_county(san_francisco, cool_coast).
climate_region_county(san_mateo, cool_coast).
climate_region_county(marin, cool_coast).
climate_region_county(santa_cruz, cool_coast).
climate_region_county(monterey, cool_coast).
climate_region_county(del_norte, cool_coast).
climate_region_county(humboldt, cool_coast).
climate_region_county(mendocino, cool_coast).
climate_region_county(orange, warm_coast).
climate_region_county(san_diego, warm_coast).
climate_region_county(los_angeles, warm_coast).
climate_region_county(ventura, warm_coast).
climate_region_county(santa_barbara, warm_coast).
climate_region_county(san_luis_obispo, warm_coast).
climate_region_county(santa_clara, inland_valley).
climate_region_county(alameda, inland_valley).
climate_region_county(contra_costa, inland_valley).
climate_region_county(napa, inland_valley).
climate_region_county(sonoma, inland_valley).
climate_region_county(san_benito, inland_valley).
climate_region_county(solano, inland_valley).
climate_region_county(riverside, inland_valley).
climate_region_county(san_bernardino, inland_valley).
climate_region_county(sacramento, central_valley).
climate_region_county(yolo, central_valley).
climate_region_county(sutter, central_valley).
climate_region_county(yuba, central_valley).
climate_region_county(colusa, central_valley).
climate_region_county(glenn, central_valley).
climate_region_county(tehama, central_valley).
climate_region_county(butte, central_valley).
climate_region_county(shasta, central_valley).
climate_region_county(san_joaquin, central_valley).
climate_region_county(stanislaus, central_valley).
climate_region_county(merced, central_valley).
climate_region_county(madera, central_valley).
climate_region_county(fresno, central_valley).
climate_region_county(kings, central_valley).
climate_region_county(tulare, central_valley).
climate_region_county(kern, central_valley).
climate_region_county(imperial, desert).
climate_region_county(inyo, desert).
climate_region_county(alpine, mountain).
climate_region_county(sierra, mountain).
climate_region_county(plumas, mountain).
climate_region_county(lassen, mountain).
climate_region_county(modoc, mountain).
climate_region_county(mono, mountain).
climate_region_county(trinity, mountain).
climate_region_county(siskiyou, mountain).
climate_region_county(el_dorado, foothill).
climate_region_county(placer, foothill).
climate_region_county(nevada, foothill).
climate_region_county(amador, foothill).
climate_region_county(calaveras, foothill).
climate_region_county(tuolumne, foothill).
climate_region_county(mariposa, foothill).
climate_region_county(lake, foothill).

climate_region(County, City, Region) :- climate_region_city(City, County, Region), !.
climate_region(County, _City, Region) :- climate_region_county(County, Region), !.
climate_region(_County, _City, unknown).

% --- Ordinal value -> integer rank ---
value_rank(very_low, 0).
value_rank(low, 1).
value_rank(low_moderate, 2).
value_rank(moderate, 3).
value_rank(moderate_high, 4).
value_rank(high, 5).
value_rank(very_high, 6).

% --- feature_value(Region, Feature, Ordinal) ---
feature_value(cool_coast, heating, high).
feature_value(cool_coast, ac, low_moderate).
feature_value(cool_coast, swamp_cooler, very_low).
feature_value(cool_coast, whole_house_fan, low).
feature_value(cool_coast, filtration, moderate).
feature_value(warm_coast, heating, moderate).
feature_value(warm_coast, ac, moderate_high).
feature_value(warm_coast, swamp_cooler, low).
feature_value(warm_coast, whole_house_fan, low_moderate).
feature_value(warm_coast, filtration, moderate).
feature_value(inland_valley, heating, moderate_high).
feature_value(inland_valley, ac, high).
feature_value(inland_valley, swamp_cooler, low_moderate).
feature_value(inland_valley, whole_house_fan, moderate).
feature_value(inland_valley, filtration, high).
feature_value(central_valley, heating, moderate_high).
feature_value(central_valley, ac, very_high).
feature_value(central_valley, swamp_cooler, moderate).
feature_value(central_valley, whole_house_fan, moderate).
feature_value(central_valley, filtration, high).
feature_value(desert, heating, moderate_high).
feature_value(desert, ac, very_high).
feature_value(desert, swamp_cooler, moderate).
feature_value(desert, whole_house_fan, low_moderate).
feature_value(desert, filtration, moderate).
feature_value(mountain, heating, very_high).
feature_value(mountain, ac, low_moderate).
feature_value(mountain, swamp_cooler, very_low).
feature_value(mountain, whole_house_fan, low).
feature_value(mountain, filtration, moderate_high).
feature_value(foothill, heating, high).
feature_value(foothill, ac, high).
feature_value(foothill, swamp_cooler, low_moderate).
feature_value(foothill, whole_house_fan, moderate).
feature_value(foothill, filtration, high).

region_feature_rank(County, City, Feature, Rank) :-
    climate_region(County, City, Region), Region \== unknown,
    feature_value(Region, Feature, Ord), value_rank(Ord, Rank).

