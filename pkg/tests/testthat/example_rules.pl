% example_rules.pl - working derivation rules used by test-rules.R.
% (Also a handy reference for the <project>_rules.pl format.)

% merge the collinear solar cluster into one boolean flag
derive(solar_any, B) :-
    ( cell(pr_has_solar, true)
    ; cell(pr_solar_owned, true)
    ; cell(pr_has_battery, true) ) -> B = true ; B = false.

% counts -> numeric columns
derive(solar_count, N)   :- count_true([pr_has_solar, pr_solar_owned, pr_has_battery], N).
derive(amenity_count, N) :- count_true_prefix(pr_has_, N).

% threshold -> boolean (with fallback)
derive(big_outbuilding, true)  :- cell(pr_outbuilding_sqft, S), S >= 1000.
derive(big_outbuilding, false).

% arithmetic -> continuous (real) column
derive(cond_scaled, V) :- cell(pr_condition_score, S), V is S * 1.5.
