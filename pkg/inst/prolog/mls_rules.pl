/* Harness for user-scripted feature-derivation rules.
 *
 * For each data row, earthUI asserts the row's columns as cell(Name, Value)
 * facts (Name an atom; Value is true/false, a number, or an atom) and then
 * collects every solution of the user's derive(Col, Val) rules. The FIRST
 * value found for a given Col wins, so a fallback clause may follow, e.g.
 *
 *     derive(turnkey, true)  :- cell(pr_condition_score, S), S >= 4.
 *     derive(turnkey, false).
 *
 * Count helpers (operate over the current row's cell/2 facts) are provided
 * below so rules can aggregate flags into numeric features.
 */
:- use_module(library(lists)).
:- dynamic(cell/2).
:- dynamic(drop/1).   % drop(Col) facts in the rules trash that column

/* count_true(+Cols, -N)  / count_false(+Cols, -N)
   N = how many of the named columns are true / false this row. */
count_true(Cols, N) :-
    findall(x, ( member(C, Cols), cell(C, true) ), L), length(L, N).
count_false(Cols, N) :-
    findall(x, ( member(C, Cols), cell(C, false) ), L), length(L, N).

/* count_true_prefix(+Prefix, -N) / count_false_prefix(+Prefix, -N)
   N = how many cells whose NAME starts with Prefix are true / false, e.g.
   count_true_prefix(pr_has_, N) counts every true pr_has_* flag. */
count_true_prefix(Prefix, N) :-
    findall(x, ( cell(Name, true),  atom_concat(Prefix, _, Name) ), L), length(L, N).
count_false_prefix(Prefix, N) :-
    findall(x, ( cell(Name, false), atom_concat(Prefix, _, Name) ), L), length(L, N).

/* NA handling: a value that was NA in the data is asserted as NO cell/2 fact,
   so cell(Col, _) simply fails for it.
   cell_or(+Col, +Default, -Value) — the cell's value, or Default if it was NA.
   is_na(+Col) — true when Col had no value (was NA) this row. */
cell_or(C, _, V) :- cell(C, V), !.
cell_or(_, D, D).
is_na(C) :- \+ cell(C, _).
