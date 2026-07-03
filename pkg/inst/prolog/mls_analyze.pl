/* IO helper for batch-running the MLS remarks DCG on raw file contents.
   Reading the remark from a file avoids any Prolog-atom escaping of quotes,
   newlines or Unicode. Consult AFTER grammar/mls_remarks.pl (it references
   analyze_remarks/2). */

read_all_chars(S, Cs) :-
    get_char(S, C),
    ( C == end_of_file -> Cs = [] ; Cs = [C|T], read_all_chars(S, T) ).

file_to_atom(File, Atom) :-
    open(File, read, S),
    read_all_chars(S, Cs),
    close(S),
    atom_chars(Atom, Cs).

analyze_file(File, Pairs) :-
    file_to_atom(File, Atom),
    analyze_remarks(Atom, Pairs).
