/* MLS remarks feature-extraction DCG (vProlog / Trealla).

   A Prolog/DCG port of ValEngr-wf/python/exploratory/remarks_analyzer.py
   (RuleBasedExtractor). Mirrors that extractor's schema so output stays
   compatible with the valuation models: graded condition (1-5), views,
   kitchen/bath remodel flags, hardwood, upgrades+count, garage spaces, pool,
   fireplace+count, location benefits, highlights/concerns and a sentiment
   score in [-1, 1].

   Entry point:
     analyze_remarks(+RemarksAtom, -Features)
   where Features is a sorted Key-Value list, e.g.
     [ condition-good, condition_score-4, has_view-true, views-[ocean], ... ]

   Approach: tokenize to lowercased word atoms, then collect every feature that
   matches at any position (findall over feat//1 at all suffixes) — independent,
   overlapping matches like the original's per-category regex passes — and
   aggregate into the schema. */

:- use_module(library(dcgs)).
:- use_module(library(lists)).

/* ---------- entry point ---------- */

analyze_remarks(Remarks, Features) :-
    tokenize(Remarks, Toks),
    findall(F, match_at(F, Toks), Raw),
    aggregate(Raw, Features).

match_at(F, Toks) :-
    append(_, Suffix, Toks),
    Suffix \== [],
    phrase(feat(F), Suffix, _).   % match a feature at the start of Suffix

/* ---------- tokenizer: atom -> [lowercased word atoms] ---------- */

tokenize(Atom, Tokens) :-
    atom_chars(Atom, Chars),
    phrase(toks(Tokens), Chars).

/* number tokens (digit runs, thousands-commas stripped, decimals kept) are
   split from letter/CJK word tokens; '$' and '%' are kept as their own tokens;
   everything else is a separator. Numbers stay as atoms (num_word/num_val
   convert them), so existing rules and tests are unaffected. */
toks([N|Ts]) --> number_lexeme(L), !, { exclude(==(','), L, C), atom_chars(N, C) }, toks(Ts).
toks([T|Ts]) --> word_chars(Wc), { Wc \== [] }, !, { atom_chars(T, Wc) }, toks(Ts).
toks([P|Ts]) --> [P], { sig_punct(P) }, !, toks(Ts).
toks(['.'|Ts]) --> [C], { boundary_char(C) }, !, toks(Ts).  % clause boundary marker
toks(Ts)     --> [_], !, toks(Ts).
toks([])     --> [].

number_lexeme([D|R]) --> [D], { is_digit(D) }, num_rest(R).   % must start with a digit
num_rest([D|T])   --> [D], { is_digit(D) }, !, num_rest(T).
num_rest([S,D|T]) --> [S], { S == (',') ; S == ('.') }, [D], { is_digit(D) }, !, num_rest(T).
num_rest([])      --> [].

word_chars([L|Cs]) --> [C], { is_wordchar(C), to_lower(C, L) }, !, word_chars(Cs).
word_chars([])     --> [].

sig_punct('$'). sig_punct('%').
boundary_char('.'). boundary_char(';'). boundary_char('!'). boundary_char('?').
is_digit(C)    :- char_code(C, X), X >= 0'0, X =< 0'9.
is_wordchar(C) :- char_code(C, X), ( X >= 0'a, X =< 0'z ; X >= 0'A, X =< 0'Z ; X > 127 ), !. % letters + CJK, not digits

to_lower(C, L) :-
    char_code(C, X),
    ( X >= 0'A, X =< 0'Z -> Y is X + 32, char_code(L, Y) ; L = C ).

num_word(W, N) :- catch(atom_number(W, N), _, fail), integer(N), !.
num_word(one, 1).   num_word(two, 2).   num_word(three, 3).
num_word(four, 4).  num_word(five, 5).  num_word(six, 6).

/* num_val also accepts decimals (e.g. '1.2'); for price magnitudes. */
num_val(W, N) :- catch(atom_number(W, N), _, fail), !.
num_val(W, N) :- num_word(W, N).
mag(m, 1000000).  mag(million, 1000000).  mag(k, 1000).  mag(thousand, 1000).
to_int(V, V) :- integer(V), !.
to_int(V, P) :- P is round(V).

/* ---------- feature phrases (feat//1) ---------- */

/* Condition tiers */
feat(cond(excellent)) --> [W], { member(W, [immaculate,pristine,impeccable,excellent,
                                            perfect,mint,spotless,turnkey,meticulously]) }.
feat(cond(excellent)) --> [move,in,ready].
feat(cond(excellent)) --> [like,new].
feat(cond(good)) --> [W], { member(W, [updated,upgraded,remodeled,renovated,refreshed,
                                        improved,modernized]) }.
feat(cond(good)) --> [well,maintained].
feat(cond(good)) --> [good,condition].
feat(cond(average)) --> [W], { member(W, [average,fair,adequate,functional,livable,solid]) }.
feat(cond(poor)) --> [W], { member(W, [fixer,tlc,investor,handyman,deferred,opportunity,potential]) }.
feat(cond(poor)) --> [needs,work].
feat(cond(poor)) --> [as,is].
feat(cond(poor)) --> [estate,sale].

/* Views */
feat(view(T)) --> [T,view], { member(T, [bay,ocean,water,city,mountain,hill,valley,golf,park,garden]) }.
feat(view(any)) --> [W], { member(W, [panoramic,vista,scenic]) }.
feat(view(any)) --> [V,P], { member(V, [view,views]), member(P, [of,toward,to]) }.  % "views of X"

/* Kitchen */
feat(kitchen_remodeled) --> [M,kitchen], { member(M, [new,updated,remodeled,renovated,modern]) }.
feat(kitchen_remodeled) --> [kitchen,M], { member(M, [new,updated,remodeled,renovated]) }.
feat(kitchen(F)) --> [F], { member(F, [granite,quartz,marble,stainless,island,gourmet,
                                        cabinets,countertops,appliances]) }.

/* Bathrooms */
feat(bath_remodeled) --> [M,Bath], { member(M, [new,updated,remodeled,renovated]),
                                     member(Bath, [bath,bathroom,baths,bathrooms]) }.
feat(bath_remodeled) --> [Bath,M], { member(Bath, [bath,bathroom,baths,bathrooms]),
                                     member(M, [new,updated,remodeled,renovated]) }.
feat(bath(F)) --> [F], { member(F, [jacuzzi,jetted]) }.
feat(bath(soaking_tub)) --> [soaking,tub].
feat(bath(dual_vanity)) --> [dual,vanity].
feat(bath(double_vanity)) --> [double,vanity].

/* Flooring */
feat(hardwood) --> [hardwood].
feat(hardwood) --> [W], { member(W, [oak,maple,refinished]) }.
feat(hardwood) --> [wood,Floor], { member(Floor, [floor,floors,flooring]) }.
feat(flooring(F)) --> [F], { member(F, [tile,travertine,carpet,laminate,vinyl,bamboo]) }.

/* Upgrades */
feat(upgrade(new_roof))   --> [new,roof].
feat(upgrade(new_hvac))   --> [new,hvac].
feat(upgrade(new_ac))     --> [new,ac].
feat(upgrade(new_furnace))--> [new,furnace].
feat(upgrade(dual_pane))  --> [Dp,pane], { member(Dp, [dual,double]) }.
feat(upgrade(new_windows))--> [new,W], { member(W, [window,windows]) }.
feat(upgrade(solar))      --> [solar].
feat(upgrade(tankless))   --> [tankless].
feat(upgrade(smart_home)) --> [smart,home].
feat(upgrade(recessed_lighting)) --> [recessed,L], { member(L, [light,lights,lighting]) }.
feat(upgrade(crown_molding)) --> [crown,M], { member(M, [mold,molding,moldings]) }.
feat(upgrade(W)) --> [W], { member(W, [custom,upgraded,renovation,addition]) }.

/* Price: a $amount is only kept when tied to a SUBJECT, so the output column
   is price_<subject> (context-free amounts are dropped — they're meaningless).
   money//1 detects "$1.2M / $575K / $850,000 / 1.2 million". */
money(P) --> ['$',N,Mag], { num_val(N, V), mag(Mag, Mult), W is V*Mult, to_int(W, P) }.
money(P) --> ['$',N],     { num_val(N, V), to_int(V, P) }.
money(P) --> [N,Mag],     { num_val(N, V), mag(Mag, Mult), W is V*Mult, to_int(W, P) }.

/* Subject phrases — what a nearby price is OF. */
price_subject(reduction) --> [W], { member(W, [reduced,reduction,lowered,slashed]) }.
price_subject(reduction) --> [price,W], { member(W, [drop,reduction,reduced]) }.
price_subject(hoa)       --> [W], { member(W, [hoa,dues]) }.
price_subject(hoa)       --> [association,W], { member(W, [dues,fee,fees]) }.
price_subject(rent)      --> [W], { member(W, [rent,rents,rental]) }.
price_subject(upgrades)  --> [W], { member(W, [upgrades,improvements,renovations,renovation,upgraded,renovated]) }.
price_subject(mello_roos)--> [mello].
price_subject(mello_roos)--> [special,assessment].
price_subject(mello_roos)--> [cfd].
price_subject(tax)       --> [W], { member(W, [taxes,tax]) }.

/* Solar prices by sense: lease / purchase / balance owed / loan payment. */
price_subject(solar_lease)    --> [solar], gap(2), [W], { member(W, [lease,leased]) }.
price_subject(solar_lease)    --> [W], { member(W, [lease,leased]) }, gap(2), [solar].
price_subject(solar_purchase) --> [solar], gap(2), [W], { member(W, [purchased,bought,paid,purchase,cost]) }.
price_subject(solar_purchase) --> [W], { member(W, [purchased,bought,paid]) }, gap(2), [solar].
price_subject(solar_owed)     --> [solar], gap(2), [W], { member(W, [owed,owe,balance,payoff,remaining]) }.
price_subject(solar_owed)     --> [W], { member(W, [owed,owe,balance,payoff]) }, gap(2), [solar].
price_subject(solar_payment)  --> [solar], gap(2), [W], { member(W, [loan,financed,payment,payments]) }.
price_subject(solar_payment)  --> [W], { member(W, [loan,financed]) }, gap(2), [solar].

/* A price tagged with its subject (subject and amount in the same clause,
   within 2 tokens — pgap won't cross a sentence boundary). */
feat(price(S, P)) --> price_subject(S), pgap(2), money(P).
feat(price(S, P)) --> money(P), pgap(2), price_subject(S).

/* Square footage: "1,850 sqft" / "1850 sq ft" / "2,153 square feet" / "875 sf" */
feat(sqft(N)) --> [W,sq,ft],      { num_word(W, N) }.
feat(sqft(N)) --> [W,square,feet], { num_word(W, N) }.
feat(sqft(N)) --> [W,square,foot], { num_word(W, N) }.
feat(sqft(N)) --> [W,U], { member(U, [sqft,sf]), num_word(W, N) }.

/* Year built: "built 1998" / "built in 1962" */
feat(year_built(Y)) --> [built,in,W], { num_word(W, Y), Y > 1700, Y < 2100 }.
feat(year_built(Y)) --> [built,W],    { num_word(W, Y), Y > 1700, Y < 2100 }.

/* Parking / garage */
feat(garage_spaces(N)) --> [D,car,garage], { num_word(D, N), N >= 1, N =< 9 }.
feat(garage_spaces(N)) --> [D,car], { num_word(D, N), N >= 1, N =< 9 }.
feat(parking(attached)) --> [attached,garage].
feat(parking(detached)) --> [detached,garage].
feat(parking(carport))  --> [carport].

/* ---------- Outbuildings / extra structures ----------
   Outbuildings are sheds, barns, workshops and "extra garages", plus cabana /
   casita / guest house / cottage / kennel / dog run / storage building. Size
   is captured when a dimension ("40x50") or "N sq ft" sits within a few tokens
   of the structure word. */
struct(shed)             --> [shed].
struct(shed)             --> [sheds].
struct(barn)             --> [barn].
struct(barn)             --> [barns].
struct(workshop)         --> [workshop].
struct(workshop)         --> [workshops].
struct(workshop)         --> [work,shop].
struct(extra_garage)     --> [extra,garage].
struct(extra_garage)     --> [extra,garages].
struct(extra_garage)     --> [additional,garage].
struct(extra_garage)     --> [detached,garage].
struct(extra_garage)     --> [rv,garage].
struct(cabana)           --> [cabana].
struct(casita)           --> [casita].
struct(casita)           --> [casitas].
struct(guest_house)      --> [guest,house].
struct(guest_house)      --> [guest,quarters].
struct(guest_house)      --> [in,law,suite].
struct(cottage)          --> [cottage].
struct(kennel)           --> [kennel].
struct(kennel)           --> [kennels].
struct(dog_run)          --> [dog,run].
struct(storage_building) --> [storage,building].
struct(storage_building) --> [storage,facility].
struct(outbuilding)      --> [outbuilding].
struct(outbuilding)      --> [out,building].

feat(outbuilding(T)) --> struct(T).

/* bounded token skip, 0..Max */
gap(_)   --> [].
gap(Max) --> { Max > 0, M1 is Max - 1 }, [_], gap(M1).

/* like gap, but never crosses a clause boundary ('.') — used for price/subject
   association so amounts don't leak across sentences. */
pgap(_)   --> [].
pgap(Max) --> { Max > 0, M1 is Max - 1 }, [T], { T \== '.' }, pgap(M1).

/* dimension "WxL" / "W by L" -> area in sq ft (sides 4..300 ft: skips lumber
   like 2x4 and oversized lot dimensions). */
dimen(A) --> [W,x,L],  { num_word(W, V1), num_word(L, V2),
                         V1 >= 4, V1 =< 300, V2 >= 4, V2 =< 300, A is V1*V2 }.
dimen(A) --> [W,by,L], { num_word(W, V1), num_word(L, V2),
                         V1 >= 4, V1 =< 300, V2 >= 4, V2 =< 300, A is V1*V2 }.

feat(outbuilding_size(T,A)) --> struct(T), gap(4), dimen(A).
feat(outbuilding_size(T,A)) --> dimen(A), gap(4), struct(T).
feat(outbuilding_size(T,A)) --> struct(T), gap(4), [N,sq,ft], { num_word(N, A) }.
feat(outbuilding_size(T,A)) --> [N,sq,ft], gap(4), struct(T), { num_word(N, A) }.
feat(outbuilding_size(T,A)) --> struct(T), gap(4), [N,sqft],  { num_word(N, A) }.

/* ---------- Garage: attributed area + epoxy. Living area EXCLUDES the garage,
   so garage sqft is separate contributory utility and must NOT leak into the
   home sqft. Car bays come via garage_spaces (e.g. "three-car"). ---------- */
feat(garage_size(A)) --> [garage], gap(4), [N,sqft],       { num_word(N, A) }.
feat(garage_size(A)) --> [N,sqft], gap(4), [garage],       { num_word(N, A) }.
feat(garage_size(A)) --> [garage], gap(4), [N,sq,ft],      { num_word(N, A) }.
feat(garage_size(A)) --> [N,sq,ft], gap(4), [garage],      { num_word(N, A) }.
feat(garage_size(A)) --> [N,square,feet], gap(4), [garage], { num_word(N, A) }.
feat(garage_size(A)) --> [garage], gap(4), dimen(A).
feat(garage_size(A)) --> dimen(A), gap(4), [garage].
feat(epoxy) --> [epoxy].

/* ---------- Lot size: living area EXCLUDES the lot. "N sqft lot" /
   "lot ... N sqft" / "N acre(s)" -> lot_size in SQFT (acres x 43560). Kept out
   of the home sqft. ---------- */
feat(lot_size(A)) --> [N,sqft],        gap(2), lot_word, { num_word(N, A) }.
feat(lot_size(A)) --> [N,sq,ft],       gap(2), lot_word, { num_word(N, A) }.
feat(lot_size(A)) --> [N,square,feet], gap(2), lot_word, { num_word(N, A) }.
feat(lot_size(A)) --> lot_word, gap(4), [N,sqft],       { num_word(N, A) }.
feat(lot_size(A)) --> lot_word, gap(4), [N,sq,ft],      { num_word(N, A) }.
feat(lot_size(A)) --> [N,W], { member(W, [acre,acres]), num_val(N, V), A is round(V*43560.0) }.
lot_word --> [W], { member(W, [lot, parcel, homesite, acreage]) }.

/* ---------- RV parking / storage (a strong cue in rural/desert markets) ---------- */
feat(rv(parking)) --> [rv,parking].
feat(rv(parking)) --> [rv,park].
feat(rv(parking)) --> [rv,pad].
feat(rv(garage))  --> [rv,garage].
feat(rv(access))  --> [rv,access].
feat(rv(storage)) --> [rv,storage].
feat(rv(carport)) --> [rv,carport].
feat(rv(hookups)) --> [rv,W], { member(W, [hookup,hookups,hook]) }.
feat(rv(boat))    --> [rv,boat].
feat(rv(any))     --> [rv].
feat(rv_spaces(N))--> [rv,spaces,W], { num_word(W, N) }.   % "# of rv spaces: N"

/* ---------- Solar storage / batteries + solar ownership ---------- */
feat(battery) --> [battery].
feat(battery) --> [batteries].
feat(battery) --> [powerwall].
feat(battery) --> [backup,battery].
feat(battery) --> [battery,backup].
feat(battery) --> [solar,W], { member(W, [battery,batteries,storage]) }.
feat(solar_owned)  --> [W,solar], { member(W, [owned,paid]) }.
feat(solar_owned)  --> [paid,off,solar].
feat(solar_owned)  --> [solar,is,paid].
feat(solar_owned)  --> [solar,W], { member(W, [owned,paid]) }.
feat(solar_leased) --> [leased,solar].
feat(solar_leased) --> [solar,W], { member(W, [lease,leased]) }.

/* Outdoor / pool */
feat(pool) --> [pool].
feat(pool) --> [swimming,pool].
feat(outdoor(hot_tub)) --> [hot,tub].
feat(outdoor(W)) --> [W], { member(W, [spa,deck,patio,yard,garden,landscaped,pergola,
                                        gazebo,fountain,orchard,bbq,firepit]) }.

/* Fireplace */
feat(fireplace(N)) --> [D,Fp], { member(Fp, [fireplace,fireplaces]), num_word(D, N) }.
feat(fireplace(1)) --> [Fp], { member(Fp, [fireplace,fireplaces,firplace,hearth,mantle,mantel]) }.
feat(fireplace(1)) --> [fire,place].
feat(fireplace(1)) --> [B,burning], { member(B, [wood,gas]) }.

/* Location benefits */
feat(location(W)) --> [W], { member(W, [downtown,transit,bart,caltrain,bus,school,schools,
                                        shopping,restaurant,restaurants,freeway,highway,parks]) }.
feat(location(near)) --> [W], { member(W, [near,close,walk,minutes,steps]) }.

/* Sentiment: highlights (positive) and concerns (negative).
   Lexicons aligned to remarks_analyzer.py HIGHLIGHTS / CONCERNS for parity. */
feat(highlight(W)) --> [W], { member(W, [stunning,gorgeous,beautiful,amazing,spectacular,
                                         exceptional,remarkable,fabulous,fantastic,wonderful,
                                         charming,elegant,luxurious,luxury,premium,designer,
                                         custom,rare,unique,wow]) }.
feat(highlight(one_of_a_kind)) --> [one,of,a,kind].
feat(highlight(must_see))      --> [must,see].
feat(concern(W)) --> [W], { member(W, [dated,older,original,deferred,repair,project,probate,
                                       foreclosure,reo,needs,requires,maintenance,work,
                                       opportunity,potential,investor,handyman,estate]) }.
feat(concern(as_is))      --> [as,is].
feat(concern(short_sale)) --> [short,sale].

/* Functional utility (layout / usability) — feeds the functional_utility latent */
feat(functional(open_floor_plan)) --> [open,floor,plan].
feat(functional(open_concept))    --> [open,concept].
feat(functional(single_level))    --> [single,level].
feat(functional(single_story))    --> [single,story].
feat(functional(great_room))      --> [great,room].
feat(functional(bonus_room))      --> [bonus,room].
feat(functional(W)) --> [W], { member(W, [office,den,study,spacious,flowing,flexible,versatile]) }.
feat(functional_neg(W)) --> [W], { member(W, [awkward,choppy,cramped,obsolete]) }.

/* Water / distance-to-water — feeds the water latent. Broadened after the
   Helendale calibration showed "open water" (+15.4/SF) is the strongest single
   cue and was being missed. */
feat(water(W)) --> [W], { member(W, [waterfront,lakefront,oceanfront,beachfront,
                                     riverfront,bayfront,lakeside]) }.
feat(water(open_water)) --> [open,water].
feat(water(on_water))   --> [on,the,W], { member(W, [water,lake]) }.
feat(water(F)) --> [Body,F], { member(Body, [water,lake,ocean,bay,river,creek,canal]),
                               member(F, [front,frontage,access,view,views]) }.
feat(water(dock)) --> [W], { member(W, [dock,pier]) }.
feat(water(steps)) --> [steps,to,W], { member(W, [beach,water,sand,shore]) }.

/* ---------- aggregation into the schema ---------- */

aggregate(Raw, Out) :-
    pick_condition(Raw, Cond, Score),
    bool(member(view(_), Raw), HasView),
    findall(V, member(view(V), Raw), Vs0), exclude(=(any), Vs0, Vs1), sort(Vs1, Views),
    bool(member(kitchen_remodeled, Raw), KR),
    bool(member(bath_remodeled, Raw), BR),
    bool(member(hardwood, Raw), HW),
    findall(U, member(upgrade(U), Raw), Us0), sort(Us0, Upgrades), length(Upgrades, UC),
    ( findall(G, member(garage_spaces(G), Raw), Gs), Gs \== []
      -> mls_max(Gs, Garage) ; Garage = 0 ),
    bool(member(pool, Raw), Pool),
    ( member(fireplace(_), Raw)
      -> HasFp = true, findall(N, member(fireplace(N), Raw), Fns), mls_max(Fns, FpC)
      ;  HasFp = false, FpC = 0 ),
    findall(L, member(location(L), Raw), Ls0), sort(Ls0, Location),
    ( setof(PS, PP^member(price(PS,PP), Raw), PriceSubjs) -> true ; PriceSubjs = [] ),
    findall(PKey-PMax, ( member(PSj, PriceSubjs),
                         findall(PPv, member(price(PSj,PPv), Raw), PPs),
                         mls_max(PPs, PMax), atom_concat(price_, PSj, PKey) ),
            PriceFeats),
    findall(GA, member(garage_size(GA), Raw), GarageAreas),
    findall(OA, member(outbuilding_size(_,OA), Raw), OutAreas),
    findall(LS, member(lot_size(LS), Raw), LotSizes),
    ( findall(Sq, member(sqft(Sq), Raw), Sqs0),
      append(GarageAreas, OutAreas, A1), append(A1, LotSizes, AttrAreas),
      subtract(Sqs0, AttrAreas, Sqs),   % home sqft excludes garage/outbuilding/lot areas
      Sqs \== [] -> mls_max(Sqs, Sqft) ; Sqft = 0 ),
    ( LotSizes \== [] -> mls_max(LotSizes, LotSize) ; LotSize = 0 ),
    ( GarageAreas \== [] -> mls_max(GarageAreas, GarageArea) ; GarageArea = 0 ),
    findall(WA, member(outbuilding_size(workshop,WA), Raw), WShopAreas),
    ( WShopAreas \== [] -> mls_max(WShopAreas, WorkshopArea) ; WorkshopArea = 0 ),
    bool(member(epoxy, Raw), Epoxy),
    ( findall(Yr, member(year_built(Yr), Raw), Yrs), Yrs \== [] -> mls_max(Yrs, Year) ; Year = 0 ),
    findall(H, member(highlight(H), Raw), Hs0), sort(Hs0, Highlights), length(Highlights, HC),
    findall(C, member(concern(C), Raw), Cs0), sort(Cs0, Concerns), length(Concerns, CC),
    ( HC + CC =:= 0 -> Sent = 0.0 ; Sent is (HC - CC) / (HC + CC) ),
    findall(Ob, member(outbuilding(Ob), Raw), Obs0), sort(Obs0, Outbuildings),
    bool(member(outbuilding(shed), Raw),         HasShed),
    bool(member(outbuilding(barn), Raw),         HasBarn),
    bool(member(outbuilding(workshop), Raw),     HasWorkshop),
    bool(member(outbuilding(extra_garage), Raw), HasExtraGarage),
    bool(member(outbuilding(casita), Raw),       HasCasita),
    bool(member(outbuilding(guest_house), Raw),  HasGuestHouse),
    bool(member(outbuilding(cabana), Raw),       HasCabana),
    bool(member(outbuilding(cottage), Raw),      HasCottage),
    bool(member(outbuilding(kennel), Raw),       HasKennel),
    bool(member(outbuilding(dog_run), Raw),      HasDogRun),
    ( findall(A, member(outbuilding_size(_,A), Raw), As), As \== []
      -> mls_max(As, OutSqft) ; OutSqft = 0 ),
    findall(Rv, ( member(rv(Rv), Raw), Rv \== any ), Rvs0), sort(Rvs0, RvFeatures),
    bool(member(rv(_), Raw), HasRv),
    ( findall(RS, member(rv_spaces(RS), Raw), RSs), RSs \== []
      -> mls_max(RSs, RvSpaces) ; RvSpaces = 0 ),
    bool(( member(upgrade(solar), Raw) ; member(solar_owned, Raw)
         ; member(solar_leased, Raw) ), HasSolar),
    bool(member(solar_owned, Raw),  SolarOwned),
    bool(member(solar_leased, Raw), SolarLeased),
    bool(member(battery, Raw),      HasBattery),
    Out0 = [ condition-Cond, condition_score-Score,
            has_view-HasView, views-Views,
            kitchen_remodeled-KR, bath_remodeled-BR, has_hardwood-HW,
            upgrades-Upgrades, upgrade_count-UC,
            garage_spaces-Garage, garage_area-GarageArea, has_epoxy-Epoxy,
            has_pool-Pool,
            has_fireplace-HasFp, fireplace_count-FpC,
            sqft-Sqft, lot_size-LotSize, year_built-Year,
            location-Location,
            outbuildings-Outbuildings, outbuilding_sqft-OutSqft,
            workshop_area-WorkshopArea,
            has_shed-HasShed, has_barn-HasBarn, has_workshop-HasWorkshop,
            has_extra_garage-HasExtraGarage, has_casita-HasCasita,
            has_guest_house-HasGuestHouse, has_cabana-HasCabana,
            has_cottage-HasCottage, has_kennel-HasKennel, has_dog_run-HasDogRun,
            has_rv-HasRv, rv_features-RvFeatures, rv_spaces-RvSpaces,
            has_solar-HasSolar, solar_owned-SolarOwned, solar_leased-SolarLeased,
            has_battery-HasBattery,
            highlights-Highlights, concerns-Concerns,
            sentiment_score-Sent ],
    append(Out0, PriceFeats, Out).

pick_condition(Raw, excellent, 5)  :- member(cond(excellent), Raw), !.
pick_condition(Raw, good, 4)       :- member(cond(good), Raw), !.
pick_condition(Raw, average, 3)    :- member(cond(average), Raw), !.
pick_condition(Raw, needs_work, 2) :- member(cond(poor), Raw), !.
pick_condition(_, average, 3).     /* default when nothing matches */

bool(Goal, true)  :- call(Goal), !.
bool(_, false).

mls_max([X|Xs], M) :- foldl_max(Xs, X, M).
foldl_max([], M, M).
foldl_max([X|Xs], A, M) :- ( X > A -> A1 = X ; A1 = A ), foldl_max(Xs, A1, M).

/* Convenience: fetch one field from the Features list. */
feature(Features, Key, Value) :- memberchk(Key-Value, Features).

/* ============================================================
   PER-LATENT CUE GROUPS  (TrilogyWF design §8.5 / §8.6)

   analyze_latents(+Remarks, -Profile) maps the detected cues onto the
   appraiser's latent variables and returns, per latent:
     latent(Name, Score, Evidence, Cues)
   - Score    : net directional signal  (#positive cues - #negative cues)
   - Evidence : total distinct cues fired = the shrinkage weight `w`
   - Cues     : signed tags (+tag / -tag) for traceable explanations
   Latents with no remark evidence return latent(Name, 0, 0, []).
   ============================================================ */

latents([condition, quality, view, functional_utility,
         yard, exterior_env, water, location]).

/* feat_latent(+FeatTerm, -Latent, -Polarity, -Tag) — the cue→latent map.
   Condition negatives use the cond(poor) tier plus concern atoms NOT already
   covered by that tier, to avoid double-counting. */
feat_latent(cond(excellent),    condition, pos, excellent_condition).
feat_latent(cond(good),         condition, pos, good_condition).
feat_latent(cond(poor),         condition, neg, poor_condition).
feat_latent(kitchen_remodeled,  condition, pos, kitchen_remodeled).
feat_latent(bath_remodeled,     condition, pos, bath_remodeled).
feat_latent(upgrade(U),         condition, pos, U).
feat_latent(concern(W),         condition, neg, W) :- member(W, [dated,older,original,repair,maintenance]).

feat_latent(kitchen(F),         quality, pos, F).
feat_latent(bath(F),            quality, pos, F).
feat_latent(hardwood,           quality, pos, hardwood).
feat_latent(flooring(F),        quality, pos, F).
feat_latent(fireplace(_),       quality, pos, fireplace).

feat_latent(view(_),            view, pos, view).

feat_latent(functional(F),      functional_utility, pos, F).
feat_latent(functional_neg(F),  functional_utility, neg, F).

feat_latent(outdoor(W),         yard, pos, W) :- member(W, [yard,garden,landscaped,orchard]).
feat_latent(outdoor(W),         exterior_env, pos, W) :-
    member(W, [spa,deck,patio,pergola,gazebo,fountain,bbq,firepit,hot_tub]).
feat_latent(pool,               exterior_env, pos, pool).

feat_latent(water(F),           water, pos, F).

feat_latent(location(L),        location, pos, L).

analyze_latents(Remarks, Profile) :-
    tokenize(Remarks, Toks),
    findall(L-P-T, ( match_at(F, Toks), feat_latent(F, L, P, T) ), Mapped0),
    sort(Mapped0, Mapped),                 % distinct (latent,polarity,tag) triples
    latents(Ls),
    maplist(latent_summary(Mapped), Ls, Profile).

latent_summary(Mapped, L, latent(L, Score, Evidence, Cues)) :-
    findall(P-T, member(L-P-T, Mapped), PTs),
    length(PTs, Evidence),
    findall(x, member(pos-_, PTs), Pos), length(Pos, NP),
    findall(x, member(neg-_, PTs), Neg), length(Neg, NN),
    Score is NP - NN,
    findall(S, ( member(P-T, PTs), signed(P, T, S) ), Cues).

signed(pos, T, +T).
signed(neg, T, -T).

/* latent_field(+Profile, +Latent, -latent(Name,Score,Evidence,Cues)) */
latent_field(Profile, L, latent(L,S,E,C)) :- memberchk(latent(L,S,E,C), Profile).
