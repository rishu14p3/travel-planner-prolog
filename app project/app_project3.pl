:- dynamic site/5.

% site(Name, [Interests], City, Cost, DistanceFromChennai).
site(brihadeeswara_temple, [architecture, temples, unesco], thanjavur, 2000, 300).
site(meenakshi_temple, [architecture, temples, festivals], madurai, 1500, 450).
site(kailasanathar_temple, [architecture, temples], kanchipuram, 1800, 70).
site(ajanta_caves, [art, unesco], aurangabad, 5000, 1100).
site(national_museum, [museums, art], delhi, 1000, 2200).
site(khajuraho_temples, [architecture, temples, unesco, art], khajuraho, 4000, 1500).
site(sun_temple, [architecture, unesco], konark, 3500, 1200).
site(victoria_memorial, [architecture, art, museums], kolkata, 2500, 1650).
site(pushkar_fair, [festivals, culture], pushkar, 3000, 1600).

% ---------------------------
% SPEED ASSUMPTIONS (km/h)
% ---------------------------
speed(car, 60).
speed(train, 80).
speed(flight, 500).

% ---------------------------
% MAX DISTANCE ALLOWED PER MODE & DAYS
% ---------------------------
max_distance(car, Days, MaxDist) :- MaxDist is Days * 300.
max_distance(train, Days, MaxDist) :- MaxDist is Days * 700.
max_distance(flight, _, 99999).   % flights allow anywhere

% ---------------------------
% RECOMMENDATION RULE
% ---------------------------
% recommend(+City,+Days,+Budget,+TravelMode,+Interests,-Result)
% Result = [Name-DistanceWithUnit-TimeWithUnit]
recommend(_, Days, Budget, TravelMode, Interests, Result) :-
    max_distance(TravelMode, Days, MaxDist),
    speed(TravelMode, Speed),
    findall(Name-DistStr-TimeStr,
        ( site(Name, Tags, _, Cost, Dist),
          Cost =< Budget,
          Dist =< MaxDist,
          has_common_interest(Interests, Tags),
          Time is Dist / Speed,
          format(atom(DistStr), '~w km', [Dist]),
          format(atom(TimeStr), '~2f h', [Time])
        ),
    Matches),
    list_to_set(Matches, Result).

% ---------------------------
% HELPER: Check if lists overlap
% ---------------------------
has_common_interest([H|_], Tags) :- member(H, Tags), !.
has_common_interest([_|T], Tags) :- has_common_interest(T, Tags).
