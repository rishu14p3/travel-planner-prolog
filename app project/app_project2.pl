:- dynamic site/5.


site(brihadeeswara_temple, [architecture, temples, unesco], thanjavur, 2000, 300).
site(meenakshi_temple, [architecture, temples, festivals], madurai, 1500, 250).
site(kailasanathar_temple, [architecture, temples], kanchipuram, 1800, 100).
site(ajanta_caves, [art, unesco], aurangabad, 5000, 800).
site(national_museum, [museums, art], delhi, 1000, 50).
site(khajuraho_temples, [architecture, temples, unesco, art], khajuraho, 4000, 700).
site(sun_temple, [architecture, unesco], konark, 3500, 600).
site(victoria_memorial, [architecture, art, museums], kolkata, 2500, 400).
site(pushkar_fair, [festivals, culture], pushkar, 3000, 500).

max_distance(car, Days, MaxDist) :- MaxDist is Days * 300.
max_distance(train, Days, MaxDist) :- MaxDist is Days * 700.
max_distance(flight, _, 99999).   % Flights allow anywhere

recommend(Interests, Days, Budget, TravelMode, Result) :-
    max_distance(TravelMode, Days, MaxDist),
    findall(Name-City,
        ( site(Name, Tags, City, Cost, Dist),
          Cost =< Budget,
          Dist =< MaxDist,
          has_common_interest(Interests, Tags)
        ),
    Matches),
    list_to_set(Matches, Result).

has_common_interest([H|_], Tags) :- member(H, Tags), !.
has_common_interest([_|T], Tags) :- has_common_interest(T, Tags).
