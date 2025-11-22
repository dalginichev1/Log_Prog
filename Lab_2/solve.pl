solve :-
    People = [voronov, golubev, grachev, kanareykin, chaykin, popugayev, skvortsov],
    Birds = [voron, golub, grach, kanareyka, chayka, popugay, skvorets],
    
    permutation(Birds, AssignedBirds),
    pairs(People, AssignedBirds, Pairs),

    condition1(Pairs),      
    condition2(Pairs),      
    condition3(Pairs),      
    condition4(Pairs),      
    condition5(Pairs),      
    condition6(Pairs),      

    format('Решение найдено:~n'),
    print_pairs(Pairs),
    find_skvorets_owner(Pairs, Owner),
    format('~nСкворец принадлежит: '),
    print_name(Owner).

pairs([], [], []).
pairs([P|People], [B|Birds], [(P,B)|Pairs]) :- 
    pairs(People, Birds, Pairs).

тезка(voronov, voron).
тезка(golubev, golub).
тезка(grachev, grach).
тезка(kanareykin, kanareyka).
тезка(chaykin, chayka).
тезка(popugayev, popugay).
тезка(skvortsov, skvorets).

тезка_птицы(voron, voronov).
тезка_птицы(golub, golubev).
тезка_птицы(grach, grachev).
тезка_птицы(kanareyka, kanareykin).
тезка_птицы(chayka, chaykin).
тезка_птицы(popugay, popugayev).
тезка_птицы(skvorets, skvortsov).

женат(voronov).
женат(grachev).
женат(chaykin).
женат(popugayev).
женат(skvortsov).
холост(golubev).
холост(kanareykin).

condition1(Pairs) :-
    forall(
        member((Person, _), Pairs),
        (тезка(Person, BirdName),
         member((OtherPerson, BirdName), Pairs),
         Person \= OtherPerson)
    ).

condition2(Pairs) :-
    member((voronov, Bird), Pairs),
    тезка_птицы(Bird, TezkaPerson),
    женат(TezkaPerson).

condition3(Pairs) :-
    member((GrachOwner, grach), Pairs),
    женат(GrachOwner),
    GrachOwner \= chaykin.

condition4(Pairs) :-
    member((VoronOwner, voron), Pairs),
    холост(VoronOwner).

condition5(Pairs) :-
    member((grachev, Bird), Pairs),
    тезка_птицы(Bird, TezkaPerson),
    member((TezkaPerson, kanareyka), Pairs).

condition6(Pairs) :-
    member((PopugayOwner, popugay), Pairs),
    тезка(PopugayOwner, TezkaBird1),          
    member((voronov, VoronovBird), Pairs),
    тезка_птицы(VoronovBird, TezkaPerson2),    
    member((TezkaPerson2, TezkaBird1), Pairs). 

find_skvorets_owner([(Owner, skvorets)|_], Owner) :- !.
find_skvorets_owner([_|Pairs], Owner) :- find_skvorets_owner(Pairs, Owner).

print_name(voronov) :- format('Воронов').
print_name(golubev) :- format('Голубев').
print_name(grachev) :- format('Грачев').
print_name(kanareykin) :- format('Канарейкин').
print_name(chaykin) :- format('Чайкин').
print_name(popugayev) :- format('Попугаев').
print_name(skvortsov) :- format('Скворцов').

print_bird(voron) :- format('ворон').
print_bird(golub) :- format('голубь').
print_bird(grach) :- format('грач').
print_bird(kanareyka) :- format('канарейка').
print_bird(chayka) :- format('чайка').
print_bird(popugay) :- format('попугай').
print_bird(skvorets) :- format('скворец').

print_pairs([]).
print_pairs([(Person,Bird)|Pairs]) :-
    print_name(Person),
    format(' владеет '),
    print_bird(Bird),
    format('~n'),
    print_pairs(Pairs).