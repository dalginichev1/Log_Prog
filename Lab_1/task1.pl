check_builtin_predicates :-
    write('Проверка встроенных предикатов:'), nl,
    (length([1,2,3], L) -> write('length доступен, L='), write(L), nl ; write('length недоступен'), nl),
    (member(2, [1,2,3]) -> write('member доступен'), nl ; write('member недоступен'), nl),
    (append([1,2], [3,4], X) -> write('append доступен, X='), write(X), nl ; write('append недоступен'), nl).

my_length([], 0).
my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.

my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

my_remove(X, [X|T], T).
my_remove(X, [H|T], [H|R]) :- my_remove(X, T, R).

my_permute([], []).
my_permute(L, [X|P]) :- my_remove(X, L, R), my_permute(R, P).

my_sublist(S, L) :- my_append(_, T, L), my_append(S, _, T).

remove_last_std([_], []).
remove_last_std([H|T], [H|R]) :- remove_last_std(T, R).

remove_last_helper([_], Acc, Acc).
remove_last_helper([H|T], Acc, Result) :-
    my_append(Acc, [H], NewAcc),
    remove_last_helper(T, NewAcc, Result).

remove_last_manual(List, Result) :- remove_last_helper(List, [], Result).

is_sorted_std([]).
is_sorted_std([_]).
is_sorted_std([X,Y|T]) :- X =< Y, is_sorted_std([Y|T]).

sorted_helper([], _).
sorted_helper([H|T], Prev) :- 
    number(H),
    Prev =< H,
    sorted_helper(T, H).
sorted_helper([H|T], _) :- sorted_helper(T, H).

is_sorted_manual(List) :- sorted_helper(List, -inf).

test_standard_predicates :-
    write('=== Тестирование стандартных предикатов ==='), nl, nl,
    
    write('1. my_length:'), nl,
    my_length([a,b,c,d], L1), write('Длина [a,b,c,d]: '), write(L1), nl,
    my_length([], L2), write('Длина []: '), write(L2), nl, nl,
    
    write('2. my_member:'), nl,
    (my_member(b, [a,b,c]) -> write('b ∈ [a,b,c] - да'), nl ; write('b ∈ [a,b,c] - нет'), nl),
    (my_member(x, [a,b,c]) -> write('x ∈ [a,b,c] - да'), nl ; write('x ∈ [a,b,c] - нет'), nl), nl,
    
    write('3. my_append:'), nl,
    my_append([1,2], [3,4], R1), write('[1,2] + [3,4] = '), write(R1), nl, nl,
    
    write('4. my_remove:'), nl,
    my_remove(b, [a,b,c,d], R2), write('Удалить b из [a,b,c,d]: '), write(R2), nl, nl,
    
    write('5. my_permute:'), nl,
    findall(P, my_permute([a,b], P), Perms), write('Перестановки [a,b]: '), write(Perms), nl, nl,
    
    write('6. my_sublist:'), nl,
    findall(S, my_sublist(S, [a,b]), Subs), write('Подсписки [a,b]: '), write(Subs), nl.

check_after_removal(List) :-
    write('Исходный список: '), write(List), nl,
    remove_last_std(List, WithoutLast),
    write('После удаления последнего: '), write(WithoutLast), nl,
    (is_sorted_std(List) -> 
        write('Исходный список упорядочен') 
    ; 
        write('Исходный список не упорядочен')
    ), nl,
    (is_sorted_std(WithoutLast) -> 
        write('Список после удаления упорядочен') 
    ; 
        write('Список после удаления не упорядочен')
    ), nl, nl.

run_all_tests :-
    check_builtin_predicates, nl,
    test_standard_predicates, nl,
    
    write('ТЕСТИРОВАНИЕ СПЕЦИАЛЬНЫХ ПРЕДИКАТОВ'), nl, nl,
    
    write('1. Удаление последнего элемента:'), nl,
    remove_last_std([1,2,3,4], R1), write('remove_last_std([1,2,3,4]): '), write(R1), nl,
    remove_last_manual([1,2,3,4], R2), write('remove_last_manual([1,2,3,4]): '), write(R2), nl,
    remove_last_std([a], R3), write('remove_last_std([a]): '), write(R3), nl, nl,
    
    write('2. Проверка упорядоченности:'), nl,
    (is_sorted_std([1,2,3,4,5]) -> write('[1,2,3,4,5] упорядочен'), nl ; write('[1,2,3,4,5] не упорядочен'), nl),
    (is_sorted_std([1,3,2,4]) -> write('[1,3,2,4] упорядочен'), nl ; write('[1,3,2,4] не упорядочен'), nl),
    (is_sorted_manual([1,2,3,4,5]) -> write('[1,2,3,4,5] упорядочен (manual)'), nl ; write('[1,2,3,4,5] не упорядочен (manual)'), nl), nl,
    
    write('ПРИМЕР СОВМЕСТНОГО ИСПОЛЬЗОВАНИЯ'), nl, nl,
    
    check_after_removal([1,2,3,4,5]),
    check_after_removal([1,2,5,3,4]),
    check_after_removal([5,4,3,2,1]).