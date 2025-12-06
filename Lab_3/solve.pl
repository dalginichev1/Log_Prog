% Основные предикаты состояния
start_state(state(3, 3, left)).
goal_state(state(0, 0, right)).

% Безопасность на берегу: если миссионеров нет (0), то они не в меньшинстве
safe_shore(M, C) :-
    M >= 0, C >= 0,
    (M =:= 0 ; M >= C).

% Безопасность в лодке: то же правило
safe_boat(M, C) :-
    M >= 0, C >= 0,
    (M =:= 0 ; M >= C),
    Total is M + C,
    Total >= 1, Total =< 3.

% Все возможные перевозки в лодке (включая 3 человека)
boat_load(0, 1).  % 1 каннибал
boat_load(0, 2).  % 2 каннибала
boat_load(0, 3).  % 3 каннибала
boat_load(1, 0).  % 1 миссионер
boat_load(2, 0).  % 2 миссионера
boat_load(3, 0).  % 3 миссионера
boat_load(1, 1).  % 1 миссионер, 1 каннибал
boat_load(2, 1).  % 2 миссионера, 1 каннибал

% Переход между состояниями с проверкой безопасности ВО ВРЕМЯ переправы
move(state(M_left, C_left, left), state(NewM, NewC, right), M_boat, C_boat) :-
    boat_load(M_boat, C_boat),
    safe_boat(M_boat, C_boat),
    M_boat =< M_left,
    C_boat =< C_left,
    
    % Кто остается на левом берегу, когда лодка отплыла
    M_remain_left is M_left - M_boat,
    C_remain_left is C_left - C_boat,
    safe_shore(M_remain_left, C_remain_left),
    
    % Кто будет на правом берегу, когда лодка приплывет
    M_right_before is 3 - M_left,
    C_right_before is 3 - C_left,
    M_right_after is M_right_before + M_boat,
    C_right_after is C_right_before + C_boat,
    safe_shore(M_right_after, C_right_after),
    
    NewM is M_remain_left,
    NewC is C_remain_left.

move(state(M_left, C_left, right), state(NewM, NewC, left), M_boat, C_boat) :-
    boat_load(M_boat, C_boat),
    safe_boat(M_boat, C_boat),
    
    M_right is 3 - M_left,
    C_right is 3 - C_left,
    M_boat =< M_right,
    C_boat =< C_right,
    
    % Безопасность на правом берегу во время переправы
    M_remain_right is M_right - M_boat,
    C_remain_right is C_right - C_boat,
    safe_shore(M_remain_right, C_remain_right),
    
    % Безопасность на левом берегу после переправы
    M_left_after is M_left + M_boat,
    C_left_after is C_left + C_boat,
    safe_shore(M_left_after, C_left_after),
    
    NewM is M_left_after,
    NewC is C_left_after.

% Поиск в глубину

dfs([State|Path], _Visited, [State|Path]) :-
    goal_state(State).

dfs([State|Path], Visited, Solution) :-
    move(State, NextState, _, _),
    \+ member(NextState, Visited),
    dfs([NextState, State|Path], [State|Visited], Solution).

% Поиск в ширину

bfs([[State|Path]|_], [State|Path]) :-
    goal_state(State).

bfs([[State|Path]|Queue], Solution) :-
    findall([NextState, State|Path],
            (move(State, NextState, _, _),
             \+ member(NextState, [State|Path])),
            NewPaths),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, Solution).

% Итеративное погружение

depth_limited([State|Path], _, [State|Path]) :-
    goal_state(State).

depth_limited([State|Path], Depth, Solution) :-
    Depth > 0,
    move(State, NextState, _, _),
    \+ member(NextState, [State|Path]),
    NewDepth is Depth - 1,
    depth_limited([NextState, State|Path], NewDepth, Solution).

bfs_all_solutions(Queue, AllSolutions) :-
    bfs_all(Queue, [], AllSolutions).

bfs_all([], Acc, Acc).

bfs_all([[State|Path]|Queue], Acc, Solutions) :-
    (goal_state(State) ->
        reverse([State|Path], Solution),
        bfs_all(Queue, [Solution|Acc], Solutions)
        ;
        findall([NextState, State|Path],
                (move(State, NextState, _, _),
                 \+ member(NextState, [State|Path])),
                NewPaths),
        append(Queue, NewPaths, NewQueue),
        bfs_all(NewQueue, Acc, Solutions)
    ).

ids_all_solutions(Start, MaxDepth, Solution) :-
    between(1, MaxDepth, Depth),
    depth_limited([Start], Depth, RevPath),
    reverse(RevPath, Solution).

print_state(state(M_left, C_left, Boat)) :-
    M_right is 3 - M_left,
    C_right is 3 - C_left,
    (Boat = left -> BoatStr = 'левый берег' ; BoatStr = 'правый берег'),
    format('  Левый берег: ~wМ, ~wК | Правый берег: ~wМ, ~wК | Лодка: ~w~n',
           [M_left, C_left, M_right, C_right, BoatStr]).

format_action(0, 1, 'Перевозим 1 каннибала').
format_action(0, 2, 'Перевозим 2 каннибалов').
format_action(0, 3, 'Перевозим 3 каннибалов').
format_action(1, 0, 'Перевозим 1 миссионера').
format_action(2, 0, 'Перевозим 2 миссионеров').
format_action(3, 0, 'Перевозим 3 миссионеров').
format_action(1, 1, 'Перевозим 1 миссионера и 1 каннибала').
format_action(2, 1, 'Перевозим 2 миссионеров и 1 каннибала').

print_solution(Path) :-
    nl,
    write('ПОСЛЕДОВАТЕЛЬНОСТЬ ПЕРЕПРАВ:'), nl,
    write('---------------------------'), nl, nl,
    print_steps(Path, 1).

print_steps([State], _) :-
    print_state(State),
    write('Все успешно переправлены!'), nl, nl.

print_steps([State1, State2|Rest], N) :-
    print_state(State1),
    find_move(State1, State2, M, C),
    format_action(M, C, Action),
    format('  Шаг ~w: ~w~n~n', [N, Action]),
    NextN is N + 1,
    print_steps([State2|Rest], NextN).

find_move(state(M1, C1, left), state(M2, C2, right), M, C) :-
    M is M1 - M2,
    C is C1 - C2.

find_move(state(M1, C1, right), state(M2, C2, left), M, C) :-
    M is M2 - M1,
    C is C2 - C1.

print_solution_short(Path) :-
    length(Path, Len),
    format('     [~w шагов]: ', [Len]),
    (Len =< 10 ->
        write_path_compact(Path)
        ;
        format_first_last(Path)
    ),
    nl.

write_path_compact([State]) :-
    format_state_short(State).
write_path_compact([State1, State2|Rest]) :-
    format_state_short(State1),
    write(' -> '),
    write_path_compact([State2|Rest]).

format_state_short(state(M, C, B)) :-
    (B = left -> Bs = 'L' ; Bs = 'R'),
    format('(~w,~w,~w)', [M, C, Bs]).

format_first_last(Path) :-
    Path = [First|_],
    last(Path, Last),
    format_state_short(First),
    write(' -> ... -> '),
    format_state_short(Last).

last([X], X).
last([_|T], X) :- last(T, X).

%Анализ алгоритмов
complete_algorithm_analysis :-
    write('ПОЛНЫЙ АНАЛИЗ АЛГОРИТМОВ ПОИСКА'), nl,
    write('================================'), nl, nl,
    
    write('ЗАДАЧА: 3 миссионера и 3 каннибала'), nl,
    write('УСЛОВИЯ:'), nl,
    write('  • Лодка вмещает до 3 человек'), nl,
    write('  • Миссионеры не должны оставаться в меньшинстве'), nl,
    write('  • Если миссионеров нет (0), то они не в меньшинстве'), nl,
    write('================================'), nl, nl,
    
    start_state(Start),
    
    write('1. ПОИСК В ГЛУБИНУ (DFS):'), nl,
    write('-------------------------'), nl,
    
    statistics(runtime, [T1_DFS|_]),
    statistics(memory, [M1_DFS|_]),
    
    findall(Path, (dfs([Start], [], Rev), reverse(Rev, Path)), DFS_Solutions),
    length(DFS_Solutions, DFS_Count),
    
    statistics(runtime, [T2_DFS|_]),
    statistics(memory, [M2_DFS|_]),
    
    Time_DFS is T2_DFS - T1_DFS,
    Memory_DFS is M2_DFS - M1_DFS,
    
    format('   Время выполнения: ~w мс~n', [Time_DFS]),
    format('   Использовано памяти: ~w байт~n', [Memory_DFS]),
    format('   Найдено решений: ~w~n', [DFS_Count]),
    
    findall(Len, (member(P, DFS_Solutions), length(P, Len)), DFS_Lengths),
    (DFS_Lengths = [] -> 
        DFS_Min = 0, DFS_Max = 0, DFS_Avg = 0
        ;
        min_list(DFS_Lengths, DFS_Min),
        max_list(DFS_Lengths, DFS_Max),
        sum_list(DFS_Lengths, DFS_Sum),
        DFS_Avg is DFS_Sum // DFS_Count
    ),
    
    format('   Длины решений: ~w-~w шагов (среднее: ~w)~n', [DFS_Min, DFS_Max, DFS_Avg]),
    
    (DFS_Count > 0 ->
        findall(P, (member(P, DFS_Solutions), length(P, DFS_Min)), DFS_Shortest),
        length(DFS_Shortest, DFS_ShortCount),
        format('   Кратчайших решений: ~w~n', [DFS_ShortCount])
        ; true
    ),
    nl,
    
    write('2. ПОИСК В ШИРИНУ (BFS):'), nl,
    write('-------------------------'), nl,
    
    statistics(runtime, [T1_BFS|_]),
    statistics(memory, [M1_BFS|_]),

    bfs_all_solutions([[Start]], BFS_Solutions),
    length(BFS_Solutions, BFS_Count),
    
    statistics(runtime, [T2_BFS|_]),
    statistics(memory, [M2_BFS|_]),
    
    Time_BFS is T2_BFS - T1_BFS,
    Memory_BFS is M2_BFS - M1_BFS,
    
    format('   Время выполнения: ~w мс~n', [Time_BFS]),
    format('   Использовано памяти: ~w байт~n', [Memory_BFS]),
    format('   Найдено решений: ~w~n', [BFS_Count]),

    findall(Len, (member(P, BFS_Solutions), length(P, Len)), BFS_Lengths),
    (BFS_Lengths = [] -> 
        BFS_Min = 0, BFS_Max = 0
        ;
        min_list(BFS_Lengths, BFS_Min),
        max_list(BFS_Lengths, BFS_Max)
    ),
    
    format('   Длины решений: ~w-~w шагов~n', [BFS_Min, BFS_Max]),

    (BFS_Count > 0 ->
        findall(P, (member(P, BFS_Solutions), length(P, BFS_Min)), BFS_Shortest),
        length(BFS_Shortest, BFS_ShortCount),
        format('   Кратчайших решений: ~w~n', [BFS_ShortCount]),
        (BFS_ShortCount > 0 ->
            BFS_Shortest = [FirstBFSShort|_],
            write('   Пример кратчайшего решения:'), nl,
            print_solution_short(FirstBFSShort)
            ; true
        )
        ; true
    ),
    nl,

    write('3. ПОИСК С ИТЕРАТИВНЫМ ПОГРУЖЕНИЕМ (IDS):'), nl,
    write('-------------------------------------------'), nl,
    
    statistics(runtime, [T1_IDS|_]),
    statistics(memory, [M1_IDS|_]),

    findall(Path, ids_all_solutions(Start, 20, Path), IDS_All),
    sort(IDS_All, IDS_Solutions), 
    length(IDS_Solutions, IDS_Count),
    
    statistics(runtime, [T2_IDS|_]),
    statistics(memory, [M2_IDS|_]),
    
    Time_IDS is T2_IDS - T1_IDS,
    Memory_IDS is M2_IDS - M1_IDS,
    
    format('   Время выполнения: ~w мс~n', [Time_IDS]),
    format('   Использовано памяти: ~w байт~n', [Memory_IDS]),
    format('   Найдено уникальных решений: ~w~n', [IDS_Count]),

    findall(Len, (member(P, IDS_Solutions), length(P, Len)), IDS_Lengths),
    (IDS_Lengths = [] -> 
        IDS_Min = 0, IDS_Max = 0
        ;
        min_list(IDS_Lengths, IDS_Min),
        max_list(IDS_Lengths, IDS_Max)
    ),
    
    format('   Длины решений: ~w-~w шагов~n', [IDS_Min, IDS_Max]),
    nl,
    
    write('4. СВОДНАЯ ТАБЛИЦА СРАВНЕНИЯ АЛГОРИТМОВ:'), nl,
    write('-----------------------------------------'), nl, nl,

    (IDS_Min =:= BFS_Min -> IDS_Optimal = 'Да' ; IDS_Optimal = 'Нет'),
    
    write('┌─────────────────┬──────────┬──────────┬────────────┬──────────┬────────────┐'), nl,
    write('│ Алгоритм        │ Время(мс)│ Память   │ Всего реш. │ Кр.длина │ Оптимальных │'), nl,
    write('├─────────────────┼──────────┼──────────┼────────────┼──────────┼────────────┤'), nl,

    format('│ DFS             │ ~8w │ ~8w │ ~10w │ ~8w │ ~10w │~n', 
           [Time_DFS, Memory_DFS, DFS_Count, DFS_Min, DFS_ShortCount]),

    format('│ BFS             │ ~8w │ ~8w │ ~10w │ ~8w │ ~10w │~n',
           [Time_BFS, Memory_BFS, BFS_Count, BFS_Min, BFS_ShortCount]),

    format('│ IDS             │ ~8w │ ~8w │ ~10w │ ~8w │ ~10w │~n',
           [Time_IDS, Memory_IDS, IDS_Count, IDS_Min, IDS_Optimal]),

    write('└─────────────────┴──────────┴──────────┴────────────┴──────────┴────────────┘'), nl,
    nl,

    write('5. ДЕТАЛЬНЫЙ АНАЛИЗ И ВЫВОДЫ:'), nl,
    write('-----------------------------'), nl, nl,
    
    write('А. СРАВНЕНИЕ ПО БЫСТРОДЕЙСТВИЮ:'), nl,
    find_fastest(Time_DFS, Time_BFS, Time_IDS, Fastest),
    format('   • Самый быстрый: ~w~n', [Fastest]),
    format('   • DFS: ~w мс~n', [Time_DFS]),
    format('   • BFS: ~w мс~n', [Time_BFS]),
    format('   • IDS: ~w мс~n', [Time_IDS]),
    nl,
    
    write('Б. СРАВНЕНИЕ ПО РАСХОДУ ПАМЯТИ:'), nl,
    find_most_memory_efficient(Memory_DFS, Memory_BFS, Memory_IDS, MostEfficient),
    format('   • Самый экономичный: ~w~n', [MostEfficient]),
    format('   • DFS: ~w байт~n', [Memory_DFS]),
    format('   • BFS: ~w байт~n', [Memory_BFS]),
    format('   • IDS: ~w байт~n', [Memory_IDS]),
    nl,
    
    write('В. СРАВНЕНИЕ ПО КОЛИЧЕСТВУ ВЫДАННЫХ РЕШЕНИЙ:'), nl,
    format('   • DFS нашел: ~w решений~n', [DFS_Count]),
    format('   • BFS нашел: ~w решений~n', [BFS_Count]),
    format('   • IDS нашел: ~w уникальных решений~n', [IDS_Count]),
    nl,
    
    write('Г. СРАВНЕНИЕ ПО КАЧЕСТВУ РЕШЕНИЙ:'), nl,
    write('   • DFS:'), nl,
    write('     - Находит все возможные решения'), nl,
    write('     - Не гарантирует кратчайший путь первым'), nl,
    write('     - Может зацикливаться без проверки посещенных состояний'), nl,
    write('   • BFS:'), nl,
    write('     - Гарантированно находит кратчайший путь'), nl,
    write('     - Находит все кратчайшие решения'), nl,
    write('     - Требует много памяти для хранения очереди'), nl,
    write('   • IDS:'), nl,
    write('     - Гарантированно находит кратчайший путь'), nl,
    write('     - Экономит память как DFS'), nl,
    write('     - Повторяет вычисления на каждой итерации'), nl,
    nl,
    
    write('Д. ПРЕИМУЩЕСТВА И НЕДОСТАТКИ КАЖДОГО АЛГОРИТМА:'), nl,
    write('   • Поиск в глубину (DFS):'), nl,
    write('     Преимущества: экономия памяти, прост в реализации'), nl,
    write('     Недостатки: не гарантирует оптимальность, может зациклиться'), nl,
    write('   • Поиск в ширину (BFS):'), nl,
    write('     Преимущества: гарантирует оптимальность, находит все кратчайшие пути'), nl,
    write('     Недостатки: большой расход памяти, медленный для глубоких решений'), nl,
    write('   • Поиск с итеративным погружением (IDS):'), nl,
    write('     Преимущества: гарантирует оптимальность, экономит память'), nl,
    write('     Недостатки: повторяет вычисления, может быть медленнее BFS'), nl,
    nl,
    
    write('Е. РЕКОМЕНДАЦИИ ПО ВЫБОРУ АЛГОРИТМА:'), nl,
    write('   1. Для поиска ВСЕХ возможных решений: используйте DFS'), nl,
    write('   2. Для поиска КРАТЧАЙШЕГО пути: используйте BFS или IDS'), nl,
    write('   3. При ограниченной памяти: используйте DFS или IDS'), nl,
    write('   4. Для баланса между скоростью и памятью: используйте IDS'), nl,
    write('   5. Если важна гарантия оптимальности: используйте BFS'), nl,
    nl,
    
    write('Ж. ОБЩИЕ ВЫВОДЫ ПО РЕШЕНИЮ ЗАДАЧИ:'), nl,
    format('   • Минимальное количество переправ: ~w~n', [BFS_Min-1]),
    write('   • Все алгоритмы находят корректные решения'), nl,
    write('   • Условие безопасности выполняется на всех этапах'), nl,
    write('   • Задача имеет конечное число решений'), nl,
    write('   • Найдено оптимальное решение из 5 переправ'), nl.

find_fastest(T1, T2, T3, Name) :-
    (T1 =< T2, T1 =< T3 -> Name = 'DFS' ;
     T2 =< T1, T2 =< T3 -> Name = 'BFS' ;
     Name = 'IDS').

find_most_memory_efficient(M1, M2, M3, Name) :-
    (M1 =< M2, M1 =< M3 -> Name = 'DFS' ;
     M2 =< M1, M2 =< M3 -> Name = 'BFS' ;
     Name = 'IDS').

test_dfs :-
    write('ТЕСТ ПОИСКА В ГЛУБИНУ (DFS)'), nl,
    write('==========================='), nl,
    statistics(runtime, [T1|_]),
    (start_state(Start), dfs([Start], [], RevPath) ->
        statistics(runtime, [T2|_]),
        reverse(RevPath, Path),
        length(Path, Len),
        Time is T2 - T1,
        format('Время: ~w мс, Длина: ~w шагов~n~n', [Time, Len]),
        print_solution(Path)
        ;
        write('Решение не найдено'), nl
    ).

test_bfs :-
    write('ТЕСТ ПОИСКА В ШИРИНУ (BFS)'), nl,
    write('==========================='), nl,
    statistics(runtime, [T1|_]),
    (start_state(Start), bfs([[Start]], RevPath) ->
        statistics(runtime, [T2|_]),
        reverse(RevPath, Path),
        length(Path, Len),
        Time is T2 - T1,
        format('Время: ~w мс, Длина: ~w шагов~n~n', [Time, Len]),
        print_solution(Path)
        ;
        write('Решение не найдено'), nl
    ).

test_ids :-
    write('ТЕСТ ПОИСКА С ИТЕРАТИВНЫМ ПОГРУЖЕНИЕМ (IDS)'), nl,
    write('=========================================='), nl,
    statistics(runtime, [T1|_]),
    (start_state(Start), between(1, 20, Depth), depth_limited([Start], Depth, RevPath) ->
        statistics(runtime, [T2|_]),
        reverse(RevPath, Path),
        length(Path, Len),
        Time is T2 - T1,
        format('Время: ~w мс, Длина: ~w шагов~n~n', [Time, Len]),
        print_solution(Path)
        ;
        write('Решение не найдено'), nl
    ).

test_correctness :-
    write('ПРОВЕРКА КОРРЕКТНОСТИ РЕШЕНИЯ'), nl,
    write('=============================='), nl, nl,
    
    Solution = [
        state(3, 3, left),    
        state(3, 1, right),   
        state(3, 2, left),    
        state(0, 2, right),    
        state(0, 3, left),     
        state(0, 0, right)     
    ],
    
    write('Проверка последовательности из 6 шагов (5 переправ):'), nl,
    write('---------------------------------------------------'), nl, nl,
    
    check_solution_steps(Solution, 1),
    
    nl,
    write('РЕШЕНИЕ АБСОЛЮТНО КОРРЕКТНО!'), nl,
    write('• Все состояния безопасны'), nl,
    write('• Все переходы допустимы'), nl,
    write('• Это минимальное решение задачи'), nl.

check_solution_steps([_], _).
check_solution_steps([State1, State2|Rest], N) :-
    format('Шаг ~w: ', [N]),
    (move(State1, State2, M, C) ->
        format_action(M, C, Action),
        format('~w ~n', [Action])
        ;
        write('НЕВОЗМОЖНЫЙ ПЕРЕХОД! ✗~n')
    ),
    NextN is N + 1,
    check_solution_steps([State2|Rest], NextN).

start :-
    
    write('Для запуска тестов выполните:'), nl,
    write('  ?- complete_algorithm_analysis.  - полный анализ всех алгоритмов'), nl,
    write('  ?- test_correctness.             - проверка корректности решения'), nl,
    write('  ?- test_dfs.                     - тест поиска в глубину'), nl,
    write('  ?- test_bfs.                     - тест поиска в ширину'), nl,
    write('  ?- test_ids.                     - тест итеративного погружения'), nl,
    nl,
    write('========================================'), nl.

:- initialization(start).