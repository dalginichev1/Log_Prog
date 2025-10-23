get_all_subjects(Subjects) :-
    setof(Subject, Group^Student^Grade^grade(Group, Student, Subject, Grade), Subjects).

get_all_groups(Groups) :-
    setof(Group, Student^Subject^Grade^grade(Group, Student, Subject, Grade), Groups).

average_grade_per_subject :-
    write('Средний балл для каждого предмета:'), nl, nl,
    get_all_subjects(Subjects),
    calculate_average_for_subjects(Subjects).

calculate_average_for_subjects([]).
calculate_average_for_subjects([Subject|Rest]) :-
    findall(Grade, grade(_, _, Subject, Grade), Grades),
    sum_list(Grades, Sum),
    length(Grades, Count),
    Average is Sum / Count,
    format('~w: ~2f', [Subject, Average]), nl,
    calculate_average_for_subjects(Rest).

failed_students_per_group :-
    write('Количество не сдавших студентов по группам:'), nl, nl,
    get_all_groups(Groups),
    count_failed_per_group(Groups).

count_failed_per_group([]).
count_failed_per_group([Group|Rest]) :-
    findall(Student, (grade(Group, Student, _, Grade), Grade < 3), FailedStudents),
    sort(FailedStudents, UniqueFailedStudents),
    length(UniqueFailedStudents, Count),
    format('Группа ~w: ~w студентов', [Group, Count]), nl,
    count_failed_per_group(Rest).

failed_students_per_subject :-
    write('Количество не сдавших студентов по предметам:'), nl, nl,
    get_all_subjects(Subjects),
    count_failed_per_subject(Subjects).

count_failed_per_subject([]).
count_failed_per_subject([Subject|Rest]) :-
    findall(Student, (grade(_, Student, Subject, Grade), Grade < 3), FailedStudents),
    sort(FailedStudents, UniqueFailedStudents),
    length(UniqueFailedStudents, Count),
    format('~w: ~w студентов', [Subject, Count]), nl,
    count_failed_per_subject(Rest).

main :-
    nl, average_grade_per_subject, nl, nl,
    failed_students_per_group, nl, nl,
    failed_students_per_subject, nl.