:- table p/3.


p(x,tid,0).
p(x,tid,1).
p(x,tid,2).
p(x,tid,3).
p(x,tid,4).
p(x,tid,5).
p(x,uid,0).
p(x,uid,1).
p(x,uid,2).
p(x,uid,3).
p('task 0 0',hasName,'Task 0').
p('task 1 0',hasName,'Task 0').
p('task 2 0',hasName,'Task 0').
p('task 3 0',hasName,'Task 0').
p('task 0 1',hasName,'Task 1').
p('task 1 1',hasName,'Task 1').
p('task 2 1',hasName,'Task 1').
p('task 3 1',hasName,'Task 1').
p('task 0 2',hasName,'Task 2').
p('task 1 2',hasName,'Task 2').
p('task 2 2',hasName,'Task 2').
p('task 3 2',hasName,'Task 2').
p('task 0 3',hasName,'Task 3').
p('task 1 3',hasName,'Task 3').
p('task 2 3',hasName,'Task 3').
p('task 3 3',hasName,'Task 3').
p('task 0 4',hasName,'Task 4').
p('task 1 4',hasName,'Task 4').
p('task 2 4',hasName,'Task 4').
p('task 3 4',hasName,'Task 4').
p('task 0 5',hasName,'Task 5').
p('task 1 5',hasName,'Task 5').
p('task 2 5',hasName,'Task 5').
p('task 3 5',hasName,'Task 5').
p('user 0',hasTask,'task_00').
p('user 1',hasTask,'task_10').
p('user 2',hasTask,'task_20').
p('user 3',hasTask,'task_30').
p('user 0',hasTask,'task_01').
p('user 1',hasTask,'task_11').
p('user 2',hasTask,'task_21').
p('user 3',hasTask,'task_31').
p('user 0',hasTask,'task_02').
p('user 1',hasTask,'task_12').
p('user 2',hasTask,'task_22').
p('user 3',hasTask,'task_32').
p('user 0',hasTask,'task_03').
p('user 1',hasTask,'task_13').
p('user 2',hasTask,'task_23').
p('user 3',hasTask,'task_33').
p('user 0',hasTask,'task_04').
p('user 1',hasTask,'task_14').
p('user 2',hasTask,'task_24').
p('user 3',hasTask,'task_34').
p('user 0',hasTask,'task_05').
p('user 1',hasTask,'task_15').
p('user 2',hasTask,'task_25').
p('user 3',hasTask,'task_35').
p('user 0',hasName,'bob0').
p('user 1',hasName,'bob1').
p('user 2',hasName,'bob2').
p('user 3',hasName,'bob3').
p(db,hasUser,'user_0').
p(db,hasUser,'user_1').
p(db,hasUser,'user_2').
p(db,hasUser,'user_3').
p(db,hasUser,'user 0').
p(db,hasUser,'user 1').
p(db,hasUser,'user 2').
p(db,hasUser,'user 3').
p('task_3f49044c1469c6990a665f46ec6c0a41',hasName,'This is a 2nd task from a mutation.').
p('user_1',hasTask,'task_3f49044c1469c6990a665f46ec6c0a41').
p('user_1',hasTask,'task_fbbe8cc6f1fa0406dc6b6bf0f968d7ac').
p('task_fbbe8cc6f1fa0406dc6b6bf0f968d7ac',hasName,'This task has a new name.').
p(html,attribute,class).
p(html,attribute,href).
p(html,attribute,id).
p(html,attribute,l1).
p(html,attribute,l2).
p(html,attribute,l3).
p(html,element,a).
p(html,element,div).

% String concatenation predicates

% Userpage structure rules
p(l(userpage, UID), hasPath, Path) :- 
    p(db,hasUser,UID),
    atomic_list_concat(['user/', UID], Path).

p(l(userpage, UID), hasContent, l(userpage, UID, c1)) :- p(db,hasUser,UID).
p(l(userpage, UID, c1), a, div) :- p(db,hasUser,UID).
p(l(userpage, UID, c1), id, 'someid') :- p(db,hasUser,UID).
p(l(userpage, UID, c1), l1, '1') :- p(db,hasUser,UID).
p(l(userpage, UID, c1), l2, '2') :- p(db,hasUser,UID).
p(l(userpage, UID, c1), class, 'flex w-full justify-center') :- p(db,hasUser,UID).
p(l(userpage, UID, c1), child, l(userpage, UID, c2)) :- p(db,hasUser,UID).
p(l(userpage, UID, c2), a, div) :- p(db,hasUser,UID).
p(l(userpage, UID, c2), class, 'flex flex-col justify-center') :- p(db,hasUser,UID).

% Task rules
p(l(userpage, UID, c2), child, l(userpage, UID, c2, Tid)) :- p(UID,hasTask,Tid).
p(l(userpage, UID, c2, Tid), a, div) :- p(UID,hasTask,Tid).
p(l(userpage, UID, c2, Tid), class, 'p-1 border-2 border-neutral-400 rounded') :- p(UID,hasTask,Tid).
p(l(userpage, UID, c2, Tid), text, Taskname) :- p(UID,hasTask,Tid), p(Tid,hasName,Taskname).

% Attribute string rule
p(X, hasAttributeString, AttrStr) :-
    p(html,element,El),
    p(html,attribute,AttrName),
    p(X,a,El),
    p(X,AttrName,AttrValue),
    atomic_list_concat([AttrName, '=\'', AttrValue, '\''], AttrStr).

% Join helpers
p(X, hasJoined, l(attributeString, Joined)) :-
    findall(AttrStr, 
            p(X, hasAttributeString, AttrStr), 
            AttrList),
    % length(AttrList, L),
    % L > 0,
    atomic_list_concat(AttrList, ' ', Joined).

p(X, hasJoined, l(childHTML, Joined)) :-
    findall(HTML,
            (p(X, child, Y), p(Y, hasHTML, HTML)),
            HTMLList),
    length(HTMLList, L),
    L > 0,
    atomic_list_concat(HTMLList, '', Joined).

% HTML generation rules
p(X, hasLeftRightHTML, l(OpenTag, CloseTag)) :-
    p(html, element, El),
    p(X, a, El),
    p(X, hasJoined, l(attributeString, Attrs)),
    atomic_list_concat(['<', El, ' ', Attrs, ' >'], OpenTag),
    atomic_list_concat(['</', El, '>'], CloseTag).

p(X, hasHTML, HTML) :-
    p(X, hasLeftRightHTML, l(S1, S2)),
    p(X, text, T),
    atomic_list_concat([S1, T, S2], HTML).

p(X, hasHTML, HTML) :-
    p(X, hasLeftRightHTML, l(S1, S2)),
    p(X, hasJoined, l(childHTML, T)),
    atomic_list_concat([S1, T, S2], HTML).
% Final result collection
p(res, is, l(P, T)) :-
    p(X, hasPath, P),
    p(X, hasContent, C),
    p(C, hasHTML, T).
