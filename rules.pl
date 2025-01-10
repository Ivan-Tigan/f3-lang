:- op(800, xfx, =>).
:- op(900, xfx, when).

:- dynamic p/3.

% Example data
p(user1, name, "bob").
p(user1, age, 25).
p(task1, owner, user1).
p(task1, status, "pending").

% Helper to execute multiple operations
execute_ops([], _).
execute_ops([Op|Ops], vars(User,Task)) :-
   writeln(executing_op(Op, vars(User,Task))),
   execute_op(Op, User, Task),
   execute_ops(Ops, vars(User,Task)).

execute_op(insert(User,P,O), User, _) :-
   % Insert for user
   writeln(inserting(p(User,P,O))),
   assertz(p(User,P,O)).
execute_op(insert(Task,P,O), _, Task) :-
   % Insert for task
   writeln(inserting(p(Task,P,O))),
   assertz(p(Task,P,O)).
execute_op(delete(Task,status,"pending"), _, Task) :-
   % Delete pending status (only for tasks)
   writeln(deleting(p(Task,status,"pending"))),
   retract(p(Task,status,"pending")).

rule(task_activation) when
   [p(User, name, _), 
    p(Task, owner, User), 
    p(Task, status, "pending")] =>
   [insert(User, status, "busy"),
    delete(Task, status, "pending"),
    insert(Task, status, "active")].

apply_rule(Name) :-
   rule(Name) when Conditions => Operations,
   % Match all conditions and collect variables
   findall(vars(User,Task), 
           (
               member(p(User, name, _), Conditions),
               member(p(Task, owner, User), Conditions),
               member(p(Task, status, "pending"), Conditions),
               p(User, name, _),
               p(Task, owner, User),
               p(Task, status, "pending")
           ), 
           [Vars]),
   writeln(found_vars(Vars)),
   % Then execute all operations with those bindings
   execute_ops(Operations, Vars).

test :-
   writeln('Initial state:'),
   listing(p),
   writeln('Applying rule...'),
   apply_rule(task_activation),
   writeln('Final state:'),
   listing(p).

:- 
   writeln('Running test...'),
   test,
   halt.