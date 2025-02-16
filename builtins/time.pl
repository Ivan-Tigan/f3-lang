:- module(time, []).
:- multifile b/3.
:- multifile builtin.


dateTimeToGraph(date(Y, M, D, H, M, S, Off, TZ, Dst), graph([
        p(datetime, years, Y),
        p(datetime, months, M),
        p(datetime, days, D),
        p(datetime, hours, H),
        p(datetime, minutes, M),
        p(datetime, seconds, S),
        p(datetime, offset, Off),
        p(datetime, timezone, TZ),
        p(datetime, dst, Dst)
    ])).


builtin(now).
builtin(duration).

b(system, now, T) :- get_time(T).


b(Timestamp, timestampToDateTime, DateTimeGraph) :- stamp_date_time(TimeStamp, DateTime, 'UTC'), dateTimeToGraph(DateTime, DateTimeGraph).

b(DateTimeGraph, [setTimeZone, TZ], NewDateTimeGraph) :- 
    dateTimeToGraph(DateTime, DateTimeGraph),
    date_time_stamp(DateTime, TimeStamp),
    stamp_date_time(TimeStamp, NewDateTime, TZ),
    dateTimeToGraph(NewDateTime, NewDateTimeGraph).
    



:- initialization(run_tests).