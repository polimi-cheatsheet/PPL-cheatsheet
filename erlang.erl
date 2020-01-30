-module(test).
-compile(export_all).
%-export([add/2]).

% Builtins
% date() time() length([1,2,3]) size({1,2,3})
% atom_to_list(atom) => "atom"
% list_to_tuple([1,2,3]) => {1,2,3}
% tuple_to_list({1,2,3}) => [1,2,3]
% integer_to_list(1234) => "1234"

% lists module
% hd(List) -> Element -- Returns the first element of the list.
% tl(List) -> List -- Returns the list minus its first element.
% length(List) -> Integer -- returns the length of the list.
% all(Pred, List) -> bool()
% any(Pred, List) -> bool()
% append(List1, List2) -> List3 is equivalent to A ++ B.
% filter(Pred, List1) -> List2
%     lists:filter(fun(X) -> X =< 3 end, [3, 1, 4, 1, 6]). % Result is [3,1,1]
% flatten(DeepList) -> List -- Returns a flattened version of DeepList.
% foldl(Fun, Acc0, List) -> Acc1
%     Example: lists:foldl(fun(X, Y) -> X + 10 * Y end, 0, [1, 2, 3]). % Result is 123
% foreach(Fun, List) -> void()
% map(Fun, List1) -> List2
%     Example: lists:map(fun(X) -> 2 * X end, [1, 2, 3]). % Result is [2,4,6]
% member(Elem, List) -> bool() -- Search Elem in List
% partition(Pred, List) -> {Satisfying, NonSatisfying}
% reverse(List1) -> List2
% seq(From, To) -> Seq -- [From, To], inclusive.
% seq(From, To, Incr) -> Seq
% sort(List1) -> List2 -- Returns a list containing the sorted elements of List1.
% unzip(List1) -> {List2, List3}
% zip(List1, List2) -> List3 -- L1 and L2 of the same length


% Variables (UpperCamelCase)
diff(A, B) ->
    C = A - B,
    C.

% Functions
add(A, B) ->
    A + B.

% Definition via pattern, note the ';'
fact(0) -> 1;
fact(N) when N > 0 -> N * fact(N-1).
area({square, Side}) -> Side * Side;
area({circle, Radius}) -> 3.14 * Radius * Radius;
area({triangle, A, B, C}) ->
    S = (A + B + C) / 2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).
% Guards after `when`:
% is_number(X) is_integer(X) is_float(X) is_atom(X) is_tuple(X) is_list(X)
% X > Y + Z
% X =< Y  (not <=)
% A and B (or/not/...)
% X =:= Y  <=>  X === Y
% X == Y  <=>  equal with float-int coercion
% X =/= Y  <=>  X !== Y

% Case
casef(X) ->
    case lists:member(a, X) of
        true -> present;
        false -> absent
    end.
iff(X) ->
    if
        is_integer(X) -> integer;
        is_tuple(X) -> tuple;
        true -> boh
    end.

% Symbols (lowercase or 'escaped')
sym() ->
    ciao.

% Tuples
tuple() ->
    {123, abc, {a, b}}.

% List
list() ->
    [1, 2, 3] ++ [4, 5].
cons(X, L) ->
    [X | L].

% List comprehensions
comp() ->
    [{X,Y} || X <- [-1,0,1], Y <- [one, two], X >= 0].
%   => [{0,one},{1,one},{0,two},{1,two}]

% Matching
match() ->
    [A,B|C] = [1,2,3,4], % => A=1, B=2, C=[3,4]
    {D,E,_} = {1,2,3}. % D=1, E=2

% Maps
map() ->
    Map = #{one => 1, "Two" => 2, 3 => three},
    Map#{one := "uno"}, % return the updated, keep Map unchanged
    #{"Two" := V} = Map,
    V. % => 2

% apply(Mod, Func, Args)
apply() ->
    apply(?MODULE, add, [1,2]). % => 3

% Lambda
lambda() ->
    fun(X) -> X*X end.
lambda2() ->
    (lambda())(9), % => 81
    lists:map(lambda(), [1,2,3]), % => [1,4,9]
    lists:foldr(fun add/2, 0, [1,2,3]). % => 6

% Processes and messages
actor1() ->
    Pid2 = spawn(?MODULE, actor2, []),
    Pid2 ! {self(), foo}.

actor2() ->
    receive
        {From, Msg} -> io:format("Actor2 received ~w from ~w~n", [Msg, From])
    end.
actor2withTimeout(T) ->
    receive
        {From, Msg} -> io:format("Actor2 received ~w from ~w~n", [Msg, From])
    after
        T -> io:format("Timeout!") % T in ms
    end.

% Management of processes
startMaster() ->
    Pid = spawn(?MODULE, masterBody, []),
    register(master, Pid).
startWorker() ->
    spawn_link(?MODULE, workerBody, []).
workerBody() ->
    BadLuck = rand:uniform(100) =< 30,
    if
        BadLuck -> exit("Bye.");
        true -> master ! {done}
    end.
masterBody() ->
    startWorker(),
    startWorker(),
    startWorker(),
    handleExits().
handleExits() ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', Pid, normal} ->
            io:format("Process ~p exited normally~n", [Pid]),
            handleExits();
        {'EXIT', Pid, Msg} ->
            io:format("Process ~p died with message: ~s~n",
		      [Pid, Msg]),
            spawn_link(?MODULE, workerBody, []),
            handleExits();
        {done} ->
            io:format("Worker done~n", []),
            handleExits()
    end.
