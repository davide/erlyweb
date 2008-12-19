%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com]
%% @copyright Yariv Sadan 2006-2007
%%
%% @doc
%% This module provides functions for getting and setting
%% values of a Yaws 'arg' record. You can use these functions
%% instead of using the record access syntax, and without
%% having to include yaws_api.hrl.
%%
%% Most functions have two forms: one for getting the value of a field and
%% one for setting it. Getters accept the record as a parameter and return
%% the value of its field. Setters take
%% two parameters -- the record and the new value -- and return a new record
%% with the modified value.
%%
%% @end

%% For license information see LICENSE.txt

-module(yaws_arg).
-author("Yariv Sadan (yarivsblog@gmail.com)").

-export([new/0,
	 add_to_opaque/2, add_all_to_opaque/2, get_opaque_val/2,
	 clisock/1, clisock/2, client_ip_port/1, client_ip_port/2,
	 headers/1, headers/2, req/1, req/2,
	 method/1, clidata/1, clidata/2, server_path/1, server_path/2,
	 querydata/1, querydata/2, appmoddata/1, appmoddata/2, docroot/1,
	 docroot/2, fullpath/1, fullpath/2, cont/1, cont/2, state/1,
	 state/2, pid/1, pid/2, opaque/1, opaque/2, appmod_prepath/1,
	 appmod_prepath/2, pathinfo/1, pathinfo/2,
	 app_root/1]).
-include("yaws_api.hrl").

%% @doc Create a new 'arg' record.
new() ->
    #arg{}.

%% @equiv Arg#arg{opaque = [Val | A#arg.opaque]}
add_to_opaque(Arg, Val) ->
    Arg#arg{opaque = [Val | Arg#arg.opaque]}.

%% @doc applies add_to_opaque for all values in the list
%%
%% @spec add_all_to_opaque(A::arg(), Vals::[term()]) -> arg()
add_all_to_opaque(A, Vals) ->
    lists:foldl(
      fun(Val, A1) ->
	      add_to_opaque(A1, Val)
      end, A, Vals).

%% @doc Return the value corrsponding to the Key in the opaque proplist.
%% If the key isn't found, return 'undefined'.
%%
%% @spec get_opaque_val(A::arg(), Key::term()) -> term() | undefined
get_opaque_val(A, Key) ->
    proplists:get_value(Key, yaws_arg:opaque(A)).

clisock(Arg) ->
    Arg#arg.clisock.

clisock(Arg, Val) ->
    Arg#arg{clisock = Val}.

client_ip_port(Arg) ->
    Arg#arg.client_ip_port.

client_ip_port(Arg, Val) ->
    Arg#arg{client_ip_port = Val}.

headers(Arg) ->
    Arg#arg.headers.

headers(Arg, Val) ->
    Arg#arg{headers = Val}.

req(Arg) ->
    Arg#arg.req.

req(Arg, Val) ->
    Arg#arg{req = Val}.

method(Arg) ->
    (Arg#arg.req)#http_request.method.

clidata(Arg) ->
    Arg#arg.clidata.

clidata(Arg, Val) ->
    Arg#arg{clidata = Val}.

server_path(Arg) ->
    Arg#arg.server_path.

server_path(Arg, Val) ->
    Arg#arg{server_path = Val}.		  
    
querydata(Arg) ->
    Arg#arg.querydata.

querydata(Arg, Val) ->
    Arg#arg{querydata = Val}.

docroot(Arg) ->
    Arg#arg.docroot.

docroot(Arg, Val) ->
    Arg#arg{docroot = Val}.

fullpath(Arg) ->
    Arg#arg.fullpath.

fullpath(Arg, Val) ->
    Arg#arg{fullpath = Val}.

cont(Arg) ->
    Arg#arg.cont.

cont(Arg, Val) ->
    Arg#arg{cont = Val}.

state(Arg) ->
    Arg#arg.state.

state(Arg, Val) ->
    Arg#arg{state = Val}.

pid(Arg) ->
    Arg#arg.pid.

pid(Arg, Val) ->
    Arg#arg{pid = Val}.

opaque(Arg) ->
    Arg#arg.opaque.

opaque(Arg, Val) ->
    Arg#arg{opaque = Val}.

appmod_prepath(Arg) ->
    Arg#arg.appmod_prepath.

appmod_prepath(Arg, Val) ->
    Arg#arg{appmod_prepath = Val}.

appmoddata(Arg) ->
    case pathinfo(Arg) of
	undefined -> "/";
	Pathinfo -> Pathinfo
    end.

appmoddata(Arg, Val) ->
    pathinfo(Arg, Val).

pathinfo(Arg) ->
    Arg#arg.pathinfo.

% If we change pathinfo value independently of the remaing values
% then all code that might use them together will break.
% Example: erlyweb:get_app_root(A) - now migrated here. See below.
% If erlyweb used its own record we'd be free to store app_root there
% but since we're wrapping yaws #arg we need to be carefull with
% changes that might create an incoherent state.
pathinfo(Arg, Val) ->
    AppRoot = app_root(Arg),
    A1 = Arg#arg{pathinfo=Val},
    A2 = A1#arg{server_path=(AppRoot ++ Val)},
    A2#arg{fullpath=(A2#arg.docroot ++ A2#arg.server_path)}.


%% @doc Get the relative URL for the application's root path.
%% This function was moved here from erlyweb because it's
%% completely dependant on other #arg fields so it makes
%% sense to offer it as if it was a part of the #arg's record.
%%
%% @spec get_app_root(A::arg()) -> string()
app_root(Arg) ->
    L1 = if (Arg#arg.pathinfo =:= undefined) -> 0; true -> length(Arg#arg.pathinfo) end,
    ServerPath =Arg#arg.server_path,
    L2 = length(ServerPath),
    string:substr(ServerPath, 1, L2 - L1).

