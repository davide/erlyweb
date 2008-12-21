%% @author Davide Marquês <nesrait@gmail.com>
%%
%% @doc This module provides functions for extending erlang packages module.
%% See http://www.erlang.org/doc/man/packages.html
%% Calls to the package module should be tunnelled through this module.
%%
%% Overridden functions
%% I've overridden to_string/1 and concat/2 because these functions
%% were a bit too low level for my taste:
%%   packages:to_string([a,b]) -> [a,b]
%%   packages:concat([a,b], [c,d]) -> [a,b,46,c,d]
%% 
%% The return value for smerl:get_module(Metamod) is an atom when used
%% on regular modules:
%%   packages:to_string(regular_module) -> "regular_module"
%%   packages:concat(package, regular_module) -> "package.regular_module"
%% But when handling packaged modules the returned value is
%%   [package, module]
%% (I'm guessing that's the abstract syntax tree internal representation)
%% This doesn't play nice with to_string/1 nor contact/2:
%%   packages:to_string([package, module]) -> [package,module]
%%   packages:concat(prefix, [package, module]) ->
%%                    [112,114,101,102,105,120,46,package,module]
%% 
%% My implementation should provide this:
%%   packages:to_string([package, module]) -> "package.module"
%%   packages:concat(prefix, [package, module]) -> "prefix.package.module"
%%
%% Known limitations:
%%  - smerl:get_forms_from_binary/2:
%%     code:where_is_file doesn't have any support for looking up files
%%     in sub-directories (even if you provide the relative path)
%%

%% For license information see LICENSE.txt

-module(packages_ext).
-extends(packages).
-author("Davide Marquês (nesrait@gmail.com)").

% Overridden functions
-export([
		to_string/1,
		concat/2
	]).

% Other function available for future hacking:
% {concat,1}, {is_valid,1}, {is_segmented,1}, {last,1}, {first,1}, {strip_last,1}, {find_modules,1}, {find_modules,2}, {split,1},

% New functions
-export([
		atomize/1,
		to_path/1,
		module_dir/1,
		package_dirs/1,
		package_dirs/2
	]).

%
% Overridden functions
%

%% @doc Return the given module as a string.
%%
%% @spec to_string(Module::atom() | Module::string() | Module::list()) -> string()
to_string(Module) when is_atom(Module) ->
	atom_to_list(Module);
to_string(Module) when is_list(Module) ->
    if	(is_atom(hd(Module))) ->
        packages:concat(Module);
        true -> Module
    end.

%% @doc Return the combination of the given modules as a string.
%%
%% @spec concat(M1::any() | M2:any()) -> string()
concat('', File) ->
	to_string(File);
concat(Package, File) when is_atom(File)->
    concat(Package, atom_to_list(File));
concat(Package, File) ->
    to_string(Package) ++ "." ++ File.

%
% New functions
%

%% @doc Return the corresponding atom for the given module name/atom.
%% Use this function to ensure that you're using a proper erlang atom.
%%
%% @spec atom_for(Module::atom() | ModuleName::string()) -> atom()
atomize(Module) when is_atom(Module) ->
    Module;
atomize(ModuleName) ->
   list_to_atom(to_string(ModuleName)).

%% @doc Returns the filesystem path that matchs the given module.
%% Example: package.module will become package/module.
%%
%% @spec atom_for(Module::atom() | Module::string()) -> string()
to_path(Module) when is_atom(Module) ->
    case packages:is_segmented(Module) of
	true -> % seg1.seg2
		string:join(packages:split(Module), "/");
	false -> % single_seg
		atom_to_list(Module)
    end;
to_path(Module) when is_list(Module) ->
    case is_integer(hd(Module)) of
	true -> % "seg1.seg2"
		lists:map(fun($.) -> $/; (O) -> O end, Module);
	false -> % [seg1,seg2,seg3]
		string:join(lists:map(fun atom_to_list/1, Module), "/")
    end.


%% @doc Returns the filesystem (relative) path for the given module.
%% Example: module_dir(a.b.c) -> "a/b/"
%%
%% @spec module_dir(Module::any()) -> string()
module_dir(Module) ->
	filename:dirname(to_path(Module)) ++ "/".

%% @equiv package_dirs(Package, "")
package_dirs(Package) ->
    package_dirs(Package, "").

%% @doc Returns the filesystem (relative) directory paths found
%% for the given Package prefixed by Prefix.
%% Example: package_dirs(a.b, "./") -> ["./a/", "./a/b/"]
%%
%% @spec package_dirs(Package::atom() | Package::string()) -> list()
package_dirs(Package, Prefix) when is_atom(Package) ->
    case packages:is_segmented(Package) of
	true -> % seg1.seg2
	    compose_dirs(packages:split(Package), Prefix, []);
	false -> % single_seg
	    compose_dirs([atom_to_list(Package)], Prefix, [])
    end;
package_dirs(Package, Prefix) when is_list(Package) ->
    case is_integer(hd(Package)) of
	true -> % "seg1.seg2"
	    compose_dirs(packages:split(Package), Prefix, []);
	false -> % [seg1,seg2,seg3]
	    compose_dirs(lists:map(fun atom_to_list/1, Package), Prefix, [])
    end.

% The segments are appended recursively to their previous path
compose_dirs([], _, Dirs) ->
    lists:reverse(Dirs);
compose_dirs([Seg|R], Prefix, Dirs) ->
    Path = Prefix ++ Seg ++ "/",
    compose_dirs(R, Path, [Path | Dirs]).
