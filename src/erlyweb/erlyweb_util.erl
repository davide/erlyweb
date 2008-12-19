%%
%% @doc This module contains a few utility functions useful
%% for ErlyWeb apps.
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com)]
%% @copyright Yariv Sadan 2006-2007


%% For license information see LICENSE.txt

-module(erlyweb_util).
-author("Yariv Sadan (yarivsblog@gmail.com, http://yarivsblog.com").

-export([
	 create_app/3,
	 create_component/4,
	 get_cookie/2,
	 indexify/2,
	 log/5
	]).

-define(Debug(Msg, Params), log(?MODULE, ?LINE, debug, Msg, Params)).
-define(Info(Msg, Params), log(?MODULE, ?LINE, info, Msg, Params)).
-define(Error(Msg, Params), log(?MODULE, ?LINE, error, Msg, Params)).

%% @hidden
log(Module, Line, Level, Msg, Params) ->
    io:format("~p:~p:~p: " ++ Msg, [Level, Module, Line] ++ Params),
    io:format("~n").

%% @hidden
create_app(AppName, Dir, Package) when is_atom(Package) ->
    create_app(AppName, Dir, atom_to_list(Package));

create_app(AppName, Dir, Package) ->
    case filelib:is_dir(Dir) of
	true ->
	    AppDir = Dir ++ "/" ++ AppName,
	    Dirs = [SrcDir, ComponentsDir, WebDir, _EbinDir, _LogDir]
		= [AppDir ++ "/src",
		   AppDir ++ "/src/components",
		   AppDir ++ "/www",
		   AppDir ++ "/ebin",
		   AppDir ++ "/log"],
	    lists:foreach(
	      fun(SubDir) ->
		      ?Info("creating ~p", [SubDir]),
		      case file:make_dir(SubDir) of
			  ok ->
			      ok;
			  Err ->
			      exit(Err)
		      end
	      end, [AppDir | Dirs]),
	
	    PackageRelDir = smerl:module_dir(Package),
	    PackageDir = AppDir ++ "/ebin/" ++ PackageRelDir ++ "/",
	    ?Info("creating package directory structure ~p", [PackageDir]),
	      case filelib:ensure_dir(PackageDir) of
		  ok ->
		      ok;
		  Err ->
		      exit(Err)
	      end,

	    Files =
		[{ComponentsDir ++ "/html_container_view.et",
		  html_container_view(AppName)},
		 {ComponentsDir ++ "/html_container_controller.erl",
		  html_container_controller(Package)},
		 {SrcDir ++ "/app_controller.erl",
		  app_controller(Package)},
		 {WebDir ++ "/index.html",
		  index(Package)},
		 {WebDir ++ "/style.css",
		  css()},
		  {SrcDir ++ "/boot.erl", boot_file(Package)},
		  {SrcDir ++ "/app.hrl", app_file(AppDir)},
		  {AppDir ++ "/yaws.conf", yaws_conf(Package, AppName)},
		  {AppDir ++ "/Makefile", makefile(AppName, PackageRelDir)},
		  {AppDir ++ "/Emakefile", emakefile(PackageRelDir)}],
	    lists:foreach(
	      fun({FileName, Bin}) ->
		      create_file(FileName, Bin)
	      end, Files),
	    ok;
	false ->
	    ?Error("~p isn't a directory", [Dir]),
	    exit({invalid_directory, Dir})
    end.

create_file(FileName, Bin) ->
    ?Info("creating ~p", [FileName]),
    case file:open(FileName, [read]) of
	{ok, File} ->
	    file:close(File),
	    exit({already_exists, FileName});
	_ ->
	    case file:write_file(FileName, Bin) of
		ok ->
		    ok;
		Err ->
		    exit({Err, FileName})
	    end
    end.

app_file(AppDir) ->
    Text =
	["-define(APP_PATH, \"" ++ AppDir ++ "\").\n"],
    iolist_to_binary(Text).
    
boot_file(Package) ->
    Text =
	["-module(", Package, ".boot).\n"
	 "-compile(export_all).\n"
	 "-include(\"app.hrl\").\n\n"
	 "start() ->\n"
	"\tprocess_flag(trap_exit, true),\n"
	"\tInets = (catch application:start(inets)),\n"
	"\tstart_phase(compile, normal, []),\n"
	"\t[{inets_start, Inets}].\n\n"
	"start_phase(compile, _Type, _Args) ->\n"
	"\tcompile(),\n"
	"\tok.\n\n"
	"compile() ->\n"
	"compile(default, []).\n\n"
	"compile_dev() ->\n"
	"\tcompile(default, [{auto_compile, true}]).\n\n"
	"compile_update() ->\n"
	"\tcompile(default, [{last_compile_time, auto}]).\n\n"
	"compile(AppDir) ->\n"
	"\tcompile(AppDir, []).\n\n"
	"\tcompile(AppDir, Opts) ->\n"
	"\t.erlyweb:compile(" ++ Package ++ ", compile_dir(AppDir),\n"
	"\t\t[{erlydb_driver, mysql}, {erlydb_timeout, 20000} | Opts]).\n\n"
	"create_component(Component) ->\n"
	"\tcreate_component(Component, default).\n\n"
	"create_component(Component, AppDir) ->\n"
	"\t.erlyweb:create_component(" ++ Package ++ ", compile_dir(AppDir), Component).\n\n"
	"compile_dir(auto) ->\n"
	"\t{ok, CWD} = .file:get_cwd(), CWD;\n"
	"compile_dir(default) ->\n"
	"\t?APP_PATH;\n"
	"compile_dir(Dir) ->\n"
	"\tDir.\n"],
    iolist_to_binary(Text).

yaws_conf(Package, AppName) ->
    Text =
	["logdir = log\n"
	"ebin_dir = ebin\n"
	"runmod = " ++ Package ++ ".boot\n\n"
	"<server " ++ AppName ++ ">\n"
	"\tport = 80\n"
	"\tlisten = 0.0.0.0\n"
	"\tdocroot = www\n"
	"\tappmods = <\"/\", erlyweb>\n"
	"\t<opaque>\n"
	"\t\tpackage = " ++ Package ++ "\n"
	"\t</opaque>\n"
	"</server>\n"],
    iolist_to_binary(Text).

emakefile(Package) ->
    Text =
	["{\"src/boot.erl\", [{outdir, \"./ebin/" ++ Package ++ "\"}]}.\n"],
    iolist_to_binary(Text).

makefile(AppName, PackageRelDir) ->
    Text =
	["all: code\n\n"
	"code: clean\n"
	"\terl -s make all load -s init stop\n\n"
	"run:	code\n"
	"\terl -yaws debug -run yaws --conf yaws.conf -yaws id " ++ AppName ++ ".webserver\n\n"
	"clean:\n"
	"\trm -fv ./ebin/*.beam ./ebin/" ++ PackageRelDir ++ "/*.beam log/* erl_crash.dump\n"],
    iolist_to_binary(Text).

app_controller(Package) ->
    Text =
	["-module(", Package, ".app_controller).\n"
	 "-export([hook/1]).\n\n"
	 "hook(A) ->\n"
	 "\t{phased, {ewc, A},\n"
	 "\t\tfun(_Ewc, Data, _PhasedVars) ->\n"
	 "\t\t\t{ewc, html_container, index, [A, {data, Data}]}\n"
	 "\t\tend}."],
    iolist_to_binary(Text).

html_container_controller(Package) ->
    Text =
	["-module(" ++ Package ++ ".html_container_controller).\n"
	 "-export([private/0, index/2]).\n\n"
	 "private() ->\n"
	 "\ttrue.\n\n"
	 "index(_A, Ewc) ->\n"
	 "\tEwc."],
    iolist_to_binary(Text).

html_container_view(AppName) ->
    Text =
	["<%@ index(Data) %>\n"
	 "<html>\n"
	 "<head>\n"
	 "<title>", AppName, "</title>\n"
	 "<link rel=\"stylesheet\" href=\"./style.css\""
	 " type=\"text/css\">\n"
	 "</style>\n"
	 "</head>\n"
	 "<body>\n"
	 "<div id=\"content\">\n"
	 "<h1>", AppName, " app</h1>\n"
	 "<% Data %>\n"
	 "<br>\n"
	 "<div width=\"80%\" align=\"right\">\n"
	 "powered by <a href=\"http://erlyweb.org\">ErlyWeb</a>"
	 " / <a href=\"http://yaws.hyber.org\">Yaws</a>\n"
	 "</div>\n"
	 "</div>\n"
	 "</body>\n"
	 "</html>\n"],
    iolist_to_binary(Text).


index(AppName) ->	
    Text =
	["<html>\n"
	 "<head>\n"
	 "<link rel=\"stylesheet\" href=\"./style.css\">\n"
	 "<title>", AppName, "</title>\n</head>\n",
	 "<body>\n"
	 "<div id=\"content\">\n"
	 "Welcome to '", AppName, "', your new "
	 "<a href=\"http://erlyweb.org\">ErlyWeb</a> "
	 "app.<br><br>\n"
	 "Let the <a href=\"http://erlang.org\">Erlang</a> "
	 "hacking begin!\n"
	 "</div>\n",
	 "</body>\n</html>"],
    iolist_to_binary(Text).

css() ->
    Text =
	"body {\n"
	"  font-family: arial, verdana,  helvetica, sans-serif;\n"
	"  color: #353535;\n"
	"  margin:10px 0px; padding:0px;\n"
	"  text-align:center;\n"
	"}\n\n"
	"#Content {\n"
	"  width:80%;\n"
	"  margin:0px auto;\n"
	"  text-align:left;\n"
	"  padding:15px;\n"
	"} \n"
	"H1 {font-size: 1.5em;}",
    iolist_to_binary(Text).

magic_declaration("", _) ->
    "";
magic_declaration(MagicStr, controller) ->
    "-erlyweb_magic(" ++ MagicStr ++"_controller).";
magic_declaration(MagicStr, {erltl, off}) ->
    "-erlyweb_magic(" ++ MagicStr ++"_view).";
magic_declaration(MagicStr, {erltl, on}) ->
    "<%~ -erlyweb_magic(" ++ MagicStr ++ "_view). %>".

view_declaration(Package, ComponentName, {ertl, off}) ->
    "-module("++ Package ++"." ++ ComponentName ++ "_view).\n";
view_declaration(_Package, _ComponentName, {ertl, on}) ->
    "".

view_filename(ComponentName, {ertl, off}) ->
    ComponentName ++ "_view.erl";
view_filename(ComponentName, {ertl, on}) ->
    ComponentName ++ "_view.et".

model_filename(_ComponentName, {model, off}) ->
    "";
model_filename(ComponentName, {model, on}) ->
    ComponentName ++ ".erl".
    
%% @hidden
create_component(Package, AppDir, ComponentName, Options) when is_atom(Package) ->
    create_component(atom_to_list(Package), AppDir, ComponentName, Options);

create_component(Package, AppDir, ComponentName, Options) ->
    {Magic, Model, Erltl} = {proplists:get_value(magic, Options, on),
			     proplists:get_value(model, Options, on),
			     proplists:get_value(erltl, Options, off)},

     if (Magic == on) andalso (Model == off) ->
 	    exit({bad_options, "Can't have magic without a model."});
	true ->
	     void
     end,
     
    case (filelib:ensure_dir(AppDir ++ "/src/components/")) of
	ok -> void;
	{error, Reason} ->
		exit({missing_directory, AppDir ++ "/src/components/", Reason})
    end,

    MagicStr = if Magic == on ->
		       "erlyweb";
		  Magic == off ->
		       "";
		  true ->
		       if is_atom(Magic) ->
			       atom_to_list(Magic);
			  true ->
			       Magic
		       end
	       end,
    
    %% Remove empty filenames from the list.
    Files = lists:filter(fun({"", _Bin}) -> false;
			    (_) -> true end,
			 [{model_filename(ComponentName, {model, Model}),
			   "-module(" ++ Package ++ "." ++ ComponentName ++ ")."},
			  {ComponentName ++ "_controller.erl",
			   "-module(" ++ Package ++ "." ++ ComponentName ++ "_controller).\n" ++
			   magic_declaration(MagicStr, controller)},
			  {view_filename(ComponentName, {ertl, Erltl}),
			   view_declaration(Package, ComponentName, {ertl, Erltl}) ++
			   magic_declaration(MagicStr, {erltl, Erltl})}]),
    
    lists:foreach(
      fun({FileName, Text}) ->
	      create_file(AppDir ++ "/src/components/" ++ FileName, Text)
      end, Files).

%% @doc Get the cookie's value from the arg.
%% @equiv yaws_api:find_cookie_val(Name, yaws_headers:cookie(A))
%%
%% @spec get_cookie(Name::string(), A::arg()) -> string()
get_cookie(Name, A) ->
    yaws_api:find_cookie_val(
      Name, yaws_headers:cookie(A)).

%% @doc Translate requests such as '/foo/bar' to '/foo/index/bar' for the given
%% list of components. This function is useful for rewriting the Arg in the
%% app controller prior to handling incoming requests.
%%
%% @deprecated This function is deprecated. Implement catch_all/3 in your
%% controllers instead.
%%
%% @spec indexify(A::arg(), ComponentNames::[string()]) -> arg()
indexify(A, ComponentNames) ->
    Appmod = yaws_arg:appmoddata(A),
    Sp = yaws_arg:server_path(A),

    Appmod1 = indexify1(Appmod, ComponentNames),
    A1 = yaws_arg:appmoddata(A, Appmod1),

    {SpRoot, _} = lists:split(length(Sp) - length(Appmod), Sp),
    yaws_arg:server_path(A1, SpRoot ++ Appmod1).
     

indexify1(S1, []) -> S1;
indexify1(S1, [Prefix | Others]) ->
    case indexify2(S1, [$/ | Prefix]) of
	stop -> S1;
	{stop, Postfix} ->
	    [$/ | Prefix] ++ "/index" ++ Postfix;
	next ->
	    indexify1(S1, Others)
    end.

indexify2([], []) -> stop;
indexify2([$/ | _] = Postfix, []) -> {stop, Postfix};
indexify2([C1 | Rest1], [C1 | Rest2]) ->
    indexify2(Rest1, Rest2);
indexify2(_, _) -> next.
