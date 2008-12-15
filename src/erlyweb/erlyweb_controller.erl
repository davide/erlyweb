%% @author Yariv Sadan (yarivsblog@gmail.com, http://yarivsblog.com)
%%
%% @hidden
%% @doc This file contains basic CRUD controller logic. It's intended
%%  for demonstration purposes, but not for production use.

%% For license information see LICENSE.txt

-module(erlyweb_controller).
-author("Yariv Sadan (yarivsblog@gmail.com, http://yarivsblog.com)").

-export([
	 index/2,
	 list/2,
	 list/3,
	 new/2,
	 edit/3,
	 delete/3
	]).
	 
-define(RECORDS_PER_PAGE, 10).

index(_A, Model) ->
    {ewr, Model, list, [1]}.

list(A, Model) ->
    list(A, Model, 1).

list(A, Model, Page) when is_list(Page) ->
    list(A, Model, list_to_integer(Page));

list(A, Model, Page) when is_integer(Page) ->
    ModelModule = erlyweb:get_app_module(A, Model),
    Records = ModelModule:find_range((Page - 1) * ?RECORDS_PER_PAGE,
			       ?RECORDS_PER_PAGE),

    %% this function makes the 'edit' links in the record ids
    ToIoListFun =
	fun(Val, Field) ->
		case erlydb_field:name(Field) of
		    id ->
			Id = ModelModule:field_to_iolist(Val),
			erlyweb_html:a(
			  [case erlyweb:get_app_root(A) of
			       "/" -> "";
			       Root -> Root
			   end,
			   atom_to_list(Model),
			   <<"edit">>, Id], Id);
		    _ ->
			default
		end
	end,
    {data, {erlyweb:get_app_root(A),
	    atom_to_list(Model),
	    ModelModule:db_field_names_bin(),
	    ModelModule:to_iolist(Records, ToIoListFun)}}.

new(A, Model) ->
    ModelModule = erlyweb:get_app_module(A, Model),
    Rec = ModelModule:new(),
    new_or_edit(A, Model, Rec).

edit(A, Model, Id) ->
    ModelModule = erlyweb:get_app_module(A, Model),
    Rec = ModelModule:find_id(Id),
    new_or_edit(A, Model, Rec).

new_or_edit(A, Model, Record) ->
    ModelModule = erlyweb:get_app_module(A, Model),
    Fields = tl(ModelModule:db_fields()),
    Vals = tl(ModelModule:to_iolist(Record)),
    Combined = lists:zip(Fields, Vals),
    IdStr = case ModelModule:id(Record) of
		undefined -> [];
		Id -> integer_to_list(Id)
	    end,
    case yaws_arg:method(A) of
	'GET' ->
	    FieldData = [{erlydb_field:name_bin(Field),
			  erlydb_field:html_input_type(Field),
			  erlydb_field:modifier(Field),
			  Val} || {Field, Val} <- Combined],
	    {data, {erlyweb:get_app_root(A),
		    atom_to_list(Model),
		    IdStr,
		    yaws_arg:server_path(A),
		    FieldData}};
	'POST' ->
	    NewVals = yaws_api:parse_post(A),
	    Record1 = ModelModule:set_fields_from_strs(Record, NewVals),
	    ModelModule:save(Record1),
	    {ewr, Model, list}
    end.

delete(A, Model, Id) ->
    ModelModule = erlyweb:get_app_module(A, Model),
    case yaws_arg:method(A) of
	'GET' ->
	    Record = ModelModule:find_id(Id),
	    Fields = [erlydb_field:name_bin(Field) ||
			 Field <- ModelModule:db_fields()],
	    Vals = ModelModule:to_iolist(Record),
	    Combined =
		lists:zipwith(
		  fun(Field, Val) -> [Field, Val] end,
		  Fields, Vals),
			 
	    {data, {erlyweb:get_app_root(A),
		    atom_to_list(Model), Id,
		    Combined}};
	'POST' ->
	    ModelModule:delete_id(Id),
	    {ewr, Model, list}
    end.
    
