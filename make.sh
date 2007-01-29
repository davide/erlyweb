#! /bin/sh
erl -noshell -eval 'make:all(), filelib:fold_files("src/", ".+\.et$", true, fun(F, _Acc) -> erltl:compile(F, [{outdir, "ebin"}, debug_info, show_errors, show_warnings]) end, []).' -pa ebin -s erlang halt