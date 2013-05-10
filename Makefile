all:
	rebar get-deps compile
	dialyzer --src ./src
	rebar xref ct skip_deps=true
docs:
	rebar doc
