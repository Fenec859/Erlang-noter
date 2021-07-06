run: compile
	cd ./ebin
	erl -pa ./ebin -run sync go -run noter start

compile:
	erl -pa ./ebin -make