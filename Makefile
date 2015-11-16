install:
	mkdir -p bin
	raco exe -o bin/Beachalyzer src/Beachalyzer.rkt
	raco exe -o bin/FileGenerator src/FileGenerator.rkt
