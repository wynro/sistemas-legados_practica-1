.SUFFIXES: .cob

# Nombre del ejecutable salida
PROG=main
# Los fuentes son todo fichero acabados en .c
SRCS=$(shell ls *.cob)
OTHER_SRC=$(shell find . -not -name 'main.cob' -name '*.cob')
# Compilador a usar
COMP=cobc -x

all: $(PROG)


$(PROG): $(PROG).cob $(OTHER_SRC)
	$(COMP) -o $@ $^

.PHONY: wall debug clean
wall: $(PROG).cob $(OTHER_SRC)
	$(COMP) -W -o $(PROG) $^

debug: $(PROG).cob $(OTHER_SRC)
	$(COMP) -fdebugging-line -o $(PROG) $^

clean:
	rm main


.PHONY: edit go goM gedit memcheck

exec:
	./$(PROG)

# Emacs masterrace
edit:
	@emacs -nw $(SRCS)

# Ejecutar el programa
go: $(PROG)
	./$(PROG) $(args)

# Ejecutar el fichero varias veces
goM: $(PROG)
	@cuenta=0;\
	while [ $$cuenta -lt $$t ] ; do \
	echo "==================================";\
	cuenta=`expr $$cuenta + 1`; \
	$(PROG) $(args); \
	done

# Abrir fuentes con gedit
gedit:
	@gedit *.cbl &

# Comprobacion de fugas de memoria
memcheck:
	valgrind --tool=memcheck -v --leak-check=full ./$(PROG) $(args)

tarea:
	tar chf "${LOGNAME}.tar" *
	gzip "${LOGNAME}.tar"
