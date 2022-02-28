include make.inc

EXEC1 = init_param_bio
EXEC2 = ptrb_param_bio

OBJS1 = init_param_bio.o
OBJS2 = ptrb_param_bio.o

all: $(EXEC1) $(EXEC2)

$(EXEC1): $(OBJS1)
	$(CF90) $(FFLAGS) $(OBJS1) -o $@

$(EXEC2): $(OBJS2)
	$(CF90) $(FFLAGS) $(OBJS2) -o $@

init_param_bio.o: init_param_bio.F90
	$(CF90) $(FFLAGS) -c $< -o $@

ptrb_param_bio.o: ptrb_param_bio.F90
	$(CF90) $(FFLAGS) -c $< -o $@

clean:
	rm $(EXEC1) $(EXEC2) *.o
