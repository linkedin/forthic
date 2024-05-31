FORTRAN_STD=f2003
OBJ = constants.o core_words.o forrth_entry_sr.o forrth_sr.o forrth_types.o \
      items.o parse_input.o process_input.o

forrthtran: forrthtran.f90 $(OBJ)
	gfortran -g -std=$(FORTRAN_STD) -o forrthtran $< $(OBJ)

%.o:%.f90
	gfortran -g -std=$(FORTRAN_STD) -c $<


constants.mod: constants.o

forrth_types.mod: constants.mod forrth_types.o

parse_input_sr.mod: constants.mod forrth_types.mod parse_input.o

items.mod: constants.mod forrth_types.mod items.o

forrth_sr.mod: constants.mod forrth_types.mod items.mod forrth_sr.o

core_words.mod: core_words.o

forrth_entry_sr.mod: constants.mod forrth_types.mod items.mod forrth_entry_sr.o

process_input_sr.mod: forrth_types.mod parse_input_sr.mod forrth_sr.mod \
                      items.mod process_input_sr.mod core_words.mod

core_words.o: constants.mod forrth_types.mod parse_input_sr.mod \
              items.mod forrth_sr.mod 

clean:
	rm *.o *.mod forrthtran