
FC = f77

.SUFFIXES:
.SUFFIXES: .T .t .F .h .o

.t.h:
	cat $< > $@
.T.F:
	cat $< > $@
.F.o:
	$(FC) -c $<


TEST_LINKS =   EEPARAMS.h utils.F
TEST_HEADERS = PACKAGES_CONFIG.h CPP_OPTIONS.h
TEST_FILES =   mnc_test_utils.F

FILES = \
  mnc_file.F \
  mnc_grid.F \
  mnc_init.F \
  mnc_utils.F \
  mnc_var.F

TEST_OBJECTS = $(TEST_FILES:.F=.o)
OBJECTS = $(FILES:.F=.o)

noexe: $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(FILES) $(OBJECTS)
	echo "compiled"

EEPARAMS.h:
	ln -s ../../eesupp/inc/EEPARAMS.h .
utils.F:
	ln -s ../../eesupp/src/utils.F .

test: mnc_test_001
	echo "running mnc_test_001 ..."
	./mnc_test_001
	echo "... done"

mnc_test_001: $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS) $(FILES) $(OBJECTS) mnc_test_001.o
	$(FC) -o mnc_test_001 mnc_test_001.o $(OBJECTS) $(TEST_OBJECTS) -lnetcdf


clean:
	-rm -f *.o $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS)
	-rm -f mnc_test_???.F mnc_test_???
