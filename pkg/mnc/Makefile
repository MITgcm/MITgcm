
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
  mnc_dim.F \
  mnc_file.F \
  mnc_grid.F \
  mnc_init.F \
  mnc_utils.F \
  mnc_var.F

TEST_OBJECTS = $(TEST_FILES:.F=.o)
OBJECTS = $(FILES:.F=.o)

ALL_TEST = $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS)

noexe: $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(FILES) $(OBJECTS)
	echo "compiled"

EEPARAMS.h:
	ln -s ../../eesupp/inc/EEPARAMS.h .
utils.F:
	ln -s ../../eesupp/src/utils.F .

TEMPLATE_FILES = \
  MNC_RESHAPE_RS_D.F MNC_RESHAPE_RL_D.F \
  MNC_RESHAPE_RS_R.F MNC_RESHAPE_RL_R.F

templates: $(TEMPLATE_FILES)

MNC_RESHAPE_RS_D.F: mnc_reshape.template
	cat mnc_reshape.template | sed -e 's/RX/RS/g' | sed -e 's/YY/D/g' > $@
MNC_RESHAPE_RL_D.F: mnc_reshape.template
	cat mnc_reshape.template | sed -e 's/RX/RL/g' | sed -e 's/YY/D/g' > $@
MNC_RESHAPE_RS_R.F: mnc_reshape.template
	cat mnc_reshape.template | sed -e 's/RX/RS/g' | sed -e 's/YY/R/g' > $@
MNC_RESHAPE_RL_R.F: mnc_reshape.template
	cat mnc_reshape.template | sed -e 's/RX/RL/g' | sed -e 's/YY/R/g' > $@

test: mnc_test_001
	echo "running mnc_test_001 ..."
	./mnc_test_001 && ncdump test_001.nc | head -50


mnc_test_001: $(ALL_TEST) $(FILES) $(OBJECTS) mnc_test_001.o
	$(FC) -o mnc_test_001 mnc_test_001.o $(OBJECTS) $(TEST_OBJECTS) -lnetcdf


clean:
	-rm -f *.o $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS)
	-rm -f mnc_test_???.F mnc_test_??? test_???.nc
	-rm -f MNC_RESHAPE_R?_?.F
