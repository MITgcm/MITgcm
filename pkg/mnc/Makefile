#
# $Header: /u/gcmpack/MITgcm/pkg/mnc/Makefile,v 1.20 2008/07/20 13:57:49 jmc Exp $
# $Name:  $
#
#  A Makefile for MNC templates and testing
#

FC = f77
CPP = cpp

.SUFFIXES:
.SUFFIXES: .T .t .F .f .h .o

.t.h:
	cat $< > $@
.T.F:
	cat $< > $@
.F.f:
	$(CPP) $(DEFINES) > $@
.f.o:
	$(FC) -DMNC_TEST -c $<


TEST_LINKS =   EEPARAMS.h utils.F
TEST_HEADERS = PACKAGES_CONFIG.h CPP_OPTIONS.h SIZE.h
TEST_FILES =   mnc_test_utils.F
TEST_DEFINES = -DMNC_DEBUG

FILES = \
  mnc_dim.F \
  mnc_file.F \
  mnc_grid.F \
  mnc_init.F \
  mnc_utils.F \
  mnc_var.F \
  mnc_cwrapper.F

TEST_OBJECTS = $(TEST_FILES:.F=.o)
OBJECTS = $(FILES:.F=.o)

ALL_TEST = $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS)

all: templates

noexe: $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(FILES) $(OBJECTS)
	echo "compiled"

EEPARAMS.h:
	ln -s ../../eesupp/inc/EEPARAMS.h .
utils.F:
	ln -s ../../eesupp/src/utils.F .

TEMPLATE_FILES = \
  MNC_CW_READWRITE_RS.F \
  MNC_CW_READWRITE_RL.F \
  MNC_CW_READWRITE_I.F
TEMPLATE_OBJECTS = $(TEMPLATE_FILES:.F=.o)

mnc_netcdf_stubs.c: mnc_netcdf_stubs.h
	-./build_netcdf_stubs > tmp_mnc_netcdf_stubs.c
	-cpp -P < tmp_mnc_netcdf_stubs.c > mnc_netcdf_stubs.c
	-rm -f tmp_mnc_netcdf_stubs.c

mnc_netcdf_stubs.o: mnc_netcdf_stubs.c
	-gcc -c mnc_netcdf_stubs.c

templates: $(TEMPLATE_FILES) MNC_ID_HEADER.h

MNC_CW_READWRITE_RS.F: mnc_cw_readwrite.template
	cat mnc_cw_readwrite.template | sed -e 's/RX/RS/g' | sed -e 's/MNC2I/NINT/g' | sed -e 's/MNCI2//g' | sed -e 's/__V/_RS/g' > MNC_CW_READWRITE_RS.F
MNC_CW_READWRITE_RL.F: mnc_cw_readwrite.template
	cat mnc_cw_readwrite.template | sed -e 's/RX/RL/g' | sed -e 's/MNC2I/NINT/g' | sed -e 's/MNCI2//g' | sed -e 's/__V/_RL/g' > MNC_CW_READWRITE_RL.F
MNC_CW_READWRITE_I.F: mnc_cw_readwrite.template
	cat mnc_cw_readwrite.template | sed -e 's/RX/I/g' | sed -e 's/MNC2I//g' | sed -e 's/MNCI2/NINT/g' | sed -e 's/__V/INTEGER/g' > MNC_CW_READWRITE_I.F

tag = ../../doc/tag-index

MNC_ID_HEADER.h:
	-./parse_local_info > $@

test: all mnc_test_001 mnc_test_002 
	echo "running mnc_test_001 ..."
	./mnc_test_001 > t_out_001 && ncdump test_001.nc | head -20
	./mnc_test_002 > t_out_002 && tail -15 t_out_002

mnc_test_001: $(ALL_TEST) $(FILES) $(OBJECTS) mnc_test_001.o
	$(FC) -o mnc_test_001 mnc_test_001.o $(OBJECTS) $(TEST_OBJECTS) -lnetcdf

mnc_test_002: $(ALL_TEST) $(FILES) $(OBJECTS) mnc_test_002.o
	$(FC) -o mnc_test_002 mnc_test_002.o $(OBJECTS) $(TEST_OBJECTS) -lnetcdf

CLEAN:
	@make clean
	-rm -f mnc_netcdf_stubs.c

Clean:
	@make clean

testclean:
	-rm -f *.o $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS)
	-rm -f mnc_test_???.F mnc_test_??? test_???.nc t_out_???

clean: testclean
	-rm -f $(TEMPLATE_FILES) MNC_ID_HEADER.h mnc_id_header.h
	-rm -f mnc_netcdf_stubs.o

