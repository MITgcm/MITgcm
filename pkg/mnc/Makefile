#
#  $Header: /u/gcmpack/MITgcm/pkg/mnc/Makefile,v 1.11 2004/03/08 21:15:49 edhill Exp $
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

# TEMPLATE_FILES = \
#   MNC_RESHAPE_RS_D.F MNC_RESHAPE_RL_D.F \
#   MNC_RESHAPE_RS_R.F MNC_RESHAPE_RL_R.F
TEMPLATE_FILES = \
  MNC_CW_READWRITE_RS_D.F MNC_CW_READWRITE_RL_D.F  \
  MNC_CW_READWRITE_RS_R.F MNC_CW_READWRITE_RL_R.F  \
  MNC_CW_READWRITE_I_I.F  \
  MNC_CW_READWRITE_RS_I.F MNC_CW_READWRITE_RL_I.F  \
  MNC_CW_READWRITE_I_R.F  MNC_CW_READWRITE_I_D.F
TEMPLATE_OBJECTS = $(TEMPLATE_FILES:.F=.o)


templates: $(TEMPLATE_FILES) mnc_id_header.h

MNC_RESHAPE_RS_D.F: mnc_reshape.template
	cat $<  | sed -e 's/RX/RS/g' | sed -e 's/YY/D/g' > $@
MNC_RESHAPE_RL_D.F: mnc_reshape.template
	cat $<  | sed -e 's/RX/RL/g' | sed -e 's/YY/D/g' > $@
MNC_RESHAPE_RS_R.F: mnc_reshape.template
	cat $<  | sed -e 's/RX/RS/g' | sed -e 's/YY/R/g' > $@
MNC_RESHAPE_RL_R.F: mnc_reshape.template
	cat $<  | sed -e 's/RX/RL/g' | sed -e 's/YY/R/g' > $@

MNC_CW_READWRITE_RS_D.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/RS/g' | sed -e 's/__V/_RS/g' | sed -e 's/YY/D/g' > $@
MNC_CW_READWRITE_RL_D.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/RL/g' | sed -e 's/__V/_RL/g' | sed -e 's/YY/D/g' > $@

MNC_CW_READWRITE_RS_R.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/RS/g' | sed -e 's/__V/_RS/g' | sed -e 's/YY/R/g' > $@
MNC_CW_READWRITE_RL_R.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/RL/g' | sed -e 's/__V/_RL/g' | sed -e 's/YY/R/g' > $@

MNC_CW_READWRITE_I_I.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/I/g' | sed -e 's/__V/INTEGER/g' | sed -e 's/YY/I/g' > $@
MNC_CW_READWRITE_RS_I.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/RS/g' | sed -e 's/__V/_RS/g' | sed -e 's/YY/I/g' > $@
MNC_CW_READWRITE_RL_I.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/RL/g' | sed -e 's/__V/_RL/g' | sed -e 's/YY/I/g' > $@
MNC_CW_READWRITE_I_R.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/I/g' | sed -e 's/__V/INTEGER/g' | sed -e 's/YY/R/g' > $@
MNC_CW_READWRITE_I_D.F: mnc_cw_readwrite.template
	cat $< | sed -e 's/RX/I/g' | sed -e 's/__V/INTEGER/g' | sed -e 's/YY/D/g' > $@

tag = ../../doc/tag-index

mnc_id_header.h:
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
Clean:
	@make clean
clean:
	-rm -f *.o $(TEST_LINKS) $(TEST_HEADERS) $(TEST_FILES) $(TEST_OBJECTS)
	-rm -f mnc_test_???.F mnc_test_??? test_???.nc t_out_???
	-rm -f $(TEMPLATE_FILES) mnc_id_header.h
