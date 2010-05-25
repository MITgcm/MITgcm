#
#  $Header: /u/gcmpack/MITgcm/pkg/regrid/Makefile,v 1.2 2010/05/25 23:30:34 jmc Exp $
#
#  A Makefile for REGRID templates
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


all: templates

TEMPLATE_FILES = \
  regrid_scalar_out_RS.F \
  regrid_scalar_out_RL.F
TEMPLATE_OBJECTS = $(TEMPLATE_FILES:.F=.o)

templates: $(TEMPLATE_FILES) 

regrid_scalar_out_RS.F: regrid_scalar_out.template
	cat regrid_scalar_out.template | sed -e 's/RX/RS/g' | sed -e 's/__V/_RS/g' > regrid_scalar_out_RS.F
regrid_scalar_out_RL.F:  regrid_scalar_out.template
	cat regrid_scalar_out.template | sed -e 's/RX/RL/g' | sed -e 's/__V/_RL/g' > regrid_scalar_out_RL.F


CLEAN:
	@make clean

Clean:
	@make clean

clean: 
	-rm -f $(TEMPLATE_FILES)

clean_old:
