# Makefile created Thu Oct 9 11:44:02 EDT 1997
# on machine xolas1
# by logon id cnh
# command line MkMake.sh ../src/cadj.F ../src/cg2d.F ../src/control.F ../src/diags.F ../src/eparam.F ../src/g_calc.F ../src/get_map.F ../src/inc_tracer.F ../src/initialise.F ../src/io.F ../src/main.F ../src/model.F ../src/pfind.F ../src/plot_field.F ../src/print.F ../src/prog_utilities.F ../src/read.F ../src/rediTensor.F ../src/set_defaults.F ../src/update_rho.F ../src/update_s.F ../src/update_t.F ../src/update_uv.F ../src/update_w.F ../src/write.F
#
# Name of executable to create
EXE=MITgcm-4

# Common options
CPP=cpp

# Sunpro F90 Parallel processing options.            
# Note: There is a problem associated with the I/O done in
#       the routine io.F. I have not been able to fix the problem. Instead
#       io.F is compiled using F77 and the problem does not occur.
#       The problem occurs if the F90 compiler is used. The OPEN statement
#       frequently generates segmentation violations. If the routine
#       update_t.F only updates less than the first 32792 bytes of the array gt 
#       the problem stops. Also if in gt you skip the elements of gt
#       that are a multiple of 32792 from the start of the array then the problem
#       does not occur. 
#       ../src/io.F prize for the person who finds the bug. CNH
F90COMP=f90
F90LINK=f90
F77COMP=f77
F77COMOPTS=-c -fast -dalign -e -explcitpar -I../src
COMOPTS=-c -fast -dalign -e -fixed -explcitpar -I../src
LNKOPTS=-fast -dalign -explcitpar -o $(EXE)

# Sunpro F90 Single processor "performance" options
CPPOPTS=-P
F90COMP=f90
F90LINK=f90
F77COMP=f90
F77COMOPTS=-c -fast -dalign -e -fixed -I../src
COMOPTS=-c -fast -dalign -e -fixed -I../src
LNKOPTS=-fast -dalign -o $(EXE)

# Sunpro F90 Debug options
F90COMP=f90
F90LINK=f90
F77COMP=f90
F77COMOPTS=-ladebug -fast -O5 -notransform_loops -extend_source -I../src -c
COMOPTS=-r8 -fpconst -O3 -notransform_loops -nospeculate -extend_source -I../src -c
LNKOPTS= -O2 -automatic -call_shared -o $(EXE)
LNKOPTS= -what -O3 -notransform_loops -nospeculate -automatic -call_shared -o $(EXE)
# END xolas.lcs.mit.edu
OBJS = \
cadj.o \
cg2d.o \
control.o \
diags.o \
eparam.o \
find_ph.o \
g_calc.o \
get_map.o \
inc_tracer.o \
initialise.o \
io.o \
main.o \
model.o \
pfind.o \
plot_field.o \
print.o \
prog_utilities.o \
read.o \
rediTensor.o \
set_defaults.o \
update_rho.o \
update_s.o \
update_t.o \
update_uv.o \
update_w.o \
write.o
 
$(EXE): $(OBJS)
	$(F90LINK) $(LNKOPTS) $(OBJS)
cadj.o: ../src/cadj.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/cadj.F > cadj.f
	$(F90COMP) $(COMOPTS) cadj.f
cg2d.o: ../src/cg2d.F ../src/CG2DA.h ../src/CG2DZ.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/cg2d.F > cg2d.f
	$(F90COMP) $(COMOPTS) cg2d.f
control.o: ../src/control.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/control.F > control.f
	$(F90COMP) $(COMOPTS) control.f
diags.o: ../src/diags.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/DIAGS.h ../src/EXTERNAL.h ../src/GRID.h ../src/MASKS.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/diags.F > diags.f
	$(F90COMP) $(COMOPTS) diags.f
find_ph.o: ../src/find_ph.F ../src/AJAINF.h ../src/CG2DA.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/find_ph.F > find_ph.f
	$(F90COMP) $(COMOPTS) find_ph.f
eparam.o: ../src/eparam.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EPARAM.h ../src/EXTERNAL.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h ../src/STRINGS.h
	$(CPP) $(CPPOPTS) -P  ../src/eparam.F > eparam.f
	$(F90COMP) $(COMOPTS) eparam.f
g_calc.o: ../src/g_calc.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/g_calc.F > g_calc.f
	$(F90COMP) $(COMOPTS) g_calc.f
get_map.o: ../src/get_map.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h
	$(CPP) $(CPPOPTS) -P  ../src/get_map.F > get_map.f
	$(F90COMP) $(COMOPTS) get_map.f
inc_tracer.o: ../src/inc_tracer.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/inc_tracer.F > inc_tracer.f
	$(F90COMP) $(COMOPTS) inc_tracer.f
initialise.o: ../src/initialise.F ../src/AJAINF.h ../src/CG2DA.h ../src/CG2DZ.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/DIAGS.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/POLYEOS.h ../src/SIZE.h ../src/STRINGS.h
	$(CPP) $(CPPOPTS) -P  ../src/initialise.F > initialise.f
	$(F90COMP) $(COMOPTS) initialise.f
io.o: ../src/io.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EXTERNAL.h
	$(CPP) $(CPPOPTS) -P  ../src/io.F > io.f
	$(F77COMP) $(F77COMOPTS) io.f
main.o: ../src/main.F ../src/AJAINF.h ../src/CG2DA.h ../src/CG2DZ.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/POLYEOS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/main.F > main.f
	$(F90COMP) $(COMOPTS) main.f
model.o: ../src/model.F ../src/AJAINF.h ../src/CG2DA.h ../src/CG2DZ.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EXTERNAL.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/model.F > model.f
	$(F90COMP) $(COMOPTS) model.f
pfind.o: ../src/pfind.F ../src/AJAINF.h ../src/CG2DA.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/pfind.F > pfind.f
	$(F90COMP) $(COMOPTS) pfind.f
plot_field.o: ../src/plot_field.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h
	$(CPP) $(CPPOPTS) -P  ../src/plot_field.F > plot_field.f
	$(F90COMP) $(COMOPTS) plot_field.f
print.o: ../src/print.F ../src/AJAINF.h ../src/CG2DA.h ../src/CG2DZ.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h ../src/STRINGS.h
	$(CPP) $(CPPOPTS) -P  ../src/print.F > print.f
	$(F90COMP) $(COMOPTS) print.f
prog_utilities.o: ../src/prog_utilities.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EXTERNAL.h
	$(CPP) $(CPPOPTS) -P  ../src/prog_utilities.F > prog_utilities.f
	$(F90COMP) $(COMOPTS) prog_utilities.f
read.o: ../src/read.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EXTERNAL.h ../src/MASKS.h ../src/PARAMS.h ../src/SIZE.h ../src/STRINGS.h
	$(CPP) $(CPPOPTS) -P  ../src/read.F > read.f
	$(F90COMP) $(COMOPTS) read.f
rediTensor.o: ../src/rediTensor.F ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/MASKS.h ../src/OPERATORS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/rediTensor.F > rediTensor.f
	$(F90COMP) $(COMOPTS) rediTensor.f
set_defaults.o: ../src/set_defaults.F ../src/AJAINF.h ../src/CG2DA.h ../src/CG2DZ.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EPARAM.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h ../src/STRINGS.h
	$(CPP) $(CPPOPTS) -P  ../src/set_defaults.F > set_defaults.f
	$(F90COMP) $(COMOPTS) set_defaults.f
update_rho.o: ../src/update_rho.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/POLYEOS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/update_rho.F > update_rho.f
	$(F90COMP) $(COMOPTS) update_rho.f
update_s.o: ../src/update_s.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/update_s.F > update_s.f
	$(F90COMP) $(COMOPTS) update_s.f
update_t.o: ../src/update_t.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/FORCING.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/update_t.F > update_t.f
	$(F90COMP) $(COMOPTS) update_t.f
update_uv.o: ../src/update_uv.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/update_uv.F > update_uv.f
	$(F90COMP) $(COMOPTS) update_uv.f
update_w.o: ../src/update_w.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/GRID.h ../src/MASKS.h ../src/OLDG.h ../src/OPERATORS.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/update_w.F > update_w.f
	$(F90COMP) $(COMOPTS) update_w.f
write.o: ../src/write.F ../src/AJAINF.h ../src/CPP_MACROS.h ../src/CPP_OPTIONS.h ../src/EXTERNAL.h ../src/OLDG.h ../src/PARAMS.h ../src/SIZE.h
	$(CPP) $(CPPOPTS) -P  ../src/write.F > write.f
	$(F90COMP) $(COMOPTS) write.f

rcs:
	ci -l *.F *.h Makefile Makefile.* *.sh README

rebuild:
	touch ../src/*.F ../src/*.h
	make -f Makefile

arch:
	ci -l *.F *.h Makefile
	rcp *.F *.h Makefile cnh@sea.mit.edu:/data34e/cnh/models/F77ishImplmentation/Src
	rsh sea.mit.edu -l cnh 'cd /data34e/cnh/models/F77ishImplmentation/Src; ci -l *.F *.h Makefile'

paiute:
	rcp *.F *.h herbaut@paiute.ucar.edu:cnh/rmtank/src
	rcp Makefile.paiute herbaut@paiute.ucar.edu:cnh/rmtank/src/Makefile
	rcp topog Lev* windx.b windy.b herbaut@paiute.ucar.edu:tmp/rmtank/bin

ouray: ouray_code

ouray_code:
	rcp *.F *.h cnh@ouray.ucar.edu:cnh/rmtank/src
	rcp Makefile.ouray cnh@ouray.ucar.edu:cnh/rmtank/src/Makefile

mario: mario_code

mario_code:
	rsh mario.psc.edu -l marshalj mkdir -p tmp/rmtank/bin
	rsh mario.psc.edu -l marshalj mkdir -p tmp/rmtank/src
	rcp *.F *.h marshalj@mario.psc.edu:tmp/rmtank/src
	rcp Makefile.mario marshalj@mario.psc.edu:tmp/rmtank/src/Makefile

