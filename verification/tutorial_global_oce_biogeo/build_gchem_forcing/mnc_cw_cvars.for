

  








CBOP
C !ROUTINE: CPP_OPTIONS.h
C !INTERFACE:
C #include "CPP_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | main CPP options file for the model:
C | Control which optional features to compile in model/src code.
C *==================================================================*
CEOP

C CPP flags controlling particular source code features

C-- Forcing code options:

C o Shortwave heating as extra term in APPLY_FORCING_T (apply_forcing.F)


C o Include/exclude Geothermal Heat Flux at the bottom of the ocean


C o Allow to account for heating due to friction (and momentum dissipation)


C o Allow mass source or sink of Fluid in the interior
C   (3-D generalisation of oceanic real-fresh water flux)


C o Include pressure loading code


C o Include/exclude balancing surface forcing fluxes code


C o Include/exclude balancing surface forcing relaxation code


C o Include/exclude checking for negative salinity


C-- Options to discard parts of the main code:

C o Exclude/allow external forcing-fields load
C   this allows to read & do simple linear time interpolation of oceanic
C   forcing fields, if no specific pkg (e.g., EXF) is used to compute them.

C   If defined, use same method (with pkg/autodiff compiled or not) for checking
C   when to load new reccord ; by default, use simpler method with pkg/autodiff.


C o Include/exclude phi_hyd calculation code


C o Include/exclude sound speed calculation code
C o (Note that this is a diagnostic from Del Grasso algorithm, not derived
C    from EOS)


C-- Vertical mixing code options:

C o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT


C o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT_INI, turned off by
C   default because it is an unpopular historical left-over


C o Include/exclude call to S/R CALC_DIFFUSIVITY


C o Allow full 3D specification of vertical diffusivity


C o Allow latitudinally varying BryanLewis79 vertical diffusivity


C o Exclude/allow partial-cell effect (physical or enhanced) in vertical mixing
C   this allows to account for partial-cell in vertical viscosity and diffusion,
C   either from grid-spacing reduction effect or as artificially enhanced mixing
C   near surface & bottom for too thin grid-cell


C o Exclude/allow to use isotropic 3-D Smagorinsky viscosity as diffusivity
C   for tracers (after scaling by constant Prandtl number)


C-- Time-stepping code options:

C o Include/exclude combined Surf.Pressure and Drag Implicit solver code


C o Include/exclude Implicit vertical advection code


C o Include/exclude AdamsBashforth-3rd-Order code


C o Include/exclude Quasi-Hydrostatic Stagger Time-step AdamsBashforth code


C-- Model formulation options:

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that grid-cell thickness (hFactors) varies with time

C o Disable code for rStar coordinate and/or code for Sigma coordinate
c#define DISABLE_RSTAR_CODE
c#define DISABLE_SIGMA_CODE

C o Include/exclude nonHydrostatic code


C o Include/exclude GM-like eddy stress in momentum code


C-- Algorithm options:

C o Include/exclude code for Non Self-Adjoint (NSA) conjugate-gradient solver


C o Include/exclude code for single reduction Conjugate-Gradient solver


C o Choices for implicit solver routines solve_*diagonal.F
C   The following has low memory footprint, but not suitable for AD

C   The following one suitable for AD but does not vectorize


C   Implementation alternative (might be faster on some platforms ?)


C-- Retired code options:

C-  These 2 flags: ISOTROPIC_COS_SCALING & COSINEMETH_III have no effect
C   here as they are reset in GAD_OPTIONS.h and in MOM_COMMON_OPTIONS.h
C   for tracer diffusivity and momentum viscosity respectively

C o Use "OLD" UV discretisation near boundaries (*not* recommended)
C   Note - only works with pkg/mom_fluxform and "no_slip_sides=.FALSE."
C          because the old code did not have no-slip BCs


C o Use LONG.bin, LATG.bin, etc., initialization for ini_curviliear_grid.F
C   Default is to use "new" grid files (OLD_GRID_IO undef) but OLD_GRID_IO
C   is still useful with, e.g., single-domain curvilinear configurations.


C o Use old EXTERNAL_FORCING_U,V,T,S subroutines (for backward compatibility)


C-- Other option files:

C o Execution environment support options


CBOP
C     !ROUTINE: CPP_EEOPTIONS.h
C     !INTERFACE:
C     include "CPP_EEOPTIONS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP\_EEOPTIONS.h                                         |
C     *==========================================================*
C     | C preprocessor "execution environment" supporting        |
C     | flags. Use this file to set flags controlling the        |
C     | execution environment in which a model runs - as opposed |
C     | to the dynamical problem the model solves.               |
C     | Note: Many options are implemented with both compile time|
C     |       and run-time switches. This allows options to be   |
C     |       removed altogether, made optional at run-time or   |
C     |       to be permanently enabled. This convention helps   |
C     |       with the data-dependence analysis performed by the |
C     |       adjoint model compiler. This data dependency       |
C     |       analysis can be upset by runtime switches that it  |
C     |       is unable to recoginise as being fixed for the     |
C     |       duration of an integration.                        |
C     |       A reasonable way to use these flags is to          |
C     |       set all options as selectable at runtime but then  |
C     |       once an experimental configuration has been        |
C     |       identified, rebuild the code with the appropriate  |
C     |       options set at compile time.                       |
C     *==========================================================*
CEOP

C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C=== Macro related options ===
C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working set size.
C     However, on vector CRAY systems this degrades performance.
C     Enable to switch REAL4_IS_SLOW from genmake2 (with LET_RS_BE_REAL4):


C--   Control use of "double" precision constants.
C     Use D0 where it means REAL*8 but not where it means REAL*16


C=== IO related options ===
C--   Flag used to indicate whether Fortran formatted write
C     and read are threadsafe. On SGI the routines can be thread
C     safe, on Sun it is not possible - if you are unsure then
C     undef this option.


C--   Flag used to indicate whether Binary write to Local file (i.e.,
C     a different file for each tile) and read are thread-safe.


C--   Flag to turn off the writing of error message to ioUnit zero


C--   Flag to turn on old default of opening scratch files with the
C     STATUS='SCRATCH' option. This method, while perfectly FORTRAN-standard,
C     caused filename conflicts on some multi-node/multi-processor platforms
C     in the past and has been replace by something (hopefully) more robust.


C--   Flag defined for eeboot_minimal.F, eeset_parms.F and open_copy_data_file.F
C     to write STDOUT, STDERR and scratch files from process 0 only.
C WARNING: to use only when absolutely confident that the setup is working
C     since any message (error/warning/print) from any proc <> 0 will be lost.


C=== MPI, EXCH and GLOBAL_SUM related options ===
C--   Flag turns off MPI_SEND ready_to_receive polling in the
C     gather_* subroutines to speed up integrations.


C--   Control MPI based parallel processing
CXXX We no longer select the use of MPI via this file (CPP_EEOPTIONS.h)
CXXX To use MPI, use an appropriate genmake2 options file or use
CXXX genmake2 -mpi .
CXXX #undef  ALLOW_USE_MPI

C--   Control use of communication that might overlap computation.
C     Under MPI selects/deselects "non-blocking" sends and receives.

C--   Control use of communication that is atomic to computation.
C     Under MPI selects/deselects "blocking" sends and receives.


C--   Control XY periodicity in processor to grid mappings
C     Note: Model code does not need to know whether a domain is
C           periodic because it has overlap regions for every box.
C           Model assume that these values have been
C           filled in some way.


C--   disconnect tiles (no exchange between tiles, just fill-in edges
C     assuming locally periodic subdomain)


C--   Always cumulate tile local-sum in the same order by applying MPI allreduce
C     to array of tiles ; can get slower with large number of tiles (big set-up)


C--   Alternative way of doing global sum without MPI allreduce call
C     but instead, explicit MPI send & recv calls. Expected to be slower.


C--   Alternative way of doing global sum on a single CPU
C     to eliminate tiling-dependent roundoff errors. Note: This is slow.


C=== Other options (to add/remove pieces of code) ===
C--   Flag to turn on checking for errors from all threads and procs
C     (calling S/R STOP_IF_ERROR) before stopping.


C--   Control use of communication with other component:
C     allow to import and export from/to Coupler interface.


C--   Activate some pieces of code for coupling to GEOS AGCM


C=== And define Macros ===

CBOP
C     !ROUTINE: CPP_EEMACROS.h
C     !INTERFACE:
C     include "CPP_EEMACROS.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP_EEMACROS.h
C     *==========================================================*
C     | C preprocessor "execution environment" supporting
C     | macros. Use this file to define macros for  simplifying
C     | execution environment in which a model runs - as opposed
C     | to the dynamical problem the model solves.
C     *==========================================================*
CEOP



C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C     Flag used to indicate which flavour of multi-threading
C     compiler directives to use. Only set one of these.
C     USE_SOLARIS_THREADING  - Takes directives for SUN Workshop
C                              compiler.
C     USE_KAP_THREADING      - Takes directives for Kuck and
C                              Associates multi-threading compiler
C                              ( used on Digital platforms ).
C     USE_IRIX_THREADING     - Takes directives for SGI MIPS
C                              Pro Fortran compiler.
C     USE_EXEMPLAR_THREADING - Takes directives for HP SPP series
C                              compiler.
C     USE_C90_THREADING      - Takes directives for CRAY/SGI C90
C                              system F90 compiler.












C--   Define the mapping for the _BARRIER macro
C     On some systems low-level hardware support can be accessed through
C     compiler directives here.


C--   Define the mapping for the BEGIN_CRIT() and  END_CRIT() macros.
C     On some systems we simply execute this section only using the
C     master thread i.e. its not really a critical section. We can
C     do this because we do not use critical sections in any critical
C     sections of our code!


C--   Define the mapping for the BEGIN_MASTER_SECTION() and
C     END_MASTER_SECTION() macros. These are generally implemented by
C     simply choosing a particular thread to be "the master" and have
C     it alone execute the BEGIN_MASTER..., END_MASTER.. sections.


CcnhDebugStarts
C      Alternate form to the above macros that increments (decrements) a counter each
C      time a MASTER section is entered (exited). This counter can then be checked in barrier
C      to try and detect calls to BARRIER within single threaded sections.
C      Using these macros requires two changes to Makefile - these changes are written
C      below.
C      1 - add a filter to the CPP command to kill off commented _MASTER lines
C      2 - add a filter to the CPP output the converts the string N EWLINE to an actual newline.
C      The N EWLINE needs to be changes to have no space when this macro and Makefile changes
C      are used. Its in here with a space to stop it getting parsed by the CPP stage in these
C      comments.
C      #define IF ( a .EQ. 1 ) THEN  IF ( a .EQ. 1 ) THEN  N EWLINE      CALL BARRIER_MS(a)
C      #define ENDIF    CALL BARRIER_MU(a) N EWLINE        ENDIF
C      'CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  grep -v '^[cC].*_MASTER' | cpp  -traditional -P'
C      .F.f:
C      $(CPP) $(DEFINES) $(INCLUDES) |  sed 's/N EWLINE/\n/' > $@
CcnhDebugEnds

C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working
C     set size. However, on vector CRAY systems this degrades
C     performance.
C- Note: global_sum/max macros were used to switch to  JAM routines (obsolete);
C  in addition, since only the R4 & R8 S/R are coded, GLOBAL RS & RL macros
C  enable to call the corresponding R4 or R8 S/R.






C- Note: a) exch macros were used to switch to  JAM routines (obsolete)
C        b) exch R4 & R8 macros are not practically used ; if needed,
C           will directly call the corrresponding S/R.


C--   Control use of JAM routines for Artic network (no longer supported)
C     These invoke optimized versions of "exchange" and "sum" that
C     utilize the programmable aspect of Artic cards.
CXXX No longer supported ; started to remove JAM routines.
CXXX #ifdef LETS_MAKE_JAM
CXXX #define CALL GLOBAL_SUM_R8 ( a, b) CALL GLOBAL_SUM_R8_JAM ( a, b)
CXXX #define CALL GLOBAL_SUM_R8 ( a, b ) CALL GLOBAL_SUM_R8_JAM ( a, b )
CXXX #define CALL EXCH_XY_RS ( a, b ) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define CALL EXCH_XY_RL ( a, b ) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define CALL EXCH_XYZ_RS ( a, b ) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #define CALL EXCH_XYZ_RL ( a, b ) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #endif

C--   Control use of "double" precision constants.
C     Use d0 where it means REAL*8 but not where it means REAL*16


C--   Substitue for 1.D variables
C     Sun compilers do not use 8-byte precision for literals
C     unless .Dnn is specified. CRAY vector machines use 16-byte
C     precision when they see .Dnn which runs very slowly!


C--   Set the format for writing processor IDs, e.g. in S/R eeset_parms
C     and S/R open_copy_data_file. The default of I9.9 should work for
C     a long time (until we will use 10e10 processors and more)


C--   Set the format for writing ensemble task IDs in S/R eeset_parms
C     and S/R open_copy_data_file.


C--   Set ACTION= in OPEN instruction for input file (before doing IO)
C     leave it empty (if EXCLUDE_OPEN_ACTION) or set it to proper value






C-  Place where multi-pkg header file ECCO_CPPOPTIONS.h used to be included







CEH3 package-specific options go here

C     #define MNC_DEBUG

C     #define MNC_DEBUG_GTYPE  

C     The following are for back-wards compatibility with "old-style" mnc
C     file names:
C
C     #define MNC_WRITE_OLDNAMES
C     #define MNC_READ_OLDNAMES

C     These are the default minimum number of characters used for the
C     per-file and per-tile file names




CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_WRITE_CVAR

C     !INTERFACE:
      SUBROUTINE MNC_CW_WRITE_CVAR(
     I     fname,
     I     cvname,
     I     fid,
     I     did,
     I     bi, bj,
     I     myThid )

C     !DESCRIPTION:
C     Write a CF-convention coordinate variable (a vector).

C     !USES:
      implicit none

C
C     ==========================================
C     MNC : an MITgcm wrapper package for NetCDF
C     ==========================================
C
C     The following common block is the "state" for the MNC interface to
C     NetCDF.  The intent is to keep track of the associations between
C     files, attributes, variables, grids, and dimensions.  These
C     objects are roughly defined as:
C
C     a dimension:
C     - contains: [ name, size ]
C     - exists per-NetCDF-file
C
C     a grid:
C     - contains *ORDERED* sets of dimensions: [ name, 1+ dim-refs ]
C     - exists per-NetCDF-file
C     - NOTE: when created, the name and dim-refs are embedded in
C         the NetCDF file attributes for later retrieval
C     - NOTE: grid coordinates are implemented using variables with
C         special names (eg. lat, lon) and special units
C         (eg. degree_east)
C
C     a variable:
C     - contains: [ name, units, 1 grid-ref, data ]
C     - exists per-NetCDF-file
C     - NOTE: is associated with *exactly* one grid
C
C     an attribute:
C     - contains: [ name, units, data ]
C     - basically, a scalar (non-grid) variable
C     - exists per-NetCDF-file
C
C     a NetCDF file:
C     - contains: [ name, 0+ attr, 0+ grid-ref, 0+ var-ref ]
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 1
C     !ROUTINE: MNC_COMMON.h

C     !INTERFACE:
C     #include "MNC_COMMON.h"

C     !DESCRIPTION:
C     Contains the "look-up" tables for the MNC package.  These tables
C     contain the mappings between the various names and the NetCDF
C     entities.

C     !LOCAL VARIABLES:
C     The following MNC "Internals" are implemented on a
C     PER-NetDCF-FILE basis:
C     .
C     mnc_blank_name    :: (convenience) just MNC_MAX_CHAR spaces
C     .
C     mnc_f_names (fi)  :: file names
C     mnc_g_names (gi)  :: grid names    <----+
C     .                                       |
C     mnc_f_info (fi,-) :: isDEF, fID, Ngrid, g1,ds1,de1,
C     .                                       g2,ds2,de2, ...
C     .                                          |   |
C     mnc_fd_ind (fi,-) :: dim indicies  <-------+---+
C     .                              |
C     mnc_d_names (di)  :: names  <--+  <--+  |
C     mnc_d_ids   (di)  :: IDs    <--+  <--+  +----+
C     mnc_d_size  (di)  :: sizes  <--+  <--+       | starting
C     .                                    |       | indicies of
C     mnc_f_alld (fi,di):: ndim, id1,id2,id3, ...  | grids in
C     .                                            | mnc_f_info
C     .                                 +----------++
C     .                                 |           |
C     mnc_fv_ids (fi,-) :: nVar, n1,ID1,ig1, n2,ID2,ig2, ...
C     .                          |           |
C     mnc_v_names (vi)  ::   <---+-----------+
C
C     fi                ::  file index
C     vi                ::  variable index
C     di                ::  dimension index
C     .
C     .
C     The following MNC "Convenience Wrapper" variables are
C     implemented independently of any NetCDF files
C     .
C     mnc_cw_fgnm  (f)  :: file group name (or "base name")
C     mnc_cw_fgud  (f)  :: file group unlimited dim value
C     mnc_cw_fgis  (f)  :: file group sequence number
C     mnc_cw_fgig  (f)  :: file group unlim dim is growing
C     mnc_cw_fgci  (f)  :: file CITER group number (cig) ------+
C     .                                                        |
C     mnc_cw_cit (3,cig):: CITER (1) flag, (2) current, and    |
C     .                          (3) next model Iter values <--+
C     .
C     mnc_cw_gname (g)  :: Gtype names              <--------+
C     mnc_cw_ndim  (g)  :: number of dimensions              |
C     mnc_cw_dn   (i,g) :: dname1, dname2, ...               |
C     mnc_cw_dims (i,g) :: d1, d2, d3, ...                   |
C     mnc_cw_is   (i,g) :: starting indicies: is1, is2, ...  |
C     mnc_cw_ie   (i,g) :: ending indicies:   ie1, ie2, ...  |
C     .                                                      |
C     mnc_cw_vname (v)  :: Vtype names                       |
C     mnc_cw_vgind (v)  :: index into                --------+
C     mnc_cw_vfmv  (v)  :: flag for missing values
C     .                      0 = ignore it (default)
C     .                      1 = use global value
C     .                      2 = use specific per-var value
C     mnc_cw_vmvi (2,v) :: integer missing values: 1=IN, 2=OUT
C     mnc_cw_vmvr (2,v) :: REAL*4  missing values: 1=IN, 2=OUT
C     mnc_cw_vmvd (2,v) :: REAL*8  missing values: 1=IN, 2=OUT
C     mnc_cw_vnat (3,v) :: number of attributes [T,I,D]
C     mnc_cw_vbij (2,v) :: bi,bi indicies (0 if not applicable)
C     mnc_cw_vtnm (i,v) :: text (character) attribute names
C     mnc_cw_vtat (i,v) :: text (character) attributes
C     mnc_cw_vinm (i,v) :: INT attribute names
C     mnc_cw_viat (i,v) :: INT attributes
C     mnc_cw_vdnm (i,v) :: REAL*8 attribute names
C     mnc_cw_vdat (i,v) :: REAL*8 attributes
C     .
C     mnc_cw_cvnm  (c)  :: CV (coordinate variable) name
C     mnc_cw_cvse (2,c) :: CV start,end indicies  ----+
C     mnc_cw_cvdt (cdt) :: CV data pool       <-------+
C     .
C     f                 :: file group index
C     g                 :: Gtype index
C     v                 :: Vtype index
C     c                 :: CV index
CEOP


C

      integer MNC_MAX_ID, MNC_MAX_FID
      integer MNC_MAX_CHAR, MNC_MAX_CATT
      integer MNC_MAX_PATH, MNC_MAX_INFO
      integer MNC_CW_MAX_I, MNC_CW_CVDAT
      parameter ( MNC_MAX_ID   =   3000 )
      parameter ( MNC_MAX_FID  =    200 )
      parameter ( MNC_MAX_CHAR =     40 )
      parameter ( MNC_MAX_CATT =    100 )
      parameter ( MNC_MAX_PATH =    500 )
      parameter ( MNC_MAX_INFO =    800 )
      parameter ( MNC_CW_MAX_I =     20 )
      parameter ( MNC_CW_CVDAT =  50000 )

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***


C=====================================================================
      COMMON /MNC_VARS_C/
     &     mnc_blank_name,
     &     mnc_g_names, mnc_v_names, mnc_d_names,
     &     mnc_out_path, mnc_f_names

      character*(MNC_MAX_CHAR) mnc_blank_name
      character*(MNC_MAX_CHAR) mnc_g_names(MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_v_names(MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_d_names(MNC_MAX_ID)

      character*(MNC_MAX_PATH) mnc_out_path
      character*(MNC_MAX_PATH) mnc_f_names(MNC_MAX_FID)

C=====================================================================
      COMMON /MNC_VARS_I/
     &     mnc_f_info,  mnc_fd_ind,  mnc_fv_ids,  mnc_f_alld,
     &     mnc_d_size,  mnc_d_ids

      integer mnc_f_info(MNC_MAX_FID,MNC_MAX_INFO)
      integer mnc_fd_ind(MNC_MAX_FID,MNC_MAX_INFO)
      integer mnc_fv_ids(MNC_MAX_FID,MNC_MAX_INFO)
      integer mnc_f_alld(MNC_MAX_FID,MNC_MAX_INFO)

      integer mnc_d_size(MNC_MAX_ID)
      integer mnc_d_ids(MNC_MAX_ID)

C=====================================================================
      COMMON /MNC_CW_VARS_C/
     &     mnc_cw_gname, mnc_cw_dn,
     &     mnc_cw_vname,
     &     mnc_cw_vtnm,  mnc_cw_vinm,  mnc_cw_vdnm,
     &     mnc_cw_fgnm,
     &     mnc_cw_vtat

      character*(MNC_MAX_CHAR) mnc_cw_gname(MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_cw_dn(MNC_CW_MAX_I,MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_cw_vname(MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_cw_vtnm(MNC_CW_MAX_I,MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_cw_vinm(MNC_CW_MAX_I,MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_cw_vdnm(MNC_CW_MAX_I,MNC_MAX_ID)
      character*(MNC_MAX_CHAR) mnc_cw_fgnm(MNC_MAX_ID)

C     Note the longer string length here
      character*(MNC_MAX_CATT) mnc_cw_vtat(MNC_CW_MAX_I,MNC_MAX_ID)

C=====================================================================
      COMMON /MNC_CW_VARS_I/
     &     mnc_cw_ndim,  mnc_cw_dims,
     &     mnc_cw_is,    mnc_cw_ie,
     &     mnc_cw_vgind, mnc_cw_vfmv,  mnc_cw_vmvi, mnc_cw_vnat,
     &     mnc_cw_vbij,  mnc_cw_viat,
     &     mnc_cw_fgud,  mnc_cw_fgis,  mnc_cw_fgig, mnc_cw_fgci,
     &     mnc_cw_cit

      integer mnc_cw_ndim(MNC_MAX_ID)
      integer mnc_cw_dims(MNC_CW_MAX_I,MNC_MAX_ID)
      integer mnc_cw_is(MNC_CW_MAX_I,MNC_MAX_ID)
      integer mnc_cw_ie(MNC_CW_MAX_I,MNC_MAX_ID)

      integer mnc_cw_vgind(MNC_MAX_ID)
      integer mnc_cw_vfmv(MNC_MAX_ID)
      integer mnc_cw_vmvi(2,MNC_MAX_ID)
      integer mnc_cw_vnat(3,MNC_MAX_ID)
      integer mnc_cw_vbij(2,MNC_MAX_ID)
      integer mnc_cw_viat(MNC_CW_MAX_I,MNC_MAX_ID)

      integer mnc_cw_fgud(MNC_MAX_ID)
      integer mnc_cw_fgis(MNC_MAX_ID)
      integer mnc_cw_fgig(MNC_MAX_ID)
      integer mnc_cw_fgci(MNC_MAX_ID)

      integer mnc_cw_cit(3,MNC_MAX_INFO)

C=====================================================================
      COMMON /MNC_CW_VARS_D/
     &     mnc_cw_vdat,  mnc_cw_vmvd
C     &     mnc_cw_cvdt

      REAL*8  mnc_cw_vmvd(2,MNC_MAX_ID)
      REAL*8  mnc_cw_vdat(MNC_CW_MAX_I,MNC_MAX_ID)

C=====================================================================
      COMMON /MNC_CW_VARS_R4/
     &     mnc_cw_vmvr

      REAL*4  mnc_cw_vmvr(2,MNC_MAX_ID)


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***

CBOP
C    !ROUTINE: SIZE.h
C    !INTERFACE:
C    include SIZE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SIZE.h Declare size of underlying computational grid.
C     *==========================================================*
C     | The design here supports a three-dimensional model grid
C     | with indices I,J and K. The three-dimensional domain
C     | is comprised of nPx*nSx blocks (or tiles) of size sNx
C     | along the first (left-most index) axis, nPy*nSy blocks
C     | of size sNy along the second axis and one block of size
C     | Nr along the vertical (third) axis.
C     | Blocks/tiles have overlap regions of size OLx and OLy
C     | along the dimensions that are subdivided.
C     *==========================================================*
C     \ev
C
C     Voodoo numbers controlling data layout:
C     sNx :: Number of X points in tile.
C     sNy :: Number of Y points in tile.
C     OLx :: Tile overlap extent in X.
C     OLy :: Tile overlap extent in Y.
C     nSx :: Number of tiles per process in X.
C     nSy :: Number of tiles per process in Y.
C     nPx :: Number of processes to use in X.
C     nPy :: Number of processes to use in Y.
C     Nx  :: Number of points in X for the full domain.
C     Ny  :: Number of points in Y for the full domain.
C     Nr  :: Number of points in vertical direction.
CEOP
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  64,
     &           sNy =  32,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   2,
     &           nSy =   2,
     &           nPx =   1,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  15)

C     MAX_OLX :: Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buffers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )


CBOP
C     !ROUTINE: EEPARAMS.h
C     !INTERFACE:
C     include "EEPARAMS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EEPARAMS.h                                               |
C     *==========================================================*
C     | Parameters for "execution environemnt". These are used   |
C     | by both the particular numerical model and the execution |
C     | environment support routines.                            |
C     *==========================================================*
CEOP

C     ========  EESIZE.h  ========================================

C     MAX_LEN_MBUF  :: Default message buffer max. size
C     MAX_LEN_FNAM  :: Default file name max. size
C     MAX_LEN_PREC  :: Default rec len for reading "parameter" files

      INTEGER MAX_LEN_MBUF
      PARAMETER ( MAX_LEN_MBUF = 512 )
      INTEGER MAX_LEN_FNAM
      PARAMETER ( MAX_LEN_FNAM = 512 )
      INTEGER MAX_LEN_PREC
      PARAMETER ( MAX_LEN_PREC = 200 )

C     MAX_NO_THREADS  :: Maximum number of threads allowed.
C     GSVec_size      :: Maximum buffer size for Global Sum Vector array
      INTEGER MAX_NO_THREADS
      PARAMETER ( MAX_NO_THREADS =  4 )
      INTEGER GSVec_size
      PARAMETER ( GSVec_size = 1024 )

C     Particularly weird and obscure voodoo numbers
C     lShare :: This wants to be the length in
C               [148]-byte words of the size of
C               the address "window" that is snooped
C               on an SMP bus. By separating elements in
C               the global sum buffer we can avoid generating
C               extraneous invalidate traffic between
C               processors. The length of this window is usually
C               a cache line i.e. small O(64 bytes).
C               The buffer arrays are usually short arrays
C               and are declared REAL ARRA(lShare[148],LBUFF).
C               Setting lShare[148] to 1 is like making these arrays
C               one dimensional.
      INTEGER cacheLineSize
      INTEGER lShare1
      INTEGER lShare4
      INTEGER lShare8
      PARAMETER ( cacheLineSize = 256 )
      PARAMETER ( lShare1 =  cacheLineSize )
      PARAMETER ( lShare4 =  cacheLineSize/4 )
      PARAMETER ( lShare8 =  cacheLineSize/8 )

C     ========  EESIZE.h  ========================================

C     Symbolic values
C     precXXXX :: precision used for I/O
      INTEGER precFloat32
      PARAMETER ( precFloat32 = 32 )
      INTEGER precFloat64
      PARAMETER ( precFloat64 = 64 )

C     Real-type constant for some frequently used simple number (0,1,2,1/2):
      Real*8     zeroRS, oneRS, twoRS, halfRS
      PARAMETER ( zeroRS = 0.0D0 , oneRS  = 1.0D0 )
      PARAMETER ( twoRS  = 2.0D0 , halfRS = 0.5D0 )
      Real*8     zeroRL, oneRL, twoRL, halfRL
      PARAMETER ( zeroRL = 0.0D0 , oneRL  = 1.0D0 )
      PARAMETER ( twoRL  = 2.0D0 , halfRL = 0.5D0 )

C     UNSET_xxx :: Used to indicate variables that have not been given a value
      Real*8  UNSET_FLOAT8
      PARAMETER ( UNSET_FLOAT8 = 1.234567D5 )
      Real*4  UNSET_FLOAT4
      PARAMETER ( UNSET_FLOAT4 = 1.234567E5 )
      Real*8     UNSET_RL
      PARAMETER ( UNSET_RL     = 1.234567D5 )
      Real*8     UNSET_RS
      PARAMETER ( UNSET_RS     = 1.234567D5 )
      INTEGER UNSET_I
      PARAMETER ( UNSET_I      = 123456789  )

C     debLevX  :: used to decide when to print debug messages
      INTEGER debLevZero
      INTEGER debLevA, debLevB,  debLevC, debLevD, debLevE
      PARAMETER ( debLevZero=0 )
      PARAMETER ( debLevA=1 )
      PARAMETER ( debLevB=2 )
      PARAMETER ( debLevC=3 )
      PARAMETER ( debLevD=4 )
      PARAMETER ( debLevE=5 )

C     SQUEEZE_RIGHT      :: Flag indicating right blank space removal
C                           from text field.
C     SQUEEZE_LEFT       :: Flag indicating left blank space removal
C                           from text field.
C     SQUEEZE_BOTH       :: Flag indicating left and right blank
C                           space removal from text field.
C     PRINT_MAP_XY       :: Flag indicating to plot map as XY slices
C     PRINT_MAP_XZ       :: Flag indicating to plot map as XZ slices
C     PRINT_MAP_YZ       :: Flag indicating to plot map as YZ slices
C     commentCharacter   :: Variable used in column 1 of parameter
C                           files to indicate comments.
C     INDEX_I            :: Variable used to select an index label
C     INDEX_J               for formatted input parameters.
C     INDEX_K
C     INDEX_NONE
      CHARACTER*(*) SQUEEZE_RIGHT
      PARAMETER ( SQUEEZE_RIGHT = 'R' )
      CHARACTER*(*) SQUEEZE_LEFT
      PARAMETER ( SQUEEZE_LEFT = 'L' )
      CHARACTER*(*) SQUEEZE_BOTH
      PARAMETER ( SQUEEZE_BOTH = 'B' )
      CHARACTER*(*) PRINT_MAP_XY
      PARAMETER ( PRINT_MAP_XY = 'XY' )
      CHARACTER*(*) PRINT_MAP_XZ
      PARAMETER ( PRINT_MAP_XZ = 'XZ' )
      CHARACTER*(*) PRINT_MAP_YZ
      PARAMETER ( PRINT_MAP_YZ = 'YZ' )
      CHARACTER*(*) commentCharacter
      PARAMETER ( commentCharacter = '#' )
      INTEGER INDEX_I
      INTEGER INDEX_J
      INTEGER INDEX_K
      INTEGER INDEX_NONE
      PARAMETER ( INDEX_I    = 1,
     &            INDEX_J    = 2,
     &            INDEX_K    = 3,
     &            INDEX_NONE = 4 )

C     EXCH_IGNORE_CORNERS :: Flag to select ignoring or
C     EXCH_UPDATE_CORNERS    updating of corners during an edge exchange.
      INTEGER EXCH_IGNORE_CORNERS
      INTEGER EXCH_UPDATE_CORNERS
      PARAMETER ( EXCH_IGNORE_CORNERS = 0,
     &            EXCH_UPDATE_CORNERS = 1 )

C     FORWARD_SIMULATION
C     REVERSE_SIMULATION
C     TANGENT_SIMULATION
      INTEGER FORWARD_SIMULATION
      INTEGER REVERSE_SIMULATION
      INTEGER TANGENT_SIMULATION
      PARAMETER ( FORWARD_SIMULATION = 0,
     &            REVERSE_SIMULATION = 1,
     &            TANGENT_SIMULATION = 2 )

C--   COMMON /EEPARAMS_L/ Execution environment public logical variables.
C     eeBootError    :: Flags indicating error during multi-processing
C     eeEndError     :: initialisation and termination.
C     fatalError     :: Flag used to indicate that the model is ended with an error
C     debugMode      :: controls printing of debug msg (sequence of S/R calls).
C     useSingleCpuIO :: When useSingleCpuIO is set, MDS_WRITE_FIELD outputs from
C                       master MPI process only. -- NOTE: read from main parameter
C                       file "data" and not set until call to INI_PARMS.
C     useSingleCpuInput :: When useSingleCpuInput is set, EXF_INTERP_READ
C                       reads forcing files from master MPI process only.
C                       -- NOTE: read from main parameter file "data"
C                          and defaults to useSingleCpuInput = useSingleCpuIO
C     printMapIncludesZeros  :: Flag that controls whether character constant
C                               map code ignores exact zero values.
C     useCubedSphereExchange :: use Cubed-Sphere topology domain.
C     useCoupler     :: use Coupler for a multi-components set-up.
C     useNEST_PARENT :: use Parent Nesting interface (pkg/nest_parent)
C     useNEST_CHILD  :: use Child  Nesting interface (pkg/nest_child)
C     useNest2W_parent :: use Parent 2-W Nesting interface (pkg/nest2w_parent)
C     useNest2W_child  :: use Child  2-W Nesting interface (pkg/nest2w_child)
C     useOASIS       :: use OASIS-coupler for a multi-components set-up.
      COMMON /EEPARAMS_L/
c    &  eeBootError, fatalError, eeEndError,
     &  eeBootError, eeEndError, fatalError, debugMode,
     &  useSingleCpuIO, useSingleCpuInput, printMapIncludesZeros,
     &  useCubedSphereExchange, useCoupler,
     &  useNEST_PARENT, useNEST_CHILD,
     &  useNest2W_parent, useNest2W_child, useOASIS,
     &  useSETRLSTK, useSIGREG
      LOGICAL eeBootError
      LOGICAL eeEndError
      LOGICAL fatalError
      LOGICAL debugMode
      LOGICAL useSingleCpuIO
      LOGICAL useSingleCpuInput
      LOGICAL printMapIncludesZeros
      LOGICAL useCubedSphereExchange
      LOGICAL useCoupler
      LOGICAL useNEST_PARENT
      LOGICAL useNEST_CHILD
      LOGICAL useNest2W_parent
      LOGICAL useNest2W_child
      LOGICAL useOASIS
      LOGICAL useSETRLSTK
      LOGICAL useSIGREG

C--   COMMON /EPARAMS_I/ Execution environment public integer variables.
C     errorMessageUnit    :: Fortran IO unit for error messages
C     standardMessageUnit :: Fortran IO unit for informational messages
C     maxLengthPrt1D :: maximum length for printing (to Std-Msg-Unit) 1-D array
C     scrUnit1      :: Scratch file 1 unit number
C     scrUnit2      :: Scratch file 2 unit number
C     eeDataUnit    :: Unit # for reading "execution environment" parameter file
C     modelDataUnit :: Unit number for reading "model" parameter file.
C     numberOfProcs :: Number of processes computing in parallel
C     pidIO         :: Id of process to use for I/O.
C     myBxLo, myBxHi :: Extents of domain in blocks in X and Y
C     myByLo, myByHi :: that each threads is responsble for.
C     myProcId      :: My own "process" id.
C     myPx          :: My X coord on the proc. grid.
C     myPy          :: My Y coord on the proc. grid.
C     myXGlobalLo   :: My bottom-left (south-west) x-index global domain.
C                      The x-coordinate of this point in for example m or
C                      degrees is *not* specified here. A model needs to
C                      provide a mechanism for deducing that information
C                      if it is needed.
C     myYGlobalLo   :: My bottom-left (south-west) y-index in global domain.
C                      The y-coordinate of this point in for example m or
C                      degrees is *not* specified here. A model needs to
C                      provide a mechanism for deducing that information
C                      if it is needed.
C     nThreads      :: No. of threads
C     nTx, nTy      :: No. of threads in X and in Y
C                      This assumes a simple cartesian gridding of the threads
C                      which is not required elsewhere but that makes it easier
C     ioErrorCount  :: IO Error Counter. Set to zero initially and increased
C                      by one every time an IO error occurs.
      COMMON /EEPARAMS_I/
     &  errorMessageUnit, standardMessageUnit, maxLengthPrt1D,
     &  scrUnit1, scrUnit2, eeDataUnit, modelDataUnit,
     &  numberOfProcs, pidIO, myProcId,
     &  myPx, myPy, myXGlobalLo, myYGlobalLo, nThreads,
     &  myBxLo, myBxHi, myByLo, myByHi,
     &  nTx, nTy, ioErrorCount
      INTEGER errorMessageUnit
      INTEGER standardMessageUnit
      INTEGER maxLengthPrt1D
      INTEGER scrUnit1
      INTEGER scrUnit2
      INTEGER eeDataUnit
      INTEGER modelDataUnit
      INTEGER ioErrorCount(MAX_NO_THREADS)
      INTEGER myBxLo(MAX_NO_THREADS)
      INTEGER myBxHi(MAX_NO_THREADS)
      INTEGER myByLo(MAX_NO_THREADS)
      INTEGER myByHi(MAX_NO_THREADS)
      INTEGER myProcId
      INTEGER myPx
      INTEGER myPy
      INTEGER myXGlobalLo
      INTEGER myYGlobalLo
      INTEGER nThreads
      INTEGER nTx
      INTEGER nTy
      INTEGER numberOfProcs
      INTEGER pidIO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: PARAMS.h
C     !INTERFACE:
C     #include PARAMS.h

C     !DESCRIPTION:
C     Header file defining model "parameters".  The values from the
C     model standard input file are stored into the variables held
C     here. Notes describing the parameters can also be found here.

CEOP

C--   Contants
C     Useful physical values
      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      Real*8 deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0           )

C--   COMMON /PARM_C/ Character valued parameters used by the model.
C     buoyancyRelation :: Flag used to indicate which relation to use to
C                         get buoyancy.
C     eosType         :: choose the equation of state:
C                        LINEAR, POLY3, UNESCO, JMD95Z, JMD95P, MDJWF, IDEALGAS
C     pickupSuff      :: force to start from pickup files (even if nIter0=0)
C                        and read pickup files with this suffix (max 10 Char.)
C     mdsioLocalDir   :: read-write tiled file from/to this directory name
C                        (+ 4 digits Processor-Rank) instead of current dir.
C     adTapeDir       :: read-write checkpointing tape files from/to this
C                        directory name instead of current dir. Conflicts
C                        mdsioLocalDir, so only one of the two can be set.
C     tRefFile      :: File containing reference Potential Temperat.  tRef (1.D)
C     sRefFile      :: File containing reference salinity/spec.humid. sRef (1.D)
C     rhoRefFile    :: File containing reference density profile rhoRef (1.D)
C     gravityFile   :: File containing gravity vertical profile (1.D)
C     delRFile      :: File containing vertical grid spacing delR  (1.D array)
C     delRcFile     :: File containing vertical grid spacing delRc (1.D array)
C     hybSigmFile   :: File containing hybrid-sigma vertical coord. coeff. (2x 1.D)
C     delXFile      :: File containing X-spacing grid definition (1.D array)
C     delYFile      :: File containing Y-spacing grid definition (1.D array)
C     horizGridFile :: File containing horizontal-grid definition
C                        (only when using curvilinear_grid)
C     bathyFile       :: File containing bathymetry. If not defined bathymetry
C                        is taken from inline function.
C     topoFile        :: File containing the topography of the surface (unit=m)
C                        (mainly used for the atmosphere = ground height).
C     addWwallFile    :: File containing 2-D additional Western  cell-edge wall
C     addSwallFile    :: File containing 2-D additional Southern cell-edge wall
C                        (e.g., to add "thin-wall" where it is =1)
C     hydrogThetaFile :: File containing initial hydrographic data (3-D)
C                        for potential temperature.
C     hydrogSaltFile  :: File containing initial hydrographic data (3-D)
C                        for salinity.
C     diffKrFile      :: File containing 3D specification of vertical diffusivity
C     viscAhDfile     :: File containing 3D specification of horizontal viscosity
C     viscAhZfile     :: File containing 3D specification of horizontal viscosity
C     viscA4Dfile     :: File containing 3D specification of horizontal viscosity
C     viscA4Zfile     :: File containing 3D specification of horizontal viscosity
C     zonalWindFile   :: File containing zonal wind data
C     meridWindFile   :: File containing meridional wind data
C     thetaClimFile   :: File containing surface theta climataology used
C                        in relaxation term -lambda(theta-theta*)
C     saltClimFile    :: File containing surface salt climataology used
C                        in relaxation term -lambda(salt-salt*)
C     surfQfile       :: File containing surface heat flux, excluding SW
C                        (old version, kept for backward compatibility)
C     surfQnetFile    :: File containing surface net heat flux
C     surfQswFile     :: File containing surface shortwave radiation
C     EmPmRfile       :: File containing surface fresh water flux
C           NOTE: for backward compatibility EmPmRfile is specified in
C                 m/s when using external_fields_load.F.  It is converted
C                 to kg/m2/s by multiplying by rhoConstFresh.
C     saltFluxFile    :: File containing surface salt flux
C     pLoadFile       :: File containing pressure loading
C     geoPotAnomFile  :: File containing constant geopotential anomaly due to
C                        density structure
C     addMassFile     :: File containing source/sink of fluid in the interior
C     eddyPsiXFile    :: File containing zonal Eddy streamfunction data
C     eddyPsiYFile    :: File containing meridional Eddy streamfunction data
C     geothermalFile  :: File containing geothermal heat flux
C     lambdaThetaFile :: File containing SST relaxation coefficient
C     lambdaSaltFile  :: File containing SSS relaxation coefficient
C     wghtBalanceFile :: File containing weight used in balancing net EmPmR
C     the_run_name    :: string identifying the name of the model "run"
      COMMON /PARM_C/
     &                buoyancyRelation, eosType,
     &                pickupSuff, mdsioLocalDir, adTapeDir,
     &                tRefFile, sRefFile, rhoRefFile, gravityFile,
     &                delRFile, delRcFile, hybSigmFile,
     &                delXFile, delYFile, horizGridFile,
     &                bathyFile, topoFile, addWwallFile, addSwallFile,
     &                viscAhDfile, viscAhZfile,
     &                viscA4Dfile, viscA4Zfile,
     &                hydrogThetaFile, hydrogSaltFile, diffKrFile,
     &                zonalWindFile, meridWindFile, thetaClimFile,
     &                saltClimFile,
     &                EmPmRfile, saltFluxFile,
     &                surfQfile, surfQnetFile, surfQswFile,
     &                uVelInitFile, vVelInitFile, pSurfInitFile,
     &                pLoadFile, geoPotAnomFile, addMassFile,
     &                eddyPsiXFile, eddyPsiYFile, geothermalFile,
     &                lambdaThetaFile, lambdaSaltFile, wghtBalanceFile,
     &                the_run_name
      CHARACTER*(MAX_LEN_FNAM) buoyancyRelation
      CHARACTER*(6)  eosType
      CHARACTER*(10) pickupSuff
      CHARACTER*(MAX_LEN_FNAM) mdsioLocalDir
      CHARACTER*(MAX_LEN_FNAM) adTapeDir
      CHARACTER*(MAX_LEN_FNAM) tRefFile
      CHARACTER*(MAX_LEN_FNAM) sRefFile
      CHARACTER*(MAX_LEN_FNAM) rhoRefFile
      CHARACTER*(MAX_LEN_FNAM) gravityFile
      CHARACTER*(MAX_LEN_FNAM) delRFile
      CHARACTER*(MAX_LEN_FNAM) delRcFile
      CHARACTER*(MAX_LEN_FNAM) hybSigmFile
      CHARACTER*(MAX_LEN_FNAM) delXFile
      CHARACTER*(MAX_LEN_FNAM) delYFile
      CHARACTER*(MAX_LEN_FNAM) horizGridFile
      CHARACTER*(MAX_LEN_FNAM) bathyFile, topoFile
      CHARACTER*(MAX_LEN_FNAM) addWwallFile, addSwallFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile, hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) diffKrFile
      CHARACTER*(MAX_LEN_FNAM) viscAhDfile
      CHARACTER*(MAX_LEN_FNAM) viscAhZfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Dfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Zfile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile
      CHARACTER*(MAX_LEN_FNAM) surfQfile
      CHARACTER*(MAX_LEN_FNAM) surfQnetFile
      CHARACTER*(MAX_LEN_FNAM) surfQswFile
      CHARACTER*(MAX_LEN_FNAM) EmPmRfile
      CHARACTER*(MAX_LEN_FNAM) saltFluxFile
      CHARACTER*(MAX_LEN_FNAM) uVelInitFile
      CHARACTER*(MAX_LEN_FNAM) vVelInitFile
      CHARACTER*(MAX_LEN_FNAM) pSurfInitFile
      CHARACTER*(MAX_LEN_FNAM) pLoadFile
      CHARACTER*(MAX_LEN_FNAM) geoPotAnomFile
      CHARACTER*(MAX_LEN_FNAM) addMassFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiXFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiYFile
      CHARACTER*(MAX_LEN_FNAM) geothermalFile
      CHARACTER*(MAX_LEN_FNAM) lambdaThetaFile
      CHARACTER*(MAX_LEN_FNAM) lambdaSaltFile
      CHARACTER*(MAX_LEN_FNAM) wghtBalanceFile
      CHARACTER*(MAX_LEN_PREC/2) the_run_name

C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        :: Maximum number of iterations in the
C                            two-dimensional con. grad solver.
C     cg2dMinItersNSA     :: Minimum number of iterations in the
C                            not-self-adjoint version (cg2d_nsa.F) of the
C                            two-dimensional con. grad solver (default = 0).
C     cg2dPreCondFreq     :: Frequency for updating cg2d preconditioner
C                            (non-linear free-surf.)
C     cg2dUseMinResSol    :: =0 : use last-iteration/converged solution
C                            =1 : use solver minimum-residual solution
C     cg3dMaxIters        :: Maximum number of iterations in the
C                            three-dimensional con. grad solver.
C     printResidualFreq   :: Frequency for printing residual in CG iterations
C     nIter0              :: Start time-step number of for this run
C     nTimeSteps          :: Number of timesteps to execute
C     nTimeSteps_l2       :: Number of inner timesteps to execute per timestep
C     selectCoriMap       :: select setting of Coriolis parameter map:
C                           =0 f-Plane (Constant Coriolis, = f0)
C                           =1 Beta-Plane Coriolis (= f0 + beta.y)
C                           =2 Spherical Coriolis (= 2.omega.sin(phi))
C                           =3 Read Coriolis 2-d fields from files.
C     selectSigmaCoord    :: option related to sigma vertical coordinate
C     nonlinFreeSurf      :: option related to non-linear free surface
C                           =0 Linear free surface ; >0 Non-linear
C     select_rStar        :: option related to r* vertical coordinate
C                           =0 (default) use r coord. ; > 0 use r*
C     selectNHfreeSurf    :: option for Non-Hydrostatic (free-)Surface formulation:
C                           =0 (default) hydrostatic surf. ; > 0 add NH effects.
C     selectP_inEOS_Zc    :: select which pressure to use in EOS (for z-coords)
C                           =0: simply: -g*rhoConst*z
C                           =1: use pRef = integral{-g*rho(Tref,Sref,pRef)*dz}
C                           =2: use hydrostatic dynamical pressure
C                           =3: use full (Hyd+NH) dynamical pressure
C     selectAddFluid      :: option to add mass source/sink of fluid in the interior
C                            (3-D generalisation of oceanic real-fresh water flux)
C                           =0 off ; =1 add fluid ; =-1 virtual flux (no mass added)
C     selectBalanceEmPmR  :: option to balance net surface fresh-water flux:
C                           =0 off ; =1 uniform correction ; = 2 weighted correction
C     selectImplicitDrag  :: select Implicit treatment of bottom/top drag
C                           = 0: fully explicit
C                           = 1: implicit on provisional velocity
C                                (i.e., before grad.Eta increment)
C                           = 2: fully implicit (combined with Impl Surf.Press)
C     selectPenetratingSW :: select treatment of penetrating shortwave radiation
C                            (requires to define SHORTWAVE_HEATING):
C                           = 0: no shortwave penetration
C                           = 1: constant in time and horizontally uniform
C                                fraction of shortwave penetration (default)
C                           = 2: constant in time, but non-uniform fraction of
C                                shortwave penetration (not yet coded)
C                           > 2: time varying fraction of shortwave penetration
C                                according to external function (e.g. BGC model,
C                                not yet coded)
C     momForcingOutAB     :: =1: take momentum forcing contribution
C                            out of (=0: in) Adams-Bashforth time stepping.
C     tracForcingOutAB    :: =1: take tracer (Temp,Salt,pTracers) forcing contribution
C                            out of (=0: in) Adams-Bashforth time stepping.
C     tempAdvScheme       :: Temp. Horiz.Advection scheme selector
C     tempVertAdvScheme   :: Temp. Vert. Advection scheme selector
C     saltAdvScheme       :: Salt. Horiz.advection scheme selector
C     saltVertAdvScheme   :: Salt. Vert. Advection scheme selector
C     selectKEscheme      :: Kinetic Energy scheme selector (Vector Inv.)
C     selectVortScheme    :: Scheme selector for Vorticity term (Vector Inv.)
C     selectMetricTerms   :: Scheme selector for Metric terms (Flux-Form)
C     selectCoriScheme    :: Scheme selector for Coriolis term
C     select3dCoriScheme  :: Scheme selector for 3-D Coriolis (in Omega.cos Phi)
C     selectBotDragQuadr  :: quadratic bottom drag discretisation option:
C                           =0: average KE from grid center to U & V location
C                           =1: use local velocity norm @ U & V location
C                           =2: same with wet-point averaging of other component
C     pCellMix_select     :: select option to enhance mixing near surface & bottom
C                            unit digit: near bottom ; tens digit: near surface
C                            with digit =0 : disable ;
C                           = 1 : increases mixing linearly with recip_hFac
C                           = 2,3,4 : increases mixing by recip_hFac^(2,3,4)
C     readBinaryPrec      :: Precision used for reading binary files
C     writeBinaryPrec     :: Precision used for writing binary files
C     rwSuffixType        :: controls the format of the mds file suffix.
C                          =0 (default): use iteration number (myIter, I10.10);
C                          =1: 100*myTime (100th sec); =2: myTime (seconds);
C                          =3: myTime/360 (10th of hr); =4: myTime/3600 (hours).
C     monitorSelect       :: select group of variables to monitor
C                            =1 : dynvars ; =2 : + vort ; =3 : + surface
C-    debugLevel          :: controls printing of algorithm intermediate results
C                            and statistics ; higher -> more writing
C-    plotLevel           :: controls printing of field maps ; higher -> more flds

      COMMON /PARM_I/
     &        cg2dMaxIters, cg2dMinItersNSA,
     &        cg2dPreCondFreq, cg2dUseMinResSol,
     &        cg3dMaxIters, printResidualFreq,
     &        nIter0, nTimeSteps, nTimeSteps_l2, nEndIter,
     &        selectCoriMap,
     &        selectSigmaCoord,
     &        nonlinFreeSurf, select_rStar,
     &        selectNHfreeSurf, selectP_inEOS_Zc,
     &        selectAddFluid, selectBalanceEmPmR, selectImplicitDrag,
     &        momForcingOutAB, tracForcingOutAB,
     &        tempAdvScheme, tempVertAdvScheme,
     &        saltAdvScheme, saltVertAdvScheme,
     &        selectKEscheme, selectVortScheme, selectMetricTerms,
     &        selectCoriScheme, select3dCoriScheme,
     &        selectBotDragQuadr, selectPenetratingSW, pCellMix_select,
     &        readBinaryPrec, writeBinaryPrec,
     &        rwSuffixType, monitorSelect, debugLevel, plotLevel
      INTEGER cg2dMaxIters
      INTEGER cg2dMinItersNSA
      INTEGER cg2dPreCondFreq
      INTEGER cg2dUseMinResSol
      INTEGER cg3dMaxIters
      INTEGER printResidualFreq
      INTEGER nIter0
      INTEGER nTimeSteps
      INTEGER nTimeSteps_l2
      INTEGER nEndIter
      INTEGER selectCoriMap
      INTEGER selectSigmaCoord
      INTEGER nonlinFreeSurf
      INTEGER select_rStar
      INTEGER selectNHfreeSurf
      INTEGER selectP_inEOS_Zc
      INTEGER selectAddFluid
      INTEGER selectBalanceEmPmR
      INTEGER selectImplicitDrag
      INTEGER momForcingOutAB, tracForcingOutAB
      INTEGER tempAdvScheme, tempVertAdvScheme
      INTEGER saltAdvScheme, saltVertAdvScheme
      INTEGER selectKEscheme
      INTEGER selectVortScheme
      INTEGER selectMetricTerms
      INTEGER selectCoriScheme
      INTEGER select3dCoriScheme
      INTEGER selectBotDragQuadr
      INTEGER selectPenetratingSW
      INTEGER pCellMix_select
      INTEGER readBinaryPrec
      INTEGER writeBinaryPrec
      INTEGER rwSuffixType
      INTEGER monitorSelect
      INTEGER debugLevel
      INTEGER plotLevel

C--   COMMON /PARM_L/ Logical valued parameters used by the model.
C- Coordinate + Grid params:
C     fluidIsAir       :: Set to indicate that the fluid major constituent
C                         is air
C     fluidIsWater     :: Set to indicate that the fluid major constituent
C                         is water
C     usingPCoords     :: Set to indicate that we are working in a pressure
C                         type coordinate (p or p*).
C     usingZCoords     :: Set to indicate that we are working in a height
C                         type coordinate (z or z*)
C     usingCartesianGrid :: If TRUE grid generation will be in a cartesian
C                           coordinate frame.
C     usingSphericalPolarGrid :: If TRUE grid generation will be in a
C                                spherical polar frame.
C     rotateGrid      :: rotate grid coordinates to geographical coordinates
C                        according to Euler angles phiEuler, thetaEuler, psiEuler
C     usingCylindricalGrid :: If TRUE grid generation will be Cylindrical
C     usingCurvilinearGrid :: If TRUE, use a curvilinear grid (to be provided)
C     hasWetCSCorners :: domain contains CS-type corners where dynamics is solved
C     deepAtmosphere :: deep model (drop the shallow-atmosphere approximation)
C     setInterFDr    :: set Interface depth (put cell-Center at the middle)
C     setCenterDr    :: set cell-Center depth (put Interface at the middle)
C     useMin4hFacEdges :: set hFacW,hFacS as minimum of adjacent hFacC factor
C     interViscAr_pCell :: account for partial-cell in interior vert. viscosity
C     interDiffKr_pCell :: account for partial-cell in interior vert. diffusion
C- Momentum params:
C     no_slip_sides  :: Impose "no-slip" at lateral boundaries.
C     no_slip_bottom :: Impose "no-slip" at bottom boundary.
C     bottomVisc_pCell :: account for partial-cell in bottom visc. (no-slip BC)
C     useSmag3D      :: Use isotropic 3-D Smagorinsky
C     useFullLeith   :: Set to true to use full Leith viscosity(may be unstable
C                       on irregular grids)
C     useStrainTensionVisc:: Set to true to use Strain-Tension viscous terms
C     useAreaViscLength :: Set to true to use old scaling for viscous lengths,
C                          e.g., L2=Raz.  May be preferable for cube sphere.
C     momViscosity  :: Flag which turns momentum friction terms on and off.
C     momAdvection  :: Flag which turns advection of momentum on and off.
C     momForcing    :: Flag which turns external forcing of momentum on and off.
C     momTidalForcing    :: Flag which turns tidal forcing on and off.
C     momPressureForcing :: Flag which turns pressure term in momentum equation
C                          on and off.
C     useNHMTerms   :: If TRUE use non-hydrostatic metric terms.
C     useCoriolis   :: Flag which turns the coriolis terms on and off.
C     useCDscheme   :: use CD-scheme to calculate Coriolis terms.
C     vectorInvariantMomentum :: use Vector-Invariant form (mom_vecinv package)
C                                (default = F = use mom_fluxform package)
C     useJamartMomAdv :: Use wet-point method for V.I. non-linear term
C     upwindVorticity :: bias interpolation of vorticity in the Coriolis term
C     highOrderVorticity :: use 3rd/4th order interp. of vorticity (V.I., advection)
C     useAbsVorticity :: work with f+zeta in Coriolis terms
C     upwindShear     :: use 1rst order upwind interp. (V.I., vertical advection)
C     momStepping    :: Turns momentum equation time-stepping off
C     calc_wVelocity :: Turns vertical velocity calculation off
C- Temp. & Salt params:
C     tempStepping   :: Turns temperature equation time-stepping on/off
C     saltStepping   :: Turns salinity equation time-stepping on/off
C     addFrictionHeating :: account for frictional heating
C     temp_stayPositive :: use Smolarkiewicz Hack to ensure Temp stays positive
C     salt_stayPositive :: use Smolarkiewicz Hack to ensure Salt stays positive
C     tempAdvection  :: Flag which turns advection of temperature on and off.
C     tempVertDiff4  :: use vertical bi-harmonic diffusion for temperature
C     tempIsActiveTr :: Pot.Temp. is a dynamically active tracer
C     tempForcing    :: Flag which turns external forcing of temperature on/off
C     saltAdvection  :: Flag which turns advection of salinity on and off.
C     saltVertDiff4  :: use vertical bi-harmonic diffusion for salinity
C     saltIsActiveTr :: Salinity  is a dynamically active tracer
C     saltForcing    :: Flag which turns external forcing of salinity on/off
C     maskIniTemp    :: apply mask to initial Pot.Temp.
C     maskIniSalt    :: apply mask to initial salinity
C     checkIniTemp   :: check for points with identically zero initial Pot.Temp.
C     checkIniSalt   :: check for points with identically zero initial salinity
C- Pressure solver related parameters (PARM02)
C     useNSACGSolver :: Set to true to use "not self-adjoint" conjugate
C                       gradient solver that stores the iteration history
C                       for an iterative adjoint as accuate as possible
C     useSRCGSolver  :: Set to true to use conjugate gradient
C                       solver with single reduction (only one call of
C                       s/r mpi_allreduce), default is false
C- Time-stepping & free-surface params:
C     rigidLid            :: Set to true to use rigid lid
C     implicitFreeSurface :: Set to true to use implicit free surface
C     uniformLin_PhiSurf  :: Set to true to use a uniform Bo_surf in the
C                            linear relation Phi_surf = Bo_surf*eta
C     uniformFreeSurfLev  :: TRUE if free-surface level-index is uniform (=1)
C     exactConserv        :: Set to true to conserve exactly the total Volume
C     linFSConserveTr     :: Set to true to correct source/sink of tracer
C                            at the surface due to Linear Free Surface
C     useRealFreshWaterFlux :: if True (=Natural BCS), treats P+R-E flux
C                         as a real Fresh Water (=> changes the Sea Level)
C                         if F, converts P+R-E to salt flux (no SL effect)
C     storePhiHyd4Phys :: store hydrostatic potential for use in Physics/EOS
C                         this requires specific code for restart & exchange
C     quasiHydrostatic :: Using non-hydrostatic terms in hydrostatic algorithm
C     nonHydrostatic   :: Using non-hydrostatic algorithm
C     use3Dsolver      :: set to true to use 3-D pressure solver
C     implicitIntGravWave :: treat Internal Gravity Wave implicitly
C     staggerTimeStep   :: enable a Stagger time stepping U,V (& W) then T,S
C     applyExchUV_early :: Apply EXCH to U,V earlier, just before integr_continuity
C     doResetHFactors   :: Do reset thickness factors @ beginning of each time-step
C     implicitDiffusion :: Turns implicit vertical diffusion on
C     implicitViscosity :: Turns implicit vertical viscosity on
C     tempImplVertAdv   :: Turns on implicit vertical advection for Temperature
C     saltImplVertAdv   :: Turns on implicit vertical advection for Salinity
C     momImplVertAdv    :: Turns on implicit vertical advection for Momentum
C     multiDimAdvection :: Flag that enable multi-dimension advection
C     useMultiDimAdvec  :: True if multi-dim advection is used at least once
C     momDissip_In_AB   :: if False, put Dissipation tendency contribution
C                          out off Adams-Bashforth time stepping.
C     doAB_onGtGs       :: if the Adams-Bashforth time stepping is used, always
C                          apply AB on tracer tendencies (rather than on Tracer)
C- Other forcing params -
C     balanceQnet     :: substract global mean of Qnet at every time step
C     balancePrintMean:: print substracted global means to STDOUT
C     doThetaClimRelax :: Set true if relaxation to temperature
C                        climatology is required.
C     doSaltClimRelax  :: Set true if relaxation to salinity
C                        climatology is required.
C     balanceThetaClimRelax :: substract global mean effect at every time step
C     balanceSaltClimRelax :: substract global mean effect at every time step
C     allowFreezing  :: Allows surface water to freeze and form ice
C     periodicExternalForcing :: Set true if forcing is time-dependant
C- I/O parameters -
C     globalFiles    :: Selects between "global" and "tiled" files.
C                       On some platforms with MPI, option globalFiles is either
C                       slow or does not work. Use useSingleCpuIO instead.
C     useSingleCpuIO :: moved to EEPARAMS.h
C     pickupStrictlyMatch :: check and stop if pickup-file do not stricly match
C     startFromPickupAB2 :: with AB-3 code, start from an AB-2 pickup
C     usePickupBeforeC54 :: start from old-pickup files, generated with code from
C                           before checkpoint-54a, Jul 06, 2004.
C     pickup_write_mdsio :: use mdsio to write pickups
C     pickup_read_mdsio  :: use mdsio to read  pickups
C     pickup_write_immed :: echo the pickup immediately (for conversion)
C     writePickupAtEnd   :: write pickup at the last timestep
C     snapshot_mdsio     :: use mdsio for "snapshot" (dumpfreq/diagfreq) output
C     monitor_stdio      :: use stdio for monitor output
C     dumpInitAndLast :: dumps model state to files at Initial (nIter0)
C                        & Last iteration, in addition multiple of dumpFreq iter.

      COMMON /PARM_L/
     & fluidIsAir, fluidIsWater,
     & usingPCoords, usingZCoords,
     & usingCartesianGrid, usingSphericalPolarGrid, rotateGrid,
     & usingCylindricalGrid, usingCurvilinearGrid, hasWetCSCorners,
     & deepAtmosphere, setInterFDr, setCenterDr, useMin4hFacEdges,
     & interViscAr_pCell, interDiffKr_pCell,
     & no_slip_sides, no_slip_bottom, bottomVisc_pCell, useSmag3D,
     & useFullLeith, useStrainTensionVisc, useAreaViscLength,
     & momViscosity, momAdvection, momForcing, momTidalForcing,
     & momPressureForcing, useNHMTerms,
     & useCoriolis, useCDscheme, vectorInvariantMomentum,
     & useJamartMomAdv, upwindVorticity, highOrderVorticity,
     & useAbsVorticity, upwindShear,
     & momStepping, calc_wVelocity, tempStepping, saltStepping,
     & addFrictionHeating, temp_stayPositive, salt_stayPositive,
     & tempAdvection, tempVertDiff4, tempIsActiveTr, tempForcing,
     & saltAdvection, saltVertDiff4, saltIsActiveTr, saltForcing,
     & maskIniTemp, maskIniSalt, checkIniTemp, checkIniSalt,
     & useNSACGSolver, useSRCGSolver,
     & rigidLid, implicitFreeSurface,
     & uniformLin_PhiSurf, uniformFreeSurfLev,
     & exactConserv, linFSConserveTr, useRealFreshWaterFlux,
     & storePhiHyd4Phys, quasiHydrostatic, nonHydrostatic,
     & use3Dsolver, implicitIntGravWave, staggerTimeStep,
     & applyExchUV_early, doResetHFactors,
     & implicitDiffusion, implicitViscosity,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & multiDimAdvection, useMultiDimAdvec,
     & momDissip_In_AB, doAB_onGtGs,
     & balanceQnet, balancePrintMean,
     & balanceThetaClimRelax, balanceSaltClimRelax,
     & doThetaClimRelax, doSaltClimRelax,
     & allowFreezing,
     & periodicExternalForcing,
     & globalFiles,
     & pickupStrictlyMatch, usePickupBeforeC54, startFromPickupAB2,
     & pickup_read_mdsio, pickup_write_mdsio, pickup_write_immed,
     & writePickupAtEnd,
     & snapshot_mdsio, monitor_stdio,
     & outputTypesInclusive, dumpInitAndLast

      LOGICAL fluidIsAir
      LOGICAL fluidIsWater
      LOGICAL usingPCoords
      LOGICAL usingZCoords
      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid, rotateGrid
      LOGICAL usingCylindricalGrid
      LOGICAL usingCurvilinearGrid, hasWetCSCorners
      LOGICAL deepAtmosphere
      LOGICAL setInterFDr
      LOGICAL setCenterDr
      LOGICAL useMin4hFacEdges
      LOGICAL interViscAr_pCell
      LOGICAL interDiffKr_pCell

      LOGICAL no_slip_sides
      LOGICAL no_slip_bottom
      LOGICAL bottomVisc_pCell
      LOGICAL useSmag3D
      LOGICAL useFullLeith
      LOGICAL useStrainTensionVisc
      LOGICAL useAreaViscLength
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL momTidalForcing
      LOGICAL momPressureForcing
      LOGICAL useNHMTerms

      LOGICAL useCoriolis
      LOGICAL useCDscheme
      LOGICAL vectorInvariantMomentum
      LOGICAL useJamartMomAdv
      LOGICAL upwindVorticity
      LOGICAL highOrderVorticity
      LOGICAL useAbsVorticity
      LOGICAL upwindShear
      LOGICAL momStepping
      LOGICAL calc_wVelocity
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL addFrictionHeating
      LOGICAL temp_stayPositive
      LOGICAL salt_stayPositive
      LOGICAL tempAdvection
      LOGICAL tempVertDiff4
      LOGICAL tempIsActiveTr
      LOGICAL tempForcing
      LOGICAL saltAdvection
      LOGICAL saltVertDiff4
      LOGICAL saltIsActiveTr
      LOGICAL saltForcing
      LOGICAL maskIniTemp
      LOGICAL maskIniSalt
      LOGICAL checkIniTemp
      LOGICAL checkIniSalt
      LOGICAL useNSACGSolver
      LOGICAL useSRCGSolver
      LOGICAL rigidLid
      LOGICAL implicitFreeSurface
      LOGICAL uniformLin_PhiSurf
      LOGICAL uniformFreeSurfLev
      LOGICAL exactConserv
      LOGICAL linFSConserveTr
      LOGICAL useRealFreshWaterFlux
      LOGICAL storePhiHyd4Phys
      LOGICAL quasiHydrostatic
      LOGICAL nonHydrostatic
      LOGICAL use3Dsolver
      LOGICAL implicitIntGravWave
      LOGICAL staggerTimeStep
      LOGICAL applyExchUV_early
      LOGICAL doResetHFactors
      LOGICAL implicitDiffusion
      LOGICAL implicitViscosity
      LOGICAL tempImplVertAdv
      LOGICAL saltImplVertAdv
      LOGICAL momImplVertAdv
      LOGICAL multiDimAdvection
      LOGICAL useMultiDimAdvec
      LOGICAL momDissip_In_AB
      LOGICAL doAB_onGtGs
      LOGICAL balanceQnet
      LOGICAL balancePrintMean
      LOGICAL doThetaClimRelax
      LOGICAL doSaltClimRelax
      LOGICAL balanceThetaClimRelax
      LOGICAL balanceSaltClimRelax
      LOGICAL allowFreezing
      LOGICAL periodicExternalForcing
      LOGICAL globalFiles
      LOGICAL pickupStrictlyMatch
      LOGICAL usePickupBeforeC54
      LOGICAL startFromPickupAB2
      LOGICAL pickup_read_mdsio, pickup_write_mdsio
      LOGICAL pickup_write_immed, writePickupAtEnd
      LOGICAL snapshot_mdsio, monitor_stdio
      LOGICAL outputTypesInclusive
      LOGICAL dumpInitAndLast

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     cg2dTargetResidual
C          :: Target residual for cg2d solver ; no unit (RHS normalisation)
C     cg2dTargetResWunit
C          :: Target residual for cg2d solver ; W unit (No RHS normalisation)
C     cg3dTargetResidual
C          :: Target residual for cg3d solver ; no unit (RHS normalisation)
C     cg3dTargetResWunit
C          :: Target residual for cg3d solver ; W unit (No RHS normalisation)
C     cg2dpcOffDFac :: Averaging weight for preconditioner off-diagonal.
C     Note. 20th May 1998
C           I made a weird discovery! In the model paper we argue
C           for the form of the preconditioner used here ( see
C           A Finite-volume, Incompressible Navier-Stokes Model
C           ...., Marshall et. al ). The algebra gives a simple
C           0.5 factor for the averaging of ac and aCw to get a
C           symmettric pre-conditioner. By using a factor of 0.51
C           i.e. scaling the off-diagonal terms in the
C           preconditioner down slightly I managed to get the
C           number of iterations for convergence in a test case to
C           drop form 192 -> 134! Need to investigate this further!
C           For now I have introduced a parameter cg2dpcOffDFac which
C           defaults to 0.51 but can be set at runtime.
C     delR      :: Vertical grid spacing ( units of r ).
C     delRc     :: Vertical grid spacing between cell centers (r unit).
C     delX      :: Separation between cell faces (m) or (deg), depending
C     delY         on input flags. Note: moved to header file SET_GRID.h
C     xgOrigin   :: Origin of the X-axis (Cartesian Grid) / Longitude of Western
C                :: most cell face (Lat-Lon grid) (Note: this is an "inert"
C                :: parameter but it makes geographical references simple.)
C     ygOrigin   :: Origin of the Y-axis (Cartesian Grid) / Latitude of Southern
C                :: most face (Lat-Lon grid).
C     rSphere    :: Radius of sphere for a spherical polar grid ( m ).
C     recip_rSphere :: Reciprocal radius of sphere ( m^-1 ).
C     radius_fromHorizGrid :: sphere Radius of input horiz. grid (Curvilinear Grid)
C     seaLev_Z   :: the reference height of sea-level (usually zero)
C     top_Pres   :: pressure (P-Coords) or reference pressure (Z-Coords) at the top
C     rSigmaBnd  :: vertical position (in r-unit) of r/sigma transition (Hybrid-Sigma)
C     gravity    :: Acceleration due to constant gravity ( m/s^2 )
C     recip_gravity :: Reciprocal gravity acceleration ( s^2/m )
C     gBaro      :: Accel. due to gravity used in barotropic equation ( m/s^2 )
C     gravFacC   :: gravity factor (vs surf. gravity) vert. profile at cell-Center
C     gravFacF   :: gravity factor (vs surf. gravity) vert. profile at cell-interF
C     rhoNil     :: Reference density for the linear equation of state
C     rhoConst   :: Vertically constant reference density (Boussinesq)
C     rho1Ref    :: reference vertical profile for density (anelastic)
C     rhoFacC    :: normalized (by rhoConst) reference density at cell-Center
C     rhoFacF    :: normalized (by rhoConst) reference density at cell-interFace
C     rhoConstFresh :: Constant reference density for fresh water (rain)
C     thetaConst :: Constant reference for potential temperature
C     tRef       :: reference vertical profile for potential temperature
C     sRef       :: reference vertical profile for salinity/specific humidity
C     rhoRef     :: density vertical profile from (tRef,sRef) [kg/m^3]
C     dBdrRef    :: vertical gradient of reference buoyancy  [(m/s/r)^2]:
C                :: z-coord: = N^2_ref = Brunt-Vaissala frequency [s^-2]
C                :: p-coord: = -(d.alpha/dp)_ref          [(m^2.s/kg)^2]
C     surf_pRef  :: surface reference pressure ( Pa )
C     pRef4EOS   :: reference pressure used in EOS (case selectP_inEOS_Zc=1)
C     phiRef     :: reference potential (press/rho, geopot) profile (m^2/s^2)
C     rVel2wUnit :: units conversion factor (Non-Hydrostatic code),
C                :: from r-coordinate vertical velocity to vertical velocity [m/s].
C                :: z-coord: = 1 ; p-coord: wSpeed [m/s] = rVel [Pa/s] * rVel2wUnit
C     wUnit2rVel :: units conversion factor (Non-Hydrostatic code),
C                :: from vertical velocity [m/s] to r-coordinate vertical velocity.
C                :: z-coord: = 1 ; p-coord: rVel [Pa/s] = wSpeed [m/s] * wUnit2rVel
C     rUnit2z    :: units conversion factor (for ocean in P-coord, only fct of k),
C                :: from r-coordinate to z [m] (at level center):
C                :: z-coord: = 1 ; p-coord: dz [m] = dr [Pa] * rUnit2z
C     z2rUnit    :: units conversion factor (for ocean in P-coord, only fct of k),
C                :: from z [m] to r-coordinate (at level center):
C                :: z-coord: = 1 ; p-coord: dr [Pa] = dz [m] * z2rUnit
C     mass2rUnit :: units conversion factor (surface forcing),
C                :: from mass per unit area [kg/m2] to vertical r-coordinate unit.
C                :: z-coord: = 1/rhoConst ( [kg/m2] / rho = [m] ) ;
C                :: p-coord: = gravity    ( [kg/m2] *  g = [Pa] ) ;
C     rUnit2mass :: units conversion factor (surface forcing),
C                :: from vertical r-coordinate unit to mass per unit area [kg/m2].
C                :: z-coord: = rhoConst  ( [m] * rho = [kg/m2] ) ;
C                :: p-coord: = 1/gravity ( [Pa] /  g = [kg/m2] ) ;
C     sIceLoadFac:: factor to scale (and turn off) sIceLoad (sea-ice loading)
C                   default = 1
C     f0         :: Reference coriolis parameter ( 1/s )
C                   ( Southern edge f for beta plane )
C     beta       :: df/dy ( s^-1.m^-1 )
C     fPrime     :: Second Coriolis parameter ( 1/s ), related to Y-component
C                   of rotation (reference value = 2.Omega.Cos(Phi))
C     omega      :: Angular velocity ( rad/s )
C     rotationPeriod :: Rotation period (s) (= 2.pi/omega)
C     viscArNr   :: vertical profile of Eddy viscosity coeff.
C                   for vertical mixing of momentum ( units of r^2/s )
C     viscAh     :: Eddy viscosity coeff. for mixing of
C                   momentum laterally ( m^2/s )
C     viscAhW    :: Eddy viscosity coeff. for mixing of vertical
C                   momentum laterally, no effect for hydrostatic
C                   model, defaults to viscAhD if unset ( m^2/s )
C                   Not used if variable horiz. viscosity is used.
C     viscA4     :: Biharmonic viscosity coeff. for mixing of
C                   momentum laterally ( m^4/s )
C     viscA4W    :: Biharmonic viscosity coeff. for mixing of vertical
C                   momentum laterally, no effect for hydrostatic
C                   model, defaults to viscA4D if unset ( m^2/s )
C                   Not used if variable horiz. viscosity is used.
C     viscAhD    :: Eddy viscosity coeff. for mixing of momentum laterally
C                   (act on Divergence part) ( m^2/s )
C     viscAhZ    :: Eddy viscosity coeff. for mixing of momentum laterally
C                   (act on Vorticity  part) ( m^2/s )
C     viscA4D    :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                   (act on Divergence part) ( m^4/s )
C     viscA4Z    :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                   (act on Vorticity  part) ( m^4/s )
C     smag3D_coeff     :: Isotropic 3-D Smagorinsky viscosity coefficient (-)
C     smag3D_diffCoeff :: Isotropic 3-D Smagorinsky diffusivity coefficient (-)
C     viscC2leith  :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC2leithD :: Modified Leith non-dimensional visc. factor (grad(div))
C     viscC2LeithQG:: QG Leith non-dimensional viscosity factor
C     viscC4leith  :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC4leithD :: Modified Leith non-dimensional viscosity factor (grad(div))
C     viscC2smag   :: Smagorinsky non-dimensional viscosity factor (harmonic)
C     viscC4smag   :: Smagorinsky non-dimensional viscosity factor (biharmonic)
C     viscAhMax    :: Maximum eddy viscosity coeff. for mixing of
C                    momentum laterally ( m^2/s )
C     viscAhReMax  :: Maximum gridscale Reynolds number for eddy viscosity
C                     coeff. for mixing of momentum laterally (non-dim)
C     viscAhGrid   :: non-dimensional grid-size dependent viscosity
C     viscAhGridMax:: maximum and minimum harmonic viscosity coefficients ...
C     viscAhGridMin::  in terms of non-dimensional grid-size dependent visc.
C     viscA4Max    :: Maximum biharmonic viscosity coeff. for mixing of
C                     momentum laterally ( m^4/s )
C     viscA4ReMax  :: Maximum Gridscale Reynolds number for
C                     biharmonic viscosity coeff. momentum laterally (non-dim)
C     viscA4Grid   :: non-dimensional grid-size dependent bi-harmonic viscosity
C     viscA4GridMax:: maximum and minimum biharmonic viscosity coefficients ...
C     viscA4GridMin::  in terms of non-dimensional grid-size dependent viscosity
C     diffKhT   :: Laplacian diffusion coeff. for mixing of
C                 heat laterally ( m^2/s )
C     diffK4T   :: Biharmonic diffusion coeff. for mixing of
C                 heat laterally ( m^4/s )
C     diffKrNrT :: vertical profile of Laplacian diffusion coeff.
C                 for mixing of heat vertically ( units of r^2/s )
C     diffKr4T  :: vertical profile of Biharmonic diffusion coeff.
C                 for mixing of heat vertically ( units of r^4/s )
C     diffKhS  ::  Laplacian diffusion coeff. for mixing of
C                 salt laterally ( m^2/s )
C     diffK4S   :: Biharmonic diffusion coeff. for mixing of
C                 salt laterally ( m^4/s )
C     diffKrNrS :: vertical profile of Laplacian diffusion coeff.
C                 for mixing of salt vertically ( units of r^2/s ),
C     diffKr4S  :: vertical profile of Biharmonic diffusion coeff.
C                 for mixing of salt vertically ( units of r^4/s )
C     diffKrBL79surf :: T/S surface diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79deep :: T/S deep diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79scl  :: depth scale for arctan fn (m) Bryan and Lewis, 1979
C     diffKrBL79Ho   :: depth offset for arctan fn (m) Bryan and Lewis, 1979
C     BL79LatVary    :: polarwise of this latitude diffKrBL79 is applied with
C                       gradual transition to diffKrBLEQ towards Equator
C     diffKrBLEQsurf :: same as diffKrBL79surf but at Equator
C     diffKrBLEQdeep :: same as diffKrBL79deep but at Equator
C     diffKrBLEQscl  :: same as diffKrBL79scl but at Equator
C     diffKrBLEQHo   :: same as diffKrBL79Ho but at Equator
C     pCellMix_maxFac :: maximum enhanced mixing factor for thin partial-cell
C     pCellMix_delR   :: thickness criteria   for too thin partial-cell
C     pCellMix_viscAr :: vertical viscosity   for too thin partial-cell
C     pCellMix_diffKr :: vertical diffusivity for too thin partial-cell
C     deltaT    :: Default timestep ( s )
C     deltaTClock  :: Timestep used as model "clock". This determines the
C                    IO frequencies and is used in tagging output. It can
C                    be totally different to the dynamical time. Typically
C                    it will be the deep-water timestep for accelerated runs.
C                    Frequency of checkpointing and dumping of the model state
C                    are referenced to this clock. ( s )
C     deltaTMom    :: Timestep for momemtum equations ( s )
C     dTtracerLev  :: Timestep for tracer equations ( s ), function of level k
C     deltaTFreeSurf :: Timestep for free-surface equation ( s )
C     freeSurfFac  :: Parameter to turn implicit free surface term on or off
C                     freeSurFac = 1. uses implicit free surface
C                     freeSurFac = 0. uses rigid lid
C     abEps        :: Adams-Bashforth-2 stabilizing weight
C     alph_AB      :: Adams-Bashforth-3 primary factor
C     beta_AB      :: Adams-Bashforth-3 secondary factor
C     implicSurfPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Surface Pressure Gradient ( 0-1 )
C     implicDiv2DFlow :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of barotropic flow Divergence ( 0-1 )
C     implicitNHPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Non-Hydrostatic Pressure Gradient ( 0-1 )
C     hFacMin      :: Minimum fraction size of a cell (affects hFacC etc...)
C     hFacMinDz    :: Minimum dimensional size of a cell (affects hFacC etc..., m)
C     hFacMinDp    :: Minimum dimensional size of a cell (affects hFacC etc..., Pa)
C     hFacMinDr    :: Minimum dimensional size of a cell (-> hFacC etc..., r units)
C     hFacInf      :: Threshold (inf and sup) for fraction size of surface cell
C     hFacSup          that control vanishing and creating levels
C     tauCD         :: CD scheme coupling timescale ( s )
C     rCD           :: CD scheme normalised coupling parameter (= 1 - deltaT/tauCD)
C     epsAB_CD      :: Adams-Bashforth-2 stabilizing weight used in CD scheme
C     baseTime      :: model base time (time origin) = time @ iteration zero
C     startTime     :: Starting time for this integration ( s ).
C     endTime       :: Ending time for this integration ( s ).
C     chkPtFreq     :: Frequency of rolling check pointing ( s ).
C     pChkPtFreq    :: Frequency of permanent check pointing ( s ).
C     dumpFreq      :: Frequency with which model state is written to
C                      post-processing files ( s ).
C     diagFreq      :: Frequency with which model writes diagnostic output
C                      of intermediate quantities.
C     afFacMom      :: Advection of momentum term multiplication factor
C     vfFacMom      :: Momentum viscosity term    multiplication factor
C     pfFacMom      :: Momentum pressure forcing  multiplication factor
C     cfFacMom      :: Coriolis term              multiplication factor
C     foFacMom      :: Momentum forcing           multiplication factor
C     mtFacMom      :: Metric terms               multiplication factor
C     cosPower      :: Power of cosine of latitude to multiply viscosity
C     cAdjFreq      :: Frequency of convective adjustment
C
C     tauThetaClimRelax :: Relaxation to climatology time scale ( s ).
C     tauSaltClimRelax :: Relaxation to climatology time scale ( s ).
C     latBandClimRelax :: latitude band where Relaxation to Clim. is applied,
C                         i.e. where |yC| <= latBandClimRelax
C     externForcingPeriod :: Is the period of which forcing varies (eg. 1 month)
C     externForcingCycle :: Is the repeat time of the forcing (eg. 1 year)
C                          (note: externForcingCycle must be an integer
C                           number times externForcingPeriod)
C     convertFW2Salt :: salinity, used to convert Fresh-Water Flux to Salt Flux
C                       (use model surface (local) value if set to -1)
C     temp_EvPrRn :: temperature of Rain & Evap.
C     salt_EvPrRn :: salinity of Rain & Evap.
C     temp_addMass :: temperature of addMass field
C     salt_addMass :: salinity of addMass field
C        (notes: a) tracer content of Rain/Evap only used if both
C                     NonLin_FrSurf & useRealFreshWater are set.
C                b) use model surface (local) value if set to UNSET_RL)
C     hMixCriteria:: criteria for mixed-layer diagnostic
C     dRhoSmall   :: parameter for mixed-layer diagnostic
C     hMixSmooth  :: Smoothing parameter for mixed-layer diag
C                    (default=0: no smoothing)
C     ivdc_kappa  :: implicit vertical diffusivity for convection [m^2/s]
C     sideDragFactor     :: side-drag scaling factor (used only if no_slip_sides)
C                           (default=2: full drag ; =1: gives half-slip BC)
C     bottomDragLinear    :: Linear    bottom-drag coefficient (units of [r]/s)
C     bottomDragQuadratic :: Quadratic bottom-drag coefficient (units of [r]/m)
C               (if using zcoordinate, units becomes linear: m/s, quadratic: [-])
C     zRoughBot :: roughness length for quadratic bottom friction coefficient
C                  (in m, typical values are order 0.01 m)
C     smoothAbsFuncRange :: 1/2 of interval around zero, for which FORTRAN ABS
C                           is to be replace by a smoother function
C                           (affects myabs, mymin, mymax)
C     nh_Am2        :: scales non-hydrostatic terms and changes internal scales
C                      (i.e. allows convection at different Rayleigh numbers)
C     tCylIn        :: Temperature of the cylinder inner boundary
C     tCylOut       :: Temperature of the cylinder outer boundary
C     phiEuler      :: Euler angle, rotation about original z-axis
C     thetaEuler    :: Euler angle, rotation about new x-axis
C     psiEuler      :: Euler angle, rotation about new z-axis
      COMMON /PARM_R/ cg2dTargetResidual, cg2dTargetResWunit,
     & cg2dpcOffDFac, cg3dTargetResidual, cg3dTargetResWunit,
     & delR, delRc, xgOrigin, ygOrigin, rSphere, recip_rSphere,
     & radius_fromHorizGrid, seaLev_Z, top_Pres, rSigmaBnd,
     & deltaT, deltaTMom, dTtracerLev, deltaTFreeSurf, deltaTClock,
     & abEps, alph_AB, beta_AB,
     & f0, beta, fPrime, omega, rotationPeriod,
     & viscFacAdj, viscAh, viscAhW, smag3D_coeff, smag3D_diffCoeff,
     & viscAhMax, viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC2leithD, viscC2LeithQG,
     & viscC2smag, viscC4smag,
     & viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W, viscA4Max,
     & viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscAhReMax, viscA4ReMax,
     & viscC4leith, viscC4leithD, viscArNr,
     & diffKhT, diffK4T, diffKrNrT, diffKr4T,
     & diffKhS, diffK4S, diffKrNrS, diffKr4S,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & BL79LatVary,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & pCellMix_maxFac, pCellMix_delR, pCellMix_viscAr, pCellMix_diffKr,
     & tauCD, rCD, epsAB_CD,
     & freeSurfFac, implicSurfPress, implicDiv2DFlow, implicitNHPress,
     & hFacMin, hFacMinDz, hFacInf, hFacSup,
     & gravity, recip_gravity, gBaro,
     & gravFacC, recip_gravFacC, gravFacF, recip_gravFacF,
     & rhoNil, rhoConst, recip_rhoConst, rho1Ref,
     & rhoFacC, recip_rhoFacC, rhoFacF, recip_rhoFacF, rhoConstFresh,
     & thetaConst, tRef, sRef, rhoRef, dBdrRef,
     & surf_pRef, pRef4EOS, phiRef,
     & rVel2wUnit, wUnit2rVel, rUnit2z, z2rUnit, mass2rUnit, rUnit2mass,
     & baseTime, startTime, endTime,
     & chkPtFreq, pChkPtFreq, dumpFreq, adjDumpFreq,
     & diagFreq, monitorFreq, adjMonitorFreq,
     & afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,
     & cosPower, cAdjFreq,
     & tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,
     & externForcingCycle, externForcingPeriod,
     & convertFW2Salt, temp_EvPrRn, salt_EvPrRn,
     & temp_addMass, salt_addMass, hFacMinDr, hFacMinDp,
     & ivdc_kappa, hMixCriteria, dRhoSmall, hMixSmooth,
     & sideDragFactor, bottomDragLinear, bottomDragQuadratic,
     & zRoughBot, nh_Am2, smoothAbsFuncRange, sIceLoadFac,
     & tCylIn, tCylOut,
     & phiEuler, thetaEuler, psiEuler

      Real*8 cg2dTargetResidual
      Real*8 cg2dTargetResWunit
      Real*8 cg3dTargetResidual
      Real*8 cg3dTargetResWunit
      Real*8 cg2dpcOffDFac
      Real*8 delR(Nr)
      Real*8 delRc(Nr+1)
      Real*8 xgOrigin
      Real*8 ygOrigin
      Real*8 rSphere
      Real*8 recip_rSphere
      Real*8 radius_fromHorizGrid
      Real*8 seaLev_Z
      Real*8 top_Pres
      Real*8 rSigmaBnd
      Real*8 deltaT
      Real*8 deltaTClock
      Real*8 deltaTMom
      Real*8 dTtracerLev(Nr)
      Real*8 deltaTFreeSurf
      Real*8 abEps, alph_AB, beta_AB
      Real*8 f0
      Real*8 beta
      Real*8 fPrime
      Real*8 omega
      Real*8 rotationPeriod
      Real*8 freeSurfFac
      Real*8 implicSurfPress
      Real*8 implicDiv2DFlow
      Real*8 implicitNHPress
      Real*8 hFacMin
      Real*8 hFacMinDz
      Real*8 hFacMinDp
      Real*8 hFacMinDr
      Real*8 hFacInf
      Real*8 hFacSup
      Real*8 viscArNr(Nr)
      Real*8 viscFacAdj
      Real*8 viscAh
      Real*8 viscAhW
      Real*8 viscAhD
      Real*8 viscAhZ
      Real*8 smag3D_coeff, smag3D_diffCoeff
      Real*8 viscAhMax
      Real*8 viscAhReMax
      Real*8 viscAhGrid, viscAhGridMax, viscAhGridMin
      Real*8 viscC2leith
      Real*8 viscC2leithD
      Real*8 viscC2LeithQG
      Real*8 viscC2smag
      Real*8 viscA4
      Real*8 viscA4W
      Real*8 viscA4D
      Real*8 viscA4Z
      Real*8 viscA4Max
      Real*8 viscA4ReMax
      Real*8 viscA4Grid, viscA4GridMax, viscA4GridMin
      Real*8 viscC4leith
      Real*8 viscC4leithD
      Real*8 viscC4smag
      Real*8 diffKhT
      Real*8 diffK4T
      Real*8 diffKrNrT(Nr)
      Real*8 diffKr4T(Nr)
      Real*8 diffKhS
      Real*8 diffK4S
      Real*8 diffKrNrS(Nr)
      Real*8 diffKr4S(Nr)
      Real*8 diffKrBL79surf
      Real*8 diffKrBL79deep
      Real*8 diffKrBL79scl
      Real*8 diffKrBL79Ho
      Real*8 BL79LatVary
      Real*8 diffKrBLEQsurf
      Real*8 diffKrBLEQdeep
      Real*8 diffKrBLEQscl
      Real*8 diffKrBLEQHo
      Real*8 pCellMix_maxFac
      Real*8 pCellMix_delR
      Real*8 pCellMix_viscAr(Nr)
      Real*8 pCellMix_diffKr(Nr)
      Real*8 tauCD, rCD, epsAB_CD
      Real*8 gravity,       recip_gravity
      Real*8 gBaro
      Real*8 gravFacC(Nr),   recip_gravFacC(Nr)
      Real*8 gravFacF(Nr+1), recip_gravFacF(Nr+1)
      Real*8 rhoNil
      Real*8 rhoConst,      recip_rhoConst
      Real*8 rho1Ref(Nr)
      Real*8 rhoFacC(Nr),   recip_rhoFacC(Nr)
      Real*8 rhoFacF(Nr+1), recip_rhoFacF(Nr+1)
      Real*8 rhoConstFresh
      Real*8 thetaConst
      Real*8 tRef(Nr)
      Real*8 sRef(Nr)
      Real*8 rhoRef(Nr)
      Real*8 dBdrRef(Nr)
      Real*8 surf_pRef, pRef4EOS(Nr)
      Real*8 phiRef(2*Nr+1)
      Real*8 rVel2wUnit(Nr+1), wUnit2rVel(Nr+1)
      Real*8 rUnit2z(Nr), z2rUnit(Nr)
      Real*8 mass2rUnit, rUnit2mass
      Real*8 baseTime
      Real*8 startTime
      Real*8 endTime
      Real*8 chkPtFreq
      Real*8 pChkPtFreq
      Real*8 dumpFreq
      Real*8 adjDumpFreq
      Real*8 diagFreq
      Real*8 monitorFreq
      Real*8 adjMonitorFreq
      Real*8 afFacMom
      Real*8 vfFacMom
      Real*8 pfFacMom
      Real*8 cfFacMom
      Real*8 foFacMom
      Real*8 mtFacMom
      Real*8 cosPower
      Real*8 cAdjFreq
      Real*8 tauThetaClimRelax
      Real*8 tauSaltClimRelax
      Real*8 latBandClimRelax
      Real*8 externForcingCycle
      Real*8 externForcingPeriod
      Real*8 convertFW2Salt
      Real*8 temp_EvPrRn
      Real*8 salt_EvPrRn
      Real*8 temp_addMass
      Real*8 salt_addMass
      Real*8 ivdc_kappa
      Real*8 hMixCriteria
      Real*8 dRhoSmall
      Real*8 hMixSmooth
      Real*8 sideDragFactor
      Real*8 bottomDragLinear
      Real*8 bottomDragQuadratic
      Real*8 zRoughBot
      Real*8 smoothAbsFuncRange
      Real*8 sIceLoadFac
      Real*8 nh_Am2
      Real*8 tCylIn, tCylOut
      Real*8 phiEuler, thetaEuler, psiEuler

C--   COMMON /PARM_A/ Thermodynamics constants ?
      COMMON /PARM_A/ HeatCapacity_Cp
      Real*8 HeatCapacity_Cp

C--   COMMON /PARM_ATM/ Atmospheric physical parameters (Ideal Gas EOS, ...)
C     celsius2K :: convert centigrade (Celsius) degree to Kelvin
C     atm_Po    :: standard reference pressure
C     atm_Cp    :: specific heat (Cp) of the (dry) air at constant pressure
C     atm_Rd    :: gas constant for dry air
C     atm_kappa :: kappa = R/Cp (R: constant of Ideal Gas EOS)
C     atm_Rq    :: water vapour specific volume anomaly relative to dry air
C                  (e.g. typical value = (29/18 -1) 10^-3 with q [g/kg])
C     integr_GeoPot :: option to select the way we integrate the geopotential
C                     (still a subject of discussions ...)
C     selectFindRoSurf :: select the way surf. ref. pressure (=Ro_surf) is
C             derived from the orography. Implemented: 0,1 (see INI_P_GROUND)
      COMMON /PARM_ATM/
     &            celsius2K,
     &            atm_Cp, atm_Rd, atm_kappa, atm_Rq, atm_Po,
     &            integr_GeoPot, selectFindRoSurf
      Real*8 celsius2K
      Real*8 atm_Po, atm_Cp, atm_Rd, atm_kappa, atm_Rq
      INTEGER integr_GeoPot, selectFindRoSurf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Logical flags for selecting packages
      LOGICAL useGAD
      LOGICAL useOBCS
      LOGICAL useSHAP_FILT
      LOGICAL useZONAL_FILT
      LOGICAL useOPPS
      LOGICAL usePP81
      LOGICAL useKL10
      LOGICAL useMY82
      LOGICAL useGGL90
      LOGICAL useKPP
      LOGICAL useGMRedi
      LOGICAL useDOWN_SLOPE
      LOGICAL useBBL
      LOGICAL useCAL
      LOGICAL useEXF
      LOGICAL useBulkForce
      LOGICAL useEBM
      LOGICAL useCheapAML
      LOGICAL useAUTODIFF
      LOGICAL useGrdchk
      LOGICAL useSMOOTH
      LOGICAL usePROFILES
      LOGICAL useECCO
      LOGICAL useCTRL
      LOGICAL useSBO
      LOGICAL useFLT
      LOGICAL usePTRACERS
      LOGICAL useGCHEM
      LOGICAL useRBCS
      LOGICAL useOffLine
      LOGICAL useMATRIX
      LOGICAL useFRAZIL
      LOGICAL useSEAICE
      LOGICAL useSALT_PLUME
      LOGICAL useShelfIce
      LOGICAL useSTIC
      LOGICAL useStreamIce
      LOGICAL useICEFRONT
      LOGICAL useThSIce
      LOGICAL useLand
      LOGICAL useATM2d
      LOGICAL useAIM
      LOGICAL useAtm_Phys
      LOGICAL useFizhi
      LOGICAL useGridAlt
      LOGICAL useDiagnostics
      LOGICAL useREGRID
      LOGICAL useLayers
      LOGICAL useMNC
      LOGICAL useRunClock
      LOGICAL useEMBED_FILES
      LOGICAL useMYPACKAGE
      COMMON /PARM_PACKAGES/
     &        useGAD, useOBCS, useSHAP_FILT, useZONAL_FILT,
     &        useOPPS, usePP81, useKL10, useMY82, useGGL90, useKPP,
     &        useGMRedi, useBBL, useDOWN_SLOPE,
     &        useCAL, useEXF, useBulkForce, useEBM, useCheapAML,
     &        useGrdchk, useSMOOTH, usePROFILES, useECCO, useCTRL,
     &        useSBO, useFLT, useAUTODIFF,
     &        usePTRACERS, useGCHEM, useRBCS, useOffLine, useMATRIX,
     &        useFRAZIL, useSEAICE, useSALT_PLUME, useShelfIce, useSTIC,
     &        useStreamIce, useICEFRONT, useThSIce, useLand,
     &        useATM2d, useAIM, useAtm_Phys, useFizhi, useGridAlt,
     &        useDiagnostics, useREGRID, useLayers, useMNC,
     &        useRunClock, useEMBED_FILES,
     &        useMYPACKAGE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C
CBOP
C    !ROUTINE: GRID.h
C    !INTERFACE:
C    include GRID.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | GRID.h
C     | o Header file defining model grid.
C     *==========================================================*
C     | Model grid is defined for each process by reference to
C     | the arrays set here.
C     | Notes
C     | =====
C     | The standard MITgcm convention of westmost, southern most
C     | and upper most having the (1,1,1) index is used here.
C     | i.e.
C     |----------------------------------------------------------
C     | (1)  Plan view schematic of model grid (top layer i.e. )
C     |      ================================= ( ocean surface )
C     |                                        ( or top of     )
C     |                                        ( atmosphere    )
C     |      This diagram shows the location of the model
C     |      prognostic variables on the model grid. The "T"
C     |      location is used for all tracers. The figure also
C     |      shows the southern most, western most indexing
C     |      convention that is used for all model variables.
C     |
C     |
C     |             V(i=1,                     V(i=Nx,
C     |               j=Ny+1,                    j=Ny+1,
C     |               k=1)                       k=1)
C     |                /|\                       /|\  "PWX"
C     |       |---------|------------------etc..  |---- *---
C     |       |                     |                   *  |
C     |"PWY"*******************************etc..  **********"PWY"
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x           |             x     *==>U
C     |  j=Ny,|      T(i=1,         |          T(i=Nx,  *(i=Nx+1,
C     |  k=1) |        j=Ny,        |            j=Ny,  *  |j=Ny,
C     |       |        k=1)         |            k=1)   *  |k=1)
C     |
C     |       .                     .                      .
C     |       .                     .                      .
C     |       .                     .                      .
C     |       e                     e                   *  e
C     |       t                     t                   *  t
C     |       c                     c                   *  c
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x           |             x     *  |
C     |  j=2, |      T(i=1,         |          T(i=Nx,  *  |
C     |  k=1) |        j=2,         |            j=2,   *  |
C     |       |        k=1)         |            k=1)   *  |
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |      -----------|------------------etc..  |-----*---
C     |       |       V(i=1,        |           V(i=Nx, *  |
C     |       |         j=2,        |             j=2,  *  |
C     |       |         k=1)        |             k=1)  *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
C     |       |        k=1)         |            k=1)   *  |k=1)
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |"SB"++>|---------|------------------etc..  |-----*---
C     |      /+\      V(i=1,                    V(i=Nx, *
C     |       +         j=1,                      j=1,  *
C     |       +         k=1)                      k=1)  *
C     |     "WB"                                      "PWX"
C     |
C     |   N, y increasing northwards
C     |  /|\ j increasing northwards
C     |   |
C     |   |
C     |   ======>E, x increasing eastwards
C     |             i increasing eastwards
C     |
C     |    i: East-west index
C     |    j: North-south index
C     |    k: up-down index
C     |    U: x-velocity (m/s)
C     |    V: y-velocity (m/s)
C     |    T: potential temperature (oC)
C     | "SB": Southern boundary
C     | "WB": Western boundary
C     |"PWX": Periodic wrap around in X.
C     |"PWY": Periodic wrap around in Y.
C     |----------------------------------------------------------
C     | (2) South elevation schematic of model grid
C     |     =======================================
C     |     This diagram shows the location of the model
C     |     prognostic variables on the model grid. The "T"
C     |     location is used for all tracers. The figure also
C     |     shows the upper most, western most indexing
C     |     convention that is used for all model variables.
C     |
C     |      "WB"
C     |       +
C     |       +
C     |      \+/       /|\                       /|\       .
C     |"UB"++>|-------- | -----------------etc..  | ----*---
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |
C     |       |         j=1,        |             j=1,  *  |
C     |       |         k=1)        |             k=1)  *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
C     |       |        k=1)         |            k=1)   *  |k=1)
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |       |-------- | -----------------etc..  | ----*---
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |
C     |       |         j=1,        |             j=1,  *  |
C     |       |         k=2)        |             k=2)  *  |
C     |
C     |       .                     .                      .
C     |       .                     .                      .
C     |       .                     .                      .
C     |       e                     e                   *  e
C     |       t                     t                   *  t
C     |       c                     c                   *  c
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |       |-------- | -----------------etc..  | ----*---
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |
C     |       |         j=1,        |             j=1,  *  |
C     |       |         k=Nr)       |             k=Nr) *  |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
C     |  k=Nr)|        j=1,         |  k=Nr)     j=1,   *  |j=1,
C     |       |        k=Nr)        |            k=Nr)  *  |k=Nr)
C     |       |                     |                   *  |
C     |"LB"++>==============================================
C     |                                               "PWX"
C     |
C     | Up   increasing upwards.
C     |/|\                                                       .
C     | |
C     | |
C     | =====> E  i increasing eastwards
C     | |         x increasing eastwards
C     | |
C     |\|/
C     | Down,k increasing downwards.
C     |
C     | Note: r => height (m) => r increases upwards
C     |       r => pressure (Pa) => r increases downwards
C     |
C     |
C     |    i: East-west index
C     |    j: North-south index
C     |    k: up-down index
C     |    U: x-velocity (m/s)
C     | rVel: z-velocity ( units of r )
C     |       The vertical velocity variable rVel is in units of
C     |       "r" the vertical coordinate. r in m will give
C     |       rVel m/s. r in Pa will give rVel Pa/s.
C     |    T: potential temperature (oC)
C     | "UB": Upper boundary.
C     | "LB": Lower boundary (always solid - therefore om|w == 0)
C     | "WB": Western boundary
C     |"PWX": Periodic wrap around in X.
C     |----------------------------------------------------------
C     | (3) Views showing nomenclature and indexing
C     |     for grid descriptor variables.
C     |
C     |      Fig 3a. shows the orientation, indexing and
C     |      notation for the grid spacing terms used internally
C     |      for the evaluation of gradient and averaging terms.
C     |      These varaibles are set based on the model input
C     |      parameters which define the model grid in terms of
C     |      spacing in X, Y and Z.
C     |
C     |      Fig 3b. shows the orientation, indexing and
C     |      notation for the variables that are used to define
C     |      the model grid. These varaibles are set directly
C     |      from the model input.
C     |
C     | Figure 3a
C     | =========
C     |       |------------------------------------
C     |       |                       |
C     |"PWY"********************************* etc...
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |
C     |       .                       .
C     |       .                       .
C     |       .                       .
C     |       e                       e
C     |       t                       t
C     |       c                       c
C     |       |-----------v-----------|-----------v----------|-
C     |       |                       |                      |
C     |       |                       |                      |
C     |       |                       |                      |
C     |       |                       |                      |
C     |       |                       |                      |
C     |       u<--dxF(i=1,j=2,k=1)--->u           t          |
C     |       |/|\       /|\          |                      |
C     |       | |         |           |                      |
C     |       | |         |           |                      |
C     |       | |         |           |                      |
C     |       |dyU(i=1,  dyC(i=1,     |                      |
C     | ---  ---|--j=2,---|--j=2,-----------------v----------|-
C     | /|\   | |  k=1)   |  k=1)     |          /|\         |
C     |  |    | |         |           |          dyF(i=2,    |
C     |  |    | |         |           |           |  j=1,    |
C     |dyG(   |\|/       \|/          |           |  k=1)    |
C     |   i=1,u---        t<---dxC(i=2,j=1,k=1)-->t          |
C     |   j=1,|                       |           |          |
C     |   k=1)|                       |           |          |
C     |  |    |                       |           |          |
C     |  |    |                       |           |          |
C     | \|/   |           |<---dxV(i=2,j=1,k=1)--\|/         |
C     |"SB"++>|___________v___________|___________v__________|_
C     |       <--dxG(i=1,j=1,k=1)----->
C     |      /+\                                              .
C     |       +
C     |       +
C     |     "WB"
C     |
C     |   N, y increasing northwards
C     |  /|\ j increasing northwards
C     |   |
C     |   |
C     |   ======>E, x increasing eastwards
C     |             i increasing eastwards
C     |
C     |    i: East-west index
C     |    j: North-south index
C     |    k: up-down index
C     |    u: x-velocity point
C     |    V: y-velocity point
C     |    t: tracer point
C     | "SB": Southern boundary
C     | "WB": Western boundary
C     |"PWX": Periodic wrap around in X.
C     |"PWY": Periodic wrap around in Y.
C     |
C     | Figure 3b
C     | =========
C     |
C     |       .                       .
C     |       .                       .
C     |       .                       .
C     |       e                       e
C     |       t                       t
C     |       c                       c
C     |       |-----------v-----------|-----------v--etc...
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       u<--delX(i=1)---------->u           t
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |-----------v-----------------------v--etc...
C     |       |          /|\          |
C     |       |           |           |
C     |       |           |           |
C     |       |           |           |
C     |       u        delY(j=1)      |           t
C     |       |           |           |
C     |       |           |           |
C     |       |           |           |
C     |       |           |           |
C     |       |          \|/          |
C     |"SB"++>|___________v___________|___________v__etc...
C     |      /+\                                                 .
C     |       +
C     |       +
C     |     "WB"
C     |
C     *==========================================================*
C     \ev
CEOP

C     Macros that override/modify standard definitions

C
CBOP
C    !ROUTINE: GRID_MACROS.h
C    !INTERFACE:
C    include GRID_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | GRID_MACROS.h
C     *==========================================================*
C     | These macros are used to substitute definitions for
C     | GRID.h variables for particular configurations.
C     | In setting these variables the following convention
C     | applies.
C     | undef  phi_CONST   - Indicates the variable phi is fixed
C     |                      in X, Y and Z.
C     | undef  phi_FX      - Indicates the variable phi only
C     |                      varies in X (i.e.not in X or Z).
C     | undef  phi_FY      - Indicates the variable phi only
C     |                      varies in Y (i.e.not in X or Z).
C     | undef  phi_FXY     - Indicates the variable phi only
C     |                      varies in X and Y ( i.e. not Z).
C     *==========================================================*
C     \ev
CEOP


C
CBOP
C    !ROUTINE: DXC_MACROS.h
C    !INTERFACE:
C    include DXC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXC_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DXF_MACROS.h
C    !INTERFACE:
C    include DXF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXF_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DXG_MACROS.h
C    !INTERFACE:
C    include DXG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXG_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DXV_MACROS.h
C    !INTERFACE:
C    include DXV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXV_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYC_MACROS.h
C    !INTERFACE:
C    include DYC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYC_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYF_MACROS.h
C    !INTERFACE:
C    include DYF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYF_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYG_MACROS.h
C    !INTERFACE:
C    include DYG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYG_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYU_MACROS.h
C    !INTERFACE:
C    include DYU_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYU_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: HFACC_MACROS.h
C    !INTERFACE:
C    include HFACC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACC_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: HFACS_MACROS.h
C    !INTERFACE:
C    include HFACS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACS_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: HFACW_MACROS.h
C    !INTERFACE:
C    include HFACW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACW_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: RECIP_DXC_MACROS.h
C    !INTERFACE:
C    include RECIP_DXC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXC_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DXF_MACROS.h
C    !INTERFACE:
C    include RECIP_DXF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXF_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DXG_MACROS.h
C    !INTERFACE:
C    include RECIP_DXG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXG_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DXV_MACROS.h
C    !INTERFACE:
C    include RECIP_DXV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXV_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYC_MACROS.h
C    !INTERFACE:
C    include RECIP_DYC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYC_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYF_MACROS.h
C    !INTERFACE:
C    include RECIP_DYF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYF_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYG_MACROS.h
C    !INTERFACE:
C    include RECIP_DYG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYG_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYU_MACROS.h
C    !INTERFACE:
C    include RECIP_DYU_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYU_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_HFACC_MACROS.h
C    !INTERFACE:
C    include RECIP_HFACC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_HFACC_MACROS.h                                      
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: RECIP_HFACS_MACROS.h
C    !INTERFACE:
C    include RECIP_HFACS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_HFACS_MACROS.h                                      
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: RECIP_HFACW_MACROS.h
C    !INTERFACE:
C    include RECIP_HFACW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_HFACW_MACROS.h                                      
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: XC_MACROS.h
C    !INTERFACE:
C    include XC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | XC_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: YC_MACROS.h
C    !INTERFACE:
C    include YC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | YC_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RA_MACROS.h
C    !INTERFACE:
C    include RA_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RA_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP








C
CBOP
C    !ROUTINE: RAW_MACROS.h
C    !INTERFACE:
C    include RAW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RAW_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP








C
CBOP
C    !ROUTINE: RAS_MACROS.h
C    !INTERFACE:
C    include RAS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RAS_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: MASKW_MACROS.h
C    !INTERFACE:
C    include MASKW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | MASKW_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP












C
CBOP
C    !ROUTINE: MASKS_MACROS.h
C    !INTERFACE:
C    include MASKS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | MASKS_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP












C
CBOP
C    !ROUTINE: TANPHIATU_MACROS.h
C    !INTERFACE:
C    include TANPHIATU_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | TANPHIATU_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: TANPHIATV_MACROS.h
C    !INTERFACE:
C    include TANPHIATV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | TANPHIATV_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: FCORI_MACROS.h
C    !INTERFACE:
C    include FCORI_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | FCORI_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP









C--   COMMON /GRID_RL/ RL valued grid defining variables.
C     deepFacC  :: deep-model grid factor (fct of vertical only) for dx,dy
C     deepFacF     at level-center (deepFacC)  and level interface (deepFacF)
C     deepFac2C :: deep-model grid factor (fct of vertical only) for area dx*dy
C     deepFac2F    at level-center (deepFac2C) and level interface (deepFac2F)
C     gravitySign :: indicates the direction of gravity relative to R direction
C                   (= -1 for R=Z (Z increases upward, -gravity direction  )
C                   (= +1 for R=P (P increases downward, +gravity direction)
C     rkSign     :: Vertical coordinate to vertical index orientation.
C                   ( +1 same orientation, -1 opposite orientation )
C     globalArea :: Domain Integrated horizontal Area [m2]
      COMMON /GRID_RL/
     &  cosFacU, cosFacV, sqCosFacU, sqCosFacV,
     &  deepFacC, deepFac2C, recip_deepFacC, recip_deepFac2C,
     &  deepFacF, deepFac2F, recip_deepFacF, recip_deepFac2F,
     &  gravitySign, rkSign, globalArea
      Real*8 cosFacU        (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cosFacV        (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 sqCosFacU      (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 sqCosFacV      (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 deepFacC       (Nr)
      Real*8 deepFac2C      (Nr)
      Real*8 deepFacF       (Nr+1)
      Real*8 deepFac2F      (Nr+1)
      Real*8 recip_deepFacC (Nr)
      Real*8 recip_deepFac2C(Nr)
      Real*8 recip_deepFacF (Nr+1)
      Real*8 recip_deepFac2F(Nr+1)
      Real*8 gravitySign
      Real*8 rkSign
      Real*8 globalArea

C--   COMMON /GRID_RS/ RS valued grid defining variables.
C     dxC     :: Cell center separation in X across western cell wall (m)
C     dxG     :: Cell face separation in X along southern cell wall (m)
C     dxF     :: Cell face separation in X thru cell center (m)
C     dxV     :: V-point separation in X across south-west corner of cell (m)
C     dyC     :: Cell center separation in Y across southern cell wall (m)
C     dyG     :: Cell face separation in Y along western cell wall (m)
C     dyF     :: Cell face separation in Y thru cell center (m)
C     dyU     :: U-point separation in Y across south-west corner of cell (m)
C     drC     :: Cell center separation along Z axis ( units of r ).
C     drF     :: Cell face separation along Z axis ( units of r ).
C     R_low   :: base of fluid in r_unit (Depth(m) / Pressure(Pa) at top Atmos.)
C     rLowW   :: base of fluid column in r_unit at Western  edge location.
C     rLowS   :: base of fluid column in r_unit at Southern edge location.
C     Ro_surf :: surface reference (at rest) position, r_unit.
C     rSurfW  :: surface reference position at Western  edge location [r_unit].
C     rSurfS  :: surface reference position at Southern edge location [r_unit].
C     hFac    :: Fraction of cell in vertical which is open i.e how
C              "lopped" a cell is (dimensionless scale factor).
C              Note: The code needs terms like MIN(hFac,hFac(I-1))
C                    On some platforms it may be better to precompute
C                    hFacW, hFacS, ... here than do MIN on the fly.
C     maskInC :: Cell Center 2-D Interior mask (i.e., zero beyond OB)
C     maskInW :: West  face 2-D Interior mask (i.e., zero on and beyond OB)
C     maskInS :: South face 2-D Interior mask (i.e., zero on and beyond OB)
C     maskC   :: cell Center land mask
C     maskW   :: West face land mask
C     maskS   :: South face land mask
C     recip_dxC   :: Reciprocal of dxC
C     recip_dxG   :: Reciprocal of dxG
C     recip_dxF   :: Reciprocal of dxF
C     recip_dxV   :: Reciprocal of dxV
C     recip_dyC   :: Reciprocal of dxC
C     recip_dyG   :: Reciprocal of dyG
C     recip_dyF   :: Reciprocal of dyF
C     recip_dyU   :: Reciprocal of dyU
C     recip_drC   :: Reciprocal of drC
C     recip_drF   :: Reciprocal of drF
C     recip_Rcol  :: Inverse of cell center column thickness (1/r_unit)
C     recip_hFacC :: Inverse of cell open-depth f[X,Y,Z] ( dimensionless ).
C     recip_hFacW    rhFacC center, rhFacW west, rhFacS south.
C     recip_hFacS   Note: This is precomputed here because it involves division.
C     xC     :: X-coordinate of cell center f[X,Y]. The units of xc, yc
C               depend on the grid. They are not used in differencing or
C               averaging but are just a convient quantity for I/O,
C               diagnostics etc.. As such xc is in m for cartesian
C               coordinates but degrees for spherical polar.
C     yC     :: Y-coordinate of center of cell f[X,Y].
C     yG     :: Y-coordinate of corner of cell (c-grid vorticity point) f[X,Y].
C     rA     :: R-face are f[X,Y] ( m^2 ).
C               Note: In a cartesian framework rA is simply dx*dy,
C                   however we use rA to allow for non-globally
C                   orthogonal coordinate frames (with appropriate
C                   metric terms).
C     rC     :: R-coordinate of center of cell f[Z] (units of r).
C     rF     :: R-coordinate of face of cell f[Z] (units of r).
C - *HybSigm* - :: Hybrid-Sigma vert. Coord coefficients
C     aHybSigmF    at level-interface (*HybSigmF) and level-center (*HybSigmC)
C     aHybSigmC    aHybSigm* = constant r part, bHybSigm* = sigma part, such as
C     bHybSigmF    r(ij,k,t) = rLow(ij) + aHybSigm(k)*[rF(1)-rF(Nr+1)]
C     bHybSigmC              + bHybSigm(k)*[eta(ij,t)+Ro_surf(ij) - rLow(ij)]
C     dAHybSigF :: vertical increment of Hybrid-Sigma coeff.: constant r part,
C     dAHybSigC    between interface (dAHybSigF) and between center (dAHybSigC)
C     dBHybSigF :: vertical increment of Hybrid-Sigma coefficient: sigma part,
C     dBHybSigC    between interface (dBHybSigF) and between center (dBHybSigC)
C     tanPhiAtU :: tan of the latitude at U point. Used for spherical polar
C                  metric term in U equation.
C     tanPhiAtV :: tan of the latitude at V point. Used for spherical polar
C                  metric term in V equation.
C     angleCosC :: cosine of grid orientation angle relative to Geographic
C direction at cell center: alpha=(Eastward_dir,grid_uVel_dir)=(North_d,vVel_d)
C     angleSinC :: sine   of grid orientation angle relative to Geographic
C direction at cell center: alpha=(Eastward_dir,grid_uVel_dir)=(North_d,vVel_d)
C     u2zonDir  :: cosine of grid orientation angle at U point location
C     v2zonDir  :: minus sine of  orientation angle at V point location
C     fCori     :: Coriolis parameter at grid Center point
C     fCoriG    :: Coriolis parameter at grid Corner point
C     fCoriCos  :: Coriolis Cos(phi) parameter at grid Center point (for NH)

      COMMON /GRID_RS/
     &  dxC,dxF,dxG,dxV,dyC,dyF,dyG,dyU,
     &  rLowW, rLowS,
     &  Ro_surf, rSurfW, rSurfS,
     &  recip_dxC,recip_dxF,recip_dxG,recip_dxV,
     &  recip_dyC,recip_dyF,recip_dyG,recip_dyU,
     &  xC,yC,rA,rAw,rAs,rAz,xG,yG,
     &  maskInC, maskInW, maskInS,
     &  maskC, maskW, maskS,
     &  recip_rA,recip_rAw,recip_rAs,recip_rAz,
     &  drC, drF, recip_drC, recip_drF, rC, rF,
     &  aHybSigmF, bHybSigmF, aHybSigmC, bHybSigmC,
     &  dAHybSigF, dBHybSigF, dBHybSigC, dAHybSigC,
     &  tanPhiAtU, tanPhiAtV,
     &  angleCosC, angleSinC, u2zonDir, v2zonDir,
     &  fCori, fCoriG, fCoriCos
      Real*8 dxC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dxF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dxG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dxV            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyU            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rLowW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rLowS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 Ro_surf        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rSurfW         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rSurfS         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 xC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 xG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 yC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 yG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rA             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rAw            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rAs            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rAz            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rAw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rAs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rAz      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskInC        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskInW        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskInS        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 maskW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 maskS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 drC            (Nr+1)
      Real*8 drF            (Nr)
      Real*8 recip_drC      (Nr+1)
      Real*8 recip_drF      (Nr)
      Real*8 rC             (Nr)
      Real*8 rF             (Nr+1)
      Real*8 aHybSigmF      (Nr+1)
      Real*8 bHybSigmF      (Nr+1)
      Real*8 aHybSigmC      (Nr)
      Real*8 bHybSigmC      (Nr)
      Real*8 dAHybSigF      (Nr)
      Real*8 dBHybSigF      (Nr)
      Real*8 dBHybSigC      (Nr+1)
      Real*8 dAHybSigC      (Nr+1)
      Real*8 tanPhiAtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 tanPhiAtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 angleCosC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 angleSinC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 u2zonDir       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 v2zonDir       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 fCori          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 fCoriG         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 fCoriCos       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /GRID_VAR_RS/ potentially time-dependent or active RS
C     valued grid defining variables. These grid defining variables are
C     time-dependent when using a non-linear free surface, or they are
C     active in an AD sense when using depth as a control parameter, or
C     both.
      COMMON /GRID_VAR_RS/
     &  hFacC, hFacW, hFacS,
     &  recip_hFacC,recip_hFacW,recip_hFacS,
     &  R_low, recip_Rcol
      Real*8 hFacC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 hFacW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 hFacS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 recip_hFacC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 recip_hFacW    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 recip_hFacS    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 R_low          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_Rcol     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)



C--   COMMON /GRID_I/ INTEGER valued grid defining variables.
C     kSurfC  :: vertical index of the surface tracer cell
C     kSurfW  :: vertical index of the surface U point
C     kSurfS  :: vertical index of the surface V point
C     kLowC   :: index of the r-lowest "wet cell" (2D)
C IMPORTANT: kLowC = 0 and kSurfC,W,S = Nr+1 (or =Nr+2 on a thin-wall)
C            where the fluid column is empty (continent)
      COMMON /GRID_I/
     &  kSurfC, kSurfW, kSurfS,
     &  kLowC
      INTEGER kSurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kSurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kSurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kLowC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

!     NetCDF-3.!
! netcdf version 3 fortran interface:
!

!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      integer nf_ubyte
      integer nf_ushort
      integer nf_uint
      integer nf_int64
      integer nf_uint64

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
      parameter (nf_ubyte = 7)
      parameter (nf_ushort = 8)
      parameter (nf_uint = 9)
      parameter (nf_int64 = 10)
      parameter (nf_uint64 = 11)

!
! default fill values:
!
      integer           nf_fill_byte
      integer           nf_fill_int1
      integer           nf_fill_char
      integer           nf_fill_short
      integer           nf_fill_int2
      integer           nf_fill_int
      real              nf_fill_float
      real              nf_fill_real
      doubleprecision   nf_fill_double

      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690d+36)

!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_64bit_data
      integer nf_cdf5
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      integer nf_format_64bit_offset
      integer nf_format_64bit_data
      integer nf_format_cdf5
      integer nf_diskless
      integer nf_mmap

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_64bit_data = 32)
      parameter (nf_cdf5 = nf_64bit_data)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
      parameter (nf_format_64bit_offset = nf_format_64bit)
      parameter (nf_format_64bit_data = 5)
      parameter (nf_format_cdf5 = nf_format_64bit_data)
      parameter (nf_diskless = 8)
      parameter (nf_mmap = 16)

!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)

!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)

!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims

      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)

!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc

      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer  nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)

!
! miscellaneous routines:
!
      character*80   nf_inq_libvers
      external       nf_inq_libvers

      character*80   nf_strerror
!                         (integer             ncerr)
      external       nf_strerror

      logical        nf_issyserr
!                         (integer             ncerr)
      external       nf_issyserr

!
! control routines:
!
      integer         nf_inq_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_inq_base_pe

      integer         nf_set_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_set_base_pe

      integer         nf_create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             ncid)
      external        nf_create

      integer         nf__create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create

      integer         nf__create_mp
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create_mp

      integer         nf_open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             ncid)
      external        nf_open

      integer         nf__open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open

      integer         nf__open_mp
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open_mp

      integer         nf_set_fill
!                         (integer             ncid,
!                          integer             fillmode,
!                          integer             old_mode)
      external        nf_set_fill

      integer         nf_set_default_format
!                          (integer             format,
!                          integer             old_format)
      external        nf_set_default_format

      integer         nf_redef
!                         (integer             ncid)
      external        nf_redef

      integer         nf_enddef
!                         (integer             ncid)
      external        nf_enddef

      integer         nf__enddef
!                         (integer             ncid,
!                          integer             h_minfree,
!                          integer             v_align,
!                          integer             v_minfree,
!                          integer             r_align)
      external        nf__enddef

      integer         nf_sync
!                         (integer             ncid)
      external        nf_sync

      integer         nf_abort
!                         (integer             ncid)
      external        nf_abort

      integer         nf_close
!                         (integer             ncid)
      external        nf_close

      integer         nf_delete
!                         (character*(*)       ncid)
      external        nf_delete

!
! general inquiry routines:
!

      integer         nf_inq
!                         (integer             ncid,
!                          integer             ndims,
!                          integer             nvars,
!                          integer             ngatts,
!                          integer             unlimdimid)
      external        nf_inq

! new inquire path

      integer nf_inq_path
      external nf_inq_path

      integer         nf_inq_ndims
!                         (integer             ncid,
!                          integer             ndims)
      external        nf_inq_ndims

      integer         nf_inq_nvars
!                         (integer             ncid,
!                          integer             nvars)
      external        nf_inq_nvars

      integer         nf_inq_natts
!                         (integer             ncid,
!                          integer             ngatts)
      external        nf_inq_natts

      integer         nf_inq_unlimdim
!                         (integer             ncid,
!                          integer             unlimdimid)
      external        nf_inq_unlimdim

      integer         nf_inq_format
!                         (integer             ncid,
!                          integer             format)
      external        nf_inq_format

!
! dimension routines:
!

      integer         nf_def_dim
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             len,
!                          integer             dimid)
      external        nf_def_dim

      integer         nf_inq_dimid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             dimid)
      external        nf_inq_dimid

      integer         nf_inq_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_dim

      integer         nf_inq_dimname
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_inq_dimname

      integer         nf_inq_dimlen
!                         (integer             ncid,
!                          integer             dimid,
!                          integer             len)
      external        nf_inq_dimlen

      integer         nf_rename_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_rename_dim

!
! general attribute routines:
!

      integer         nf_inq_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len)
      external        nf_inq_att

      integer         nf_inq_attid
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             attnum)
      external        nf_inq_attid

      integer         nf_inq_atttype
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype)
      external        nf_inq_atttype

      integer         nf_inq_attlen
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_attlen

      integer         nf_inq_attname
!                         (integer             ncid,
!                          integer             varid,
!                          integer             attnum,
!                          character(*)        name)
      external        nf_inq_attname

      integer         nf_copy_att
!                         (integer             ncid_in,
!                          integer             varid_in,
!                          character(*)        name,
!                          integer             ncid_out,
!                          integer             varid_out)
      external        nf_copy_att

      integer         nf_rename_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        curname,
!                          character(*)        newname)
      external        nf_rename_att

      integer         nf_del_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_del_att

!
! attribute put/get routines:
!

      integer         nf_put_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len,
!                          character(*)        text)
      external        nf_put_att_text

      integer         nf_get_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          character(*)        text)
      external        nf_get_att_text

      integer         nf_put_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int1_t           i1vals(1))
      external        nf_put_att_int1

      integer         nf_get_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int1_t           i1vals(1))
      external        nf_get_att_int1

      integer         nf_put_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int2_t           i2vals(1))
      external        nf_put_att_int2

      integer         nf_get_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int2_t           i2vals(1))
      external        nf_get_att_int2

      integer         nf_put_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          integer             ivals(1))
      external        nf_put_att_int

      integer         nf_get_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             ivals(1))
      external        nf_get_att_int

      integer         nf_put_att_int64
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int8_t           i8vals(1))
      external        nf_put_att_int64

      integer         nf_get_att_int64
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int8_t           i8vals(1))
      external        nf_get_att_int64

      integer         nf_put_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          real                rvals(1))
      external        nf_put_att_real

      integer         nf_get_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          real                rvals(1))
      external        nf_get_att_real

      integer         nf_put_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          double              dvals(1))
      external        nf_put_att_double

      integer         nf_get_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          double              dvals(1))
      external        nf_get_att_double

!
! general variable routines:
!

      integer         nf_def_var
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             varid)
      external        nf_def_var

      integer         nf_inq_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             natts)
      external        nf_inq_var

      integer         nf_inq_varid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             varid)
      external        nf_inq_varid

      integer         nf_inq_varname
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_inq_varname

      integer         nf_inq_vartype
!                         (integer             ncid,
!                          integer             varid,
!                          integer             xtype)
      external        nf_inq_vartype

      integer         nf_inq_varndims
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ndims)
      external        nf_inq_varndims

      integer         nf_inq_vardimid
!                         (integer             ncid,
!                          integer             varid,
!                          integer             dimids(1))
      external        nf_inq_vardimid

      integer         nf_inq_varnatts
!                         (integer             ncid,
!                          integer             varid,
!                          integer             natts)
      external        nf_inq_varnatts

      integer         nf_rename_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_rename_var

      integer         nf_copy_var
!                         (integer             ncid_in,
!                          integer             varid,
!                          integer             ncid_out)
      external        nf_copy_var

!
! entire variable put/get routines:
!

      integer         nf_put_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_put_var_text

      integer         nf_get_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_get_var_text

      integer         nf_put_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_put_var_int1

      integer         nf_get_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_get_var_int1

      integer         nf_put_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_put_var_int2

      integer         nf_get_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_get_var_int2

      integer         nf_put_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_put_var_int

      integer         nf_get_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_get_var_int

      integer         nf_put_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_put_var_real

      integer         nf_get_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_get_var_real

      integer         nf_put_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_put_var_double

      integer         nf_get_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_get_var_double

!
! single variable put/get routines:
!

      integer         nf_put_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_put_var1_text

      integer         nf_get_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_get_var1_text

      integer         nf_put_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_put_var1_int1

      integer         nf_get_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_get_var1_int1

      integer         nf_put_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_put_var1_int2

      integer         nf_get_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_get_var1_int2

      integer         nf_put_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_put_var1_int

      integer         nf_get_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_get_var1_int

      integer         nf_put_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_put_var1_real

      integer         nf_get_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_get_var1_real

      integer         nf_put_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_put_var1_double

      integer         nf_get_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_get_var1_double

!
! variable array put/get routines:
!

      integer         nf_put_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_put_vara_text

      integer         nf_get_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_get_vara_text

      integer         nf_put_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vara_int1

      integer         nf_get_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vara_int1

      integer         nf_put_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vara_int2

      integer         nf_get_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vara_int2

      integer         nf_put_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_put_vara_int

      integer         nf_get_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_get_vara_int

      integer         nf_put_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_put_vara_real

      integer         nf_get_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_get_vara_real

      integer         nf_put_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vara_double

      integer         nf_get_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vara_double

!
! strided variable put/get routines:
!

      integer         nf_put_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_put_vars_text

      integer         nf_get_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_get_vars_text

      integer         nf_put_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vars_int1

      integer         nf_get_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vars_int1

      integer         nf_put_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vars_int2

      integer         nf_get_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vars_int2

      integer         nf_put_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_put_vars_int

      integer         nf_get_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_get_vars_int

      integer         nf_put_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_put_vars_real

      integer         nf_get_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_get_vars_real

      integer         nf_put_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vars_double

      integer         nf_get_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vars_double

!
! mapped variable put/get routines:
!

      integer         nf_put_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_put_varm_text

      integer         nf_get_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_get_varm_text

      integer         nf_put_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_varm_int1

      integer         nf_get_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_varm_int1

      integer         nf_put_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_varm_int2

      integer         nf_get_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_varm_int2

      integer         nf_put_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_put_varm_int

      integer         nf_get_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_get_varm_int

      integer         nf_put_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_put_varm_real

      integer         nf_get_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_get_varm_real

      integer         nf_put_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_put_varm_double

      integer         nf_get_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_get_varm_double

!     64-bit int functions.
      integer nf_put_var1_int64
      external nf_put_var1_int64
      integer nf_put_vara_int64
      external nf_put_vara_int64
      integer nf_put_vars_int64
      external nf_put_vars_int64
      integer nf_put_varm_int64
      external nf_put_varm_int64
      integer nf_put_var_int64
      external nf_put_var_int64
      integer nf_get_var1_int64
      external nf_get_var1_int64
      integer nf_get_vara_int64
      external nf_get_vara_int64
      integer nf_get_vars_int64
      external nf_get_vars_int64
      integer nf_get_varm_int64
      external nf_get_varm_int64
      integer nf_get_var_int64
      external nf_get_var_int64


!     NetCDF-4.!     This is part of netCDF-4. Copyright 2006, UCAR, See COPYRIGHT
!     file for distribution information.

!     Netcdf version 4 fortran interface.

!     $Id: netcdf4.inc,v 1.28 2010/05/25 13:53:02 ed Exp $

!     New netCDF-4 types.
      integer nf_string
      integer nf_vlen
      integer nf_opaque
      integer nf_enum
      integer nf_compound

      parameter (nf_string = 12)
      parameter (nf_vlen = 13)
      parameter (nf_opaque = 14)
      parameter (nf_enum = 15)
      parameter (nf_compound = 16)

!     New netCDF-4 fill values.
      integer           nf_fill_ubyte
      integer           nf_fill_ushort
!      real              nf_fill_uint
!      real              nf_fill_int64
!      real              nf_fill_uint64
      parameter (nf_fill_ubyte = 255)
      parameter (nf_fill_ushort = 65535)

!     New constants.
      integer nf_format_netcdf4
      parameter (nf_format_netcdf4 = 3)

      integer nf_format_netcdf4_classic
      parameter (nf_format_netcdf4_classic = 4)

      integer nf_netcdf4
      parameter (nf_netcdf4 = 4096)

      integer nf_classic_model
      parameter (nf_classic_model = 256)

      integer nf_chunk_seq
      parameter (nf_chunk_seq = 0)
      integer nf_chunk_sub
      parameter (nf_chunk_sub = 1)
      integer nf_chunk_sizes
      parameter (nf_chunk_sizes = 2)

      integer nf_endian_native
      parameter (nf_endian_native = 0)
      integer nf_endian_little
      parameter (nf_endian_little = 1)
      integer nf_endian_big
      parameter (nf_endian_big = 2)

!     For NF_DEF_VAR_CHUNKING
      integer nf_chunked
      parameter (nf_chunked = 0)
      integer nf_contiguous
      parameter (nf_contiguous = 1)
      integer nf_compact
      parameter (nf_compact = 2)

!     For NF_DEF_VAR_FLETCHER32
      integer nf_nochecksum
      parameter (nf_nochecksum = 0)
      integer nf_fletcher32
      parameter (nf_fletcher32 = 1)

!     For NF_DEF_VAR_DEFLATE
      integer nf_noshuffle
      parameter (nf_noshuffle = 0)
      integer nf_shuffle
      parameter (nf_shuffle = 1)

!     For NF_DEF_VAR_SZIP
      integer nf_szip_ec_option_mask
      parameter (nf_szip_ec_option_mask = 4)
      integer nf_szip_nn_option_mask
      parameter (nf_szip_nn_option_mask = 32)

!     For parallel I/O.
      integer nf_mpiio      
      parameter (nf_mpiio = 8192)
      integer nf_mpiposix
      parameter (nf_mpiposix = 16384)
      integer nf_pnetcdf
      parameter (nf_pnetcdf = 32768)

!     For NF_VAR_PAR_ACCESS.
      integer nf_independent
      parameter (nf_independent = 0)
      integer nf_collective
      parameter (nf_collective = 1)

!     For NF_DEF_VAR_QUANTIZE.
      integer nf_noquantize
      parameter (nf_noquantize = 0)
      integer nf_quantize_bitgroom
      parameter (nf_quantize_bitgroom = 1)
      integer nf_quantize_granularbr
      parameter (nf_quantize_granularbr = 2)
      integer nf_quantize_bitround
      parameter (nf_quantize_bitround = 3)

!     New error codes.
      integer nf_ehdferr        ! Error at HDF5 layer. 
      parameter (nf_ehdferr = -101)
      integer nf_ecantread      ! Can't read. 
      parameter (nf_ecantread = -102)
      integer nf_ecantwrite     ! Can't write. 
      parameter (nf_ecantwrite = -103)
      integer nf_ecantcreate    ! Can't create. 
      parameter (nf_ecantcreate = -104)
      integer nf_efilemeta      ! Problem with file metadata. 
      parameter (nf_efilemeta = -105)
      integer nf_edimmeta       ! Problem with dimension metadata. 
      parameter (nf_edimmeta = -106)
      integer nf_eattmeta       ! Problem with attribute metadata. 
      parameter (nf_eattmeta = -107)
      integer nf_evarmeta       ! Problem with variable metadata. 
      parameter (nf_evarmeta = -108)
      integer nf_enocompound    ! Not a compound type. 
      parameter (nf_enocompound = -109)
      integer nf_eattexists     ! Attribute already exists. 
      parameter (nf_eattexists = -110)
      integer nf_enotnc4        ! Attempting netcdf-4 operation on netcdf-3 file.   
      parameter (nf_enotnc4 = -111)
      integer nf_estrictnc3     ! Attempting netcdf-4 operation on strict nc3 netcdf-4 file.   
      parameter (nf_estrictnc3 = -112)
      integer nf_enotnc3        ! Attempting netcdf-3 operation on netcdf-4 file.   
      parameter (nf_enotnc3 = -113)
      integer nf_enopar         ! Parallel operation on file opened for non-parallel access.   
      parameter (nf_enopar = -114)
      integer nf_eparinit       ! Error initializing for parallel access.   
      parameter (nf_eparinit = -115)
      integer nf_ebadgrpid      ! Bad group ID.   
      parameter (nf_ebadgrpid = -116)
      integer nf_ebadtypid      ! Bad type ID.   
      parameter (nf_ebadtypid = -117)
      integer nf_etypdefined    ! Type has already been defined and may not be edited. 
      parameter (nf_etypdefined = -118)
      integer nf_ebadfield      ! Bad field ID.   
      parameter (nf_ebadfield = -119)
      integer nf_ebadclass      ! Bad class.   
      parameter (nf_ebadclass = -120)
      integer nf_emaptype       ! Mapped access for atomic types only.   
      parameter (nf_emaptype = -121)
      integer nf_elatefill      ! Attempt to define fill value when data already exists. 
      parameter (nf_elatefill = -122)
      integer nf_elatedef       ! Attempt to define var properties, like deflate, after enddef. 
      parameter (nf_elatedef = -123)
      integer nf_edimscale      ! Probem with HDF5 dimscales. 
      parameter (nf_edimscale = -124)
      integer nf_enogrp       ! No group found.
      parameter (nf_enogrp = -125)
      integer nf_estorage ! Can't specify both contiguous and chunking. 
      parameter (nf_estorage = -126)    
      integer nf_ebadchunk ! Bad chunksize. 
      parameter (nf_ebadchunk = -127)    
      integer nf_enotbuilt       ! NetCDF feature not built.
      parameter (nf_enotbuilt = -128)
      integer nf_ediskless ! Error in using diskless  access. 
      parameter (nf_ediskless = -129)    
      integer nf_ecantextend ! Attempt to extend dataset during ind. I/O operation. 
      parameter (nf_ecantextend = -130)    
      integer nf_empi ! MPI operation failed. 
      parameter (nf_empi = -131)    
      integer nf_efilter ! Filter operation failed. 
      parameter (nf_efilter = -132)    
      integer nf_ercfile ! RC file failure 
      parameter (nf_ercfile = -133)    
      integer nf_enullpad ! Header Bytes not Null-Byte padded 
      parameter (nf_enullpad = -134)    
      integer nf_einmemory ! In-memory file error 
      parameter (nf_einmemory = -135)    
      integer nf_enofilter ! Filter not defined on variable. 
      parameter (nf_enofilter = -136)    
      integer nf_enczarr ! Error at NCZarr layer. 
      parameter (nf_enczarr = -137)    
      integer nf_es3 ! Generic S3 error 
      parameter (nf_es3 = -138)    
      integer nf_eempty ! Attempt to read empty NCZarr map key 
      parameter (nf_eempty = -139)    
      integer nf_eobject ! Some object exists when it should not 
      parameter (nf_eobject = -140)    
      integer nf_enoobject ! Some object not found 
      parameter (nf_enoobject = -141)    
      integer nf_eplugin ! Unclassified failure in accessing a dynamically loaded plugin> 
      parameter (nf_eplugin = -142)    


!     New functions.

!     Parallel I/O.
      integer nf_create_par
      external nf_create_par

      integer nf_open_par
      external nf_open_par

      integer nf_var_par_access
      external nf_var_par_access

!     Functions to handle groups.
      integer nf_inq_ncid
      external nf_inq_ncid

      integer nf_inq_grps
      external nf_inq_grps

      integer nf_inq_grpname
      external nf_inq_grpname

      integer nf_inq_grpname_full
      external nf_inq_grpname_full

      integer nf_inq_grpname_len
      external nf_inq_grpname_len

      integer nf_inq_grp_parent
      external nf_inq_grp_parent

      integer nf_inq_grp_ncid
      external nf_inq_grp_ncid

      integer nf_inq_grp_full_ncid
      external nf_inq_grp_full_ncid

      integer nf_inq_varids
      external nf_inq_varids

      integer nf_inq_dimids
      external nf_inq_dimids

      integer nf_def_grp
      external nf_def_grp

!     New rename grp function

      integer nf_rename_grp
      external nf_rename_grp

!     New options for netCDF variables.
      integer nf_def_var_deflate
      external nf_def_var_deflate

      integer nf_inq_var_deflate
      external nf_inq_var_deflate

      integer nf_def_var_zstandard
      external nf_def_var_zstandard

      integer nf_inq_var_zstandard
      external nf_inq_var_zstandard

      integer nf_def_var_szip
      external nf_def_var_szip

      integer nf_inq_var_szip
      external nf_inq_var_szip

      integer nf_def_var_quantize
      external nf_def_var_quantize

      integer nf_inq_var_quantize
      external nf_inq_var_quantize

      integer nf_def_var_fletcher32
      external nf_def_var_fletcher32

      integer nf_inq_var_fletcher32
      external nf_inq_var_fletcher32

      integer nf_def_var_chunking
      external nf_def_var_chunking

      integer nf_inq_var_chunking
      external nf_inq_var_chunking

      integer nf_def_var_fill
      external nf_def_var_fill

      integer nf_inq_var_fill
      external nf_inq_var_fill

      integer nf_def_var_endian
      external nf_def_var_endian

      integer nf_inq_var_endian
      external nf_inq_var_endian

      integer nf_def_var_filter
      external nf_def_var_filter

      integer nf_inq_var_filter
      external nf_inq_var_filter

!     User defined types.
      integer nf_inq_typeids
      external nf_inq_typeids

      integer nf_inq_typeid
      external nf_inq_typeid

      integer nf_inq_type
      external nf_inq_type

      integer nf_inq_user_type
      external nf_inq_user_type

!     User defined types - compound types.
      integer nf_def_compound
      external nf_def_compound

      integer nf_insert_compound
      external nf_insert_compound

      integer nf_insert_array_compound
      external nf_insert_array_compound

      integer nf_inq_compound
      external nf_inq_compound

      integer nf_inq_compound_name
      external nf_inq_compound_name

      integer nf_inq_compound_size
      external nf_inq_compound_size

      integer nf_inq_compound_nfields
      external nf_inq_compound_nfields

      integer nf_inq_compound_field
      external nf_inq_compound_field

      integer nf_inq_compound_fieldname
      external nf_inq_compound_fieldname

      integer nf_inq_compound_fieldindex
      external nf_inq_compound_fieldindex

      integer nf_inq_compound_fieldoffset
      external nf_inq_compound_fieldoffset

      integer nf_inq_compound_fieldtype
      external nf_inq_compound_fieldtype

      integer nf_inq_compound_fieldndims
      external nf_inq_compound_fieldndims

      integer nf_inq_compound_fielddim_sizes
      external nf_inq_compound_fielddim_sizes

!     User defined types - variable length arrays.
      integer nf_def_vlen
      external nf_def_vlen

      integer nf_inq_vlen
      external nf_inq_vlen

      integer nf_free_vlen
      external nf_free_vlen

!     User defined types - enums.
      integer nf_def_enum
      external nf_def_enum

      integer nf_insert_enum
      external nf_insert_enum

      integer nf_inq_enum
      external nf_inq_enum

      integer nf_inq_enum_member
      external nf_inq_enum_member

      integer nf_inq_enum_ident
      external nf_inq_enum_ident

!     User defined types - opaque.
      integer nf_def_opaque
      external nf_def_opaque

      integer nf_inq_opaque
      external nf_inq_opaque

!     Write and read attributes of any type, including user defined
!     types.
      integer nf_put_att
      external nf_put_att
      integer nf_get_att
      external nf_get_att

!     Write and read variables of any type, including user defined
!     types.
      integer nf_put_var
      external nf_put_var
      integer nf_put_var1
      external nf_put_var1
      integer nf_put_vara
      external nf_put_vara
      integer nf_put_vars
      external nf_put_vars
      integer nf_get_var
      external nf_get_var
      integer nf_get_var1
      external nf_get_var1
      integer nf_get_vara
      external nf_get_vara
      integer nf_get_vars
      external nf_get_vars

!     For helping F77 users with VLENs.
      integer nf_get_vlen_element
      external nf_get_vlen_element
      integer nf_put_vlen_element
      external nf_put_vlen_element

!     For dealing with file level chunk cache.
      integer nf_set_chunk_cache
      external nf_set_chunk_cache
      integer nf_get_chunk_cache
      external nf_get_chunk_cache

!     For dealing with per variable chunk cache.
      integer nf_set_var_chunk_cache
      external nf_set_var_chunk_cache
      integer nf_get_var_chunk_cache
      external nf_get_var_chunk_cache

!     NetCDF-2.!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!      
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil

      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil


      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!     
!     masks for the struct nc flag field passed in as 'mode' arg to
!     nccreate and ncopen.
!     

!     read/write, 0 => readonly 
      parameter(ncrdwr = 1)
!     in create phase, cleared by ncendef 
      parameter(nccreat = 2)
!     on create destroy existing file 
      parameter(ncexcl = 4)
!     in define mode, cleared by ncendef 
      parameter(ncindef = 8)
!     synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
!     synchronise whole header on change (x'20')
      parameter(nchsync = 32)
!     numrecs has changed (x'40')
      parameter(ncndirty = 64)  
!     header info has changed (x'80')
      parameter(nchdirty = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
!     do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
!     isa link (x'8000')
      parameter(nclink = 32768)

!     
!     'mode' arguments for nccreate and ncopen
!     
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!     
!     'size' argument to ncdimdef for an unlimited dimension
!     
      integer ncunlim
      parameter(ncunlim = 0)

!     
!     attribute id to put/get a global attribute
!     
      parameter(ncglobal  = 0)

!     
!     advisory maximums:
!     
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
!     not enforced 
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)

!     
!     global netcdf error status variable
!     initialized in error.c
!     

!     no error 
      parameter(ncnoerr = nf_noerr)
!     not a netcdf id 
      parameter(ncebadid = nf_ebadid)
!     too many netcdfs open 
      parameter(ncenfile = -31)   ! nc_syserr
!     netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
!     invalid argument 
      parameter(nceinval = nf_einval)
!     write to read only 
      parameter(nceperm = nf_eperm)
!     operation not allowed in data mode 
      parameter(ncenotin = nf_enotindefine )   
!     operation not allowed in define mode 
      parameter(nceindef = nf_eindefine)   
!     coordinates out of domain 
      parameter(ncecoord = nf_einvalcoords)
!     maxncdims exceeded 
      parameter(ncemaxds = nf_emaxdims)
!     string match to name in use 
      parameter(ncename = nf_enameinuse)   
!     attribute not found 
      parameter(ncenoatt = nf_enotatt)
!     maxncattrs exceeded 
      parameter(ncemaxat = nf_emaxatts)
!     not a netcdf data type 
      parameter(ncebadty = nf_ebadtype)
!     invalid dimension id 
      parameter(ncebadd = nf_ebaddim)
!     ncunlimited in the wrong index 
      parameter(nceunlim = nf_eunlimpos)
!     maxncvars exceeded 
      parameter(ncemaxvs = nf_emaxvars)
!     variable not found 
      parameter(ncenotvr = nf_enotvar)
!     action prohibited on ncglobal varid 
      parameter(nceglob = nf_eglobal)
!     not a netcdf file 
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname) 
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!     
!     global options variable. used to determine behavior of error handler.
!     initialized in lerror.c
!     
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
!     default fill values.  these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)


C     Functions
      integer IFNBLNK, ILNBLNK

C     !INPUT PARAMETERS:
      character*(*) fname
      character*(*) cvname
      integer fid, did, bi,bj
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i, vid, nnf, nnl, doit, err
      integer nids, cv_did(1), xtmin,ytmin
      character*(MAX_LEN_MBUF) msgbuf
      integer cv_start(1), cv_count(1)
      Real*8 rtmp(sNx + 2*OLx + sNy + 2*OLy + Nr)
C     variables for text attributes
      integer MAX_LEN_NAME, ia
      PARAMETER ( MAX_LEN_NAME = 128 )
      character*(MAX_LEN_NAME) units, long_name, positive

      DO i=1,MAX_LEN_NAME
         units(i:i)     = ' '
         long_name(i:i) = ' '
         positive(i:i)  = ' '
      ENDDO

      nnf = IFNBLNK(cvname)
      nnl = ILNBLNK(cvname)

      xtmin = 0
      ytmin = 0

      IF ( .NOT. useCubedSphereExchange ) THEN
C     make sure for a non-cubed-sphere curvi-linear grid,
C     that the X/Y coordinate variables are monotonous
C     bi+(myXGlobalLo-1)/sNx is the i-tile number
C     bj+(myYGlobalLo-1)/sNy is the j-tile number
       xtmin = sNx * ( bi+(myXGlobalLo-1)/sNx - 1 )
       ytmin = sNy * ( bj+(myYGlobalLo-1)/sNy - 1 )
      ENDIF

      doit = 1
      nids = 1
      cv_did(1)= did

C     Check all the coordinate variables that we know about
      IF (cvname(nnf:nnl) .EQ. 'X') THEN

        cv_start(1) = 1
        cv_count(1) = sNx

        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xtmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xC(i,1,bi,bj)
         ENDDO
        ENDIF

        IF ( usingCartesianGrid ) THEN
         long_name = 'X-coordinate of cell center'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'i-index of cell center'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'longitude of cell center'
         units     = 'degrees_east'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'polar angle coordinate of cell center'
         units     = 'degrees'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Xp1') THEN

        cv_start(1) = 1
        cv_count(1) = sNx + 1

        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xtmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xG(i,1,bi,bj)
         ENDDO
        ENDIF

        IF ( usingCartesianGrid ) THEN
         long_name = 'X-Coordinate of cell corner'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'i-index of cell corner'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'longitude of cell corner'
         units     = 'degrees_east'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'polar angle  of cell corner'
         units     = 'degrees'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Xwh') THEN

        cv_start(1) = 1
        cv_count(1) = sNx + 2*OLx

        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xtmin - OLx + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xC(i,1,bi,bj)
CML????          rtmp(i) = xC(i-Olx,1,bi,bj)
         ENDDO
        ENDIF

        IF ( usingCartesianGrid ) THEN
         long_name = 'X-Coordinate of cell center including overlaps'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'i-index of cell center including overlaps'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'longitude of cell center including overlaps'
         units     = 'degrees_east'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name =
     &        'polar angle coordinate of cell center including overlaps'
         units     = 'degrees'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Y') THEN

        cv_start(1) = 1
        cv_count(1) = sNy

        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = ytmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = yC(1,i,bi,bj)
         ENDDO
        ENDIF

        IF ( usingCartesianGrid ) THEN
         long_name = 'Y-Coordinate of cell center'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'j-index of cell center'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'latitude of cell center'
         units     = 'degrees_north'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'radial coordinate of cell center'
         units     = 'meters'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Yp1') THEN

        cv_start(1) = 1
        cv_count(1) = sNy + 1

        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = ytmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = yG(1,i,bi,bj)
         ENDDO
        ENDIF

        IF ( usingCartesianGrid ) THEN
         long_name = 'Y-Coordinate of cell corner'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'j-index of cell corner'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'latitude of cell corner'
         units     = 'degrees_north'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'radial coordinate of cell corner'
         units     = 'meters'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Ywh') THEN

        cv_start(1) = 1
        cv_count(1) = sNy + 2*OLy

        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = ytmin - OLy + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = yC(1,i-OLy,bi,bj)
         ENDDO
        ENDIF

        IF ( usingCartesianGrid ) THEN
         long_name = 'Y-Coordinate of cell center including overlaps'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'j-index of cell center including overlaps'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'latitude of cell center including overlaps'
         units     = 'degrees_north'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name =
     &        'radial coordinate of cell center including overlaps'
         units     = 'meters'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Z') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rC(i)
        ENDDO
C
        long_name = 'vertical coordinate of cell center'
        IF ( usingZCoords ) THEN
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zp1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr + 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO
C
        long_name = 'vertical coordinate of cell interface'
        IF ( usingZCoords ) THEN
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zu') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i + 1)
        ENDDO
C
        IF ( usingZCoords ) THEN
         long_name = 'vertical coordinate of lower cell interface'
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         long_name = 'vertical coordinate of upper cell interface'
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zl') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO
C
        IF ( usingZCoords ) THEN
         long_name = 'vertical coordinate of upper cell interface'
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         long_name = 'vertical coordinate of lower cell interface'
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zm1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr - 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i + 1)
        ENDDO
C
        IF ( usingZCoords ) THEN
         long_name = 'vertical coordinate of lower cell interface'
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         long_name = 'vertical coordinate of upper cell interface'
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSE

        doit = 0

      ENDIF

      IF ( doit .EQ. 1 ) THEN

        CALL MNC_FILE_REDEF(fname, myThid)

        err = NF_DEF_VAR(fid, cvname, NF_DOUBLE,
     &       nids, cv_did, vid)

        i = ILNBLNK( fname )
        write(msgbuf,'(5a)') 'defining coordinate variable ''',
     &       cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
C     add attributes if set
        ia = ILNBLNK(long_name)
        IF ( ia .GT. 0 ) THEN
         err = NF_PUT_ATT_TEXT(fid, vid, 'long_name', ia, long_name)
         write(msgbuf,'(5a)')
     &     'adding attribute ''long_name'' to coordinate variable ''',
     &     cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
         CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        ENDIF
        ia = ILNBLNK(units)
        IF ( ia .GT. 0 ) THEN
         err = NF_PUT_ATT_TEXT(fid, vid, 'units', ia, units)
         write(msgbuf,'(5a)')
     &     'adding attribute ''units'' to coordinate variable ''',
     &     cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
         CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        ENDIF
        ia = ILNBLNK(positive)
        IF ( ia .GT. 0 ) THEN
         err = NF_PUT_ATT_TEXT(fid, vid, 'positive', ia, positive)
         write(msgbuf,'(5a)')
     &     'adding attribute ''positive'' to coordinate variable ''',
     &     cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
         CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        ENDIF
C
        CALL MNC_FILE_ENDDEF(fname, myThid)

        err = NF_PUT_VARA_DOUBLE(fid, vid,
     &       cv_start, cv_count, rtmp)

        write(msgbuf,'(5a)') 'writing coordinate variable ''',
     &       cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


