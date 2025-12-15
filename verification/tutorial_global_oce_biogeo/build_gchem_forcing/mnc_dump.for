

  








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
CBOP 0
C     !ROUTINE: MNC_DUMP

C     !INTERFACE:
      SUBROUTINE MNC_DUMP( myThid )

C     !DESCRIPTION:
C     Create a short (usually incomplete) summary of the current state
C     of the MNC lookup tables.  This can be helpful for debugging MNC.
      
C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP
      CALL MNC_DUMP_ALL(5, 20, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_DUMP_ALL

C     !INTERFACE:
      SUBROUTINE MNC_DUMP_ALL( n_id, n_items, myThid )

C     !DESCRIPTION:
C     Create a summary of a specified portion of the current state of
C     the MNC lookup tables.  This can be helpful for debugging MNC.

C     !USES:

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


C     !INPUT PARAMETERS:
      integer n_id, n_items, myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j

      write(*,*) 'mnc_f_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_f_names(i)(1:40)
      ENDDO
      write(*,*) 'mnc_g_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_g_names(i)(1:40)
      ENDDO
      write(*,*) 'mnc_v_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_v_names(i)(1:40)
      ENDDO
      write(*,*) 'mnc_d_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_d_names(i)(1:40)
      ENDDO

      write(*,*) 'mnc_d_ids, mnc_d_size :'
      DO i = 1,n_id
        write(*,*) i,': ',mnc_d_ids(i), mnc_d_size(i)
      ENDDO

      write(*,*) 'mnc_f_info :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_f_info(i,j), j=1,n_items)
      ENDDO
      write(*,*) 'mnc_fd_ind :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_fd_ind(i,j), j=1,n_items)
      ENDDO
      write(*,*) 'mnc_fv_ids :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_fv_ids(i,j), j=1,n_items)
      ENDDO
      write(*,*) 'mnc_f_alld :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_f_alld(i,j), j=1,n_items)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

