

  








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
C     !ROUTINE: MNC_CW_INIT

C     !INTERFACE:
      SUBROUTINE MNC_CW_INIT( 
     I     sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr, 
     I     myThid )

C     !DESCRIPTION:
C     Create the pre-defined grid types and variable types.

C     The grid type is a character string that encodes the presence and
C     types associated with the four possible dimensions.  The character
C     string follows the format
C     \begin{center}
C       \texttt{H0\_H1\_H2\_\_V\_\_T}
C     \end{center}
C     where the terms \textit{H0}, \textit{H1}, \textit{H2}, \textit{V},
C     \textit{T} can be almost any combination of the following:
C     \begin{center}
C       \begin{tabular}[h]{|ccc|c|c|}\hline
C         \multicolumn{3}{|c|}{Horizontal} & Vertical & Time \C         \textit{H0}: location & \textit{H1}: dimensions & \textit{H2}: halo 

C               & \textit{V}: location & \textit{T}: level  \\\hline
C         \texttt{-} & xy & Hn & \texttt{-} & \texttt{-} \C         U  &  x  &  Hy  &  i  &  t  \C         V  &  y  &      &  c  &     \C         Cen  &   &      &     &     \C         Cor  &   &      &     &     \\\hline

C       \end{tabular}
C     \end{center}


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


C     !INPUT PARAMETERS:
      integer myThid
      integer sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr
CEOP

C     !LOCAL VARIABLES:
      integer CW_MAX_LOC
      parameter ( CW_MAX_LOC = 6 )
      integer i, ihorz,ihsub,ivert,itime,ihalo, is,ih, n,ntot
      integer ndim, ncomb, nvch
      character*(MNC_MAX_CHAR) name
      character*(MNC_MAX_CHAR) dn(CW_MAX_LOC)
      character*(5) horz_dat(CW_MAX_LOC), hsub_dat(CW_MAX_LOC),
     &     vert_dat(CW_MAX_LOC), time_dat(CW_MAX_LOC), 
     &     halo_dat(CW_MAX_LOC)
      integer dim(CW_MAX_LOC), ib(CW_MAX_LOC), ie(CW_MAX_LOC)

C     Functions
      integer ILNBLNK
      external ILNBLNK

C     ......12345....12345....12345....12345....12345...
      data horz_dat /
     &     '-    ', 'U    ', 'V    ', 'Cen  ', 'Cor  ', '     '  /
      data hsub_dat /
     &     'xy   ', 'x    ', 'y    ', '-    ', '     ', '     '  /
      data halo_dat /
     &     'Hn   ', 'Hy   ', '--   ', '     ', '     ', '     '  /
      data vert_dat /
     &     '-    ', 'C    ', 'I    ', 'L    ', 'U    ', 'S    '  /
      data time_dat /
     &     '-    ', 't    ', '     ', '     ', '     ', '     '  /

C     Create the types
      ncomb = 0
      DO ihorz = 1,5
        DO is = 1,3
          DO ih = 1,2
            
C           Loop just ONCE if the Horiz component is "-"
            ihsub = is
            ihalo = ih
            IF (ihorz .EQ. 1) THEN
              IF ((is .EQ. 1) .AND. (ih .EQ. 1)) THEN
                ihsub = 4
                ihalo = 3
              ELSE
                GOTO 10
              ENDIF
            ENDIF
            
            DO ivert = 1,6
              DO itime = 1,2
                
C               horiz and hsub
                name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
                n = ILNBLNK(horz_dat(ihorz))
                name(1:n) = horz_dat(ihorz)(1:n)
                ntot = n + 1              
                name(ntot:ntot) = '_'
                n = ILNBLNK(hsub_dat(ihsub))
                name((ntot+1):(ntot+n)) = hsub_dat(ihsub)(1:n)
                ntot = ntot + n

C               halo, vert, and time
                write(name((ntot+1):(ntot+5)), '(a1,2a2)')
     &               '_', halo_dat(ihalo)(1:2), '__'
                nvch = ILNBLNK(vert_dat(ivert))
                n = ntot+6+nvch-1
                name((ntot+6):(n)) = vert_dat(ivert)(1:nvch)
                write(name((n+1):(n+3)), '(a2,a1)') 
     &               '__', time_dat(itime)(1:1)

                ndim = 0
                DO i = 1,CW_MAX_LOC
                  dn(i)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
                  dim(i) = 0
                  ib(i) = 0
                  ie(i) = 0
                ENDDO

C               Horizontal dimensions
                IF (halo_dat(ihalo)(1:5) .EQ. 'Hn   ') THEN

                  IF (hsub_dat(ihsub)(1:1) .EQ. 'x') THEN
                    ndim = ndim + 1
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cen')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'V') ) THEN
                      dn(ndim)(1:1) = 'X'
                      dim(ndim) = sNx + 2*OLx
                      ib(ndim)  = OLx + 1
                      ie(ndim)  = OLx + sNx
                    ENDIF
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cor')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'U') ) THEN
                      dn(ndim)(1:3) = 'Xp1'
                      dim(ndim) = sNx + 2*OLx
                      ib(ndim)  = OLx + 1
                      ie(ndim)  = OLx + sNx + 1
                    ENDIF
                  ENDIF
                  IF ((hsub_dat(ihsub)(1:1) .EQ. 'y')
     &                 .OR. (hsub_dat(ihsub)(2:2) .EQ. 'y')) THEN
                    ndim = ndim + 1
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cen')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'U') ) THEN
                      dn(ndim)(1:1) = 'Y'
                      dim(ndim) = sNy + 2*OLy
                      ib(ndim)  = OLy + 1
                      ie(ndim)  = OLy + sNy
                    ENDIF
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cor')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'V') ) THEN
                      dn(ndim)(1:3) = 'Yp1'
                      dim(ndim) = sNy + 2*OLy
                      ib(ndim)  = OLy + 1
                      ie(ndim)  = OLy + sNy + 1
                    ENDIF
                  ENDIF

                ELSEIF (halo_dat(ihalo)(1:5) .EQ. 'Hy   ') THEN

                  IF (hsub_dat(ihsub)(1:1) .EQ. 'x') THEN
                    ndim = ndim + 1
                    dn(ndim)(1:3) = 'Xwh'
                    dim(ndim) = sNx + 2*OLx
                    ib(ndim)  = 1
                    ie(ndim)  = sNx + 2*OLx
                  ENDIF
                  IF ((hsub_dat(ihsub)(1:1) .EQ. 'y')
     &                 .OR. (hsub_dat(ihsub)(2:2) .EQ. 'y')) THEN
                    ndim = ndim + 1
                    dn(ndim)(1:3) = 'Ywh'
                    dim(ndim) = sNy + 2*OLy
                    ib(ndim)  = 1
                    ie(ndim)  = sNy + 2*OLy
                  ENDIF

                ENDIF

C               Vertical dimension
                IF (vert_dat(ivert)(1:1) .EQ. 'C') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'Z'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'I') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:3) = 'Zp1'
                  dim(ndim) = Nr + 1
                  ib(ndim)  = 1
                  ie(ndim)  = Nr + 1
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'L') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:2) = 'Zl'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'U') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:2) = 'Zu'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'M') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:3) = 'Zm1'
                  dim(ndim) = Nr - 1
                  ib(ndim)  = 1
                  ie(ndim)  = Nr - 1
                ENDIF

C               Time dimension
                IF (time_dat(itime)(1:1) .EQ. 't') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'T'
                  dim(ndim) = -1
                  ib(ndim)  = 1
                  ie(ndim)  = 1
                ENDIF

                IF (ndim .GT. 0) THEN


                  CALL MNC_CW_ADD_GNAME(name, ndim, 
     &                 dim, dn, ib, ie, myThid)
                ENDIF

              ENDDO
            ENDDO

 10         CONTINUE
          ENDDO
        ENDDO
      ENDDO
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

