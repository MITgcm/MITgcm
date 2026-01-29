C $Header: /u/gcmpack/MITgcm_contrib/llc_hires/llc_4320/code/CPP_OPTIONS.h,v 1.3 2016/04/14 03:51:44 dimitri Exp $
C $Name:  $



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

C o Shortwave heating as extra term in external_forcing.F
C Note: this should be a run-time option


C o Include/exclude Geothermal Heat Flux at the bottom of the ocean


C o Include/exclude phi_hyd calculation code


C o Include/exclude call to S/R CONVECT


C o Include/exclude call to S/R CALC_DIFFUSIVITY


C o Allow full 3D specification of vertical diffusivity


C o Allow latitudinally varying BryanLewis79 vertical diffusivity


C o Include/exclude Implicit vertical advection code


C o Include/exclude AdamsBashforth-3rd-Order code


C o Include/exclude nonHydrostatic code


C o Allow to account for heating due to friction (and momentum dissipation)


C o Allow mass source or sink of Fluid in the interior
C   (3-D generalisation of oceanic real-fresh water flux)


C o Include pressure loading code


C o exclude/allow external forcing-fields load
C   this allows to read & do simple linear time interpolation of oceanic
C   forcing fields, if no specific pkg (e.g., EXF) is used to compute them.


C o Include/exclude balancing surface forcing fluxes code


C o Include/exclude balancing surface forcing relaxation code


C o Include/exclude GM-like eddy stress in momentum code


C o Use "Exact Convervation" of fluid in Free-Surface formulation
C   so that d/dt(eta) is exactly equal to - Div.Transport


C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that surface thickness (hFactors) vary with time


C o Include/exclude code for single reduction Conjugate-Gradient solver


C o Choices for implicit solver routines solve_*diagonal.F
C   The following has low memory footprint, but not suitable for AD

C   The following one suitable for AD but does not vectorize


C o ALLOW isotropic scaling of harmonic and bi-harmonic terms when
C   using an locally isotropic spherical grid with (dlambda) x (dphi*cos(phi))
C *only for use on a lat-lon grid*
C   Setting this flag here affects both momentum and tracer equation unless
C   it is set/unset again in other header fields (e.g., GAD_OPTIONS.h).
C   The definition of the flag is commented to avoid interference with
C   such other header files.
C   The preferred method is specifying a value for viscAhGrid or viscA4Grid
C   in data which is then automatically scaled by the grid size;
C   the old method of specifying viscAh/viscA4 and this flag is provided
C   for completeness only (and for use with the adjoint).
C#define ISOTROPIC_COS_SCALING

C o This flag selects the form of COSINE(lat) scaling of bi-harmonic term.
C *only for use on a lat-lon grid*
C   Has no effect if ISOTROPIC_COS_SCALING is undefined.
C   Has no effect on vector invariant momentum equations.
C   Setting this flag here affects both momentum and tracer equation unless
C   it is set/unset again in other header fields (e.g., GAD_OPTIONS.h).
C   The definition of the flag is commented to avoid interference with
C   such other header files.
C#define COSINEMETH_III

C o Use "OLD" UV discretisation near boundaries (*not* recommended)
C   Note - only works with pkg/mom_fluxform and "no_slip_sides=.FALSE."
C          because the old code did not have no-slip BCs


C o Use LONG.bin, LATG.bin, etc., initialization for ini_curviliear_grid.F
C   Default is to use "new" grid files (OLD_GRID_IO undef) but OLD_GRID_IO
C   is still useful with, e.g., single-domain curvilinear configurations.


C o Use old EXTERNAL_FORCING_U,V,T,S subroutines (for backward compatibility)


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






C o Include/exclude single header file containing multiple packages options
C   (AUTODIFF, COST, CTRL, ECCO, EXF ...) instead of the standard way where
C   each of the above pkg get its own options from its specific option file.
C   Although this method, inherited from ECCO setup, has been traditionally
C   used for all adjoint built, work is in progress to allow to use the
C   standard method also for adjoint built.
c#ifdef PACKAGES_CONFIG_H
c# include "ECCO_CPPOPTIONS.h"
c#endif



C--  File gsw_teos10.F: routines that compute quantities related to seawater.
C--   Contents
C--   TEOS-10 routines (Gibbs seawater, GSW)
C--   o GSW_PT_FROM_CT: function to compute potential temperature 
C--              from conservative temperature and absolute salinity
C--   o GSW_CT_FROM_PT: function to compute conservative temperature with
C--              from potential temperature and absolute salinity
C--   o GSW_GIBBS_PT0_PT0: function to compute specific Gibbs free energy
C--              from potential temperature and absolute salinity

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GSW_PT_FROM_CT
C     !INTERFACE:
      Real*8 FUNCTION GSW_PT_FROM_CT(SA,CT)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  GSW_PT_FROM_CT
C     | o compute potential temperature at reference level 0 dbar
C     |   from conservative temperature (CT) and absolute
C     |    salinity (SA)
C     | o this is a more or less shameless copy of the teos-10 code
C     |   available at http://www.teos-10.org
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     SA :: Absolute Salinity                               (g/kg)
C     CT :: Conservative Temperature                        (deg C)
      Real*8 SA,CT

C     !FUNCTIONS:
C     == Functions ==
CML      Real*8 gsw_gibbs
      Real*8 gsw_gibbs_pt0_pt0
      Real*8 gsw_ct_from_pt
CML      EXTERNAL gsw_gibbs, gsw_gibbs_pt0_pt0, gsw_ct_from_pt
      EXTERNAL gsw_gibbs_pt0_pt0, gsw_ct_from_pt

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER n0, n2
      Real*8 s1, p0, cp0 
      Real*8 a0, a1, a2, a3, a4, a5, b0, b1, b2, b3
      Real*8 a5ct, b3ct, ct_factor, pt_num, pt_den, ct_diff
      Real*8 pt, pt_old, ptm, dct_dpt
CEOP

      cp0 = 3991.86795711963D0    

      n0 = 0
      n2 = 2

      s1 = SA * 35.D0 / 35.16504D0
      p0 = 0.D0

      a0 = -1.446013646344788D-2
      a1 = -3.305308995852924D-3
      a2 =  1.062415929128982D-4
      a3 =  9.477566673794488D-1
      a4 =  2.166591947736613D-3
      a5 =  3.828842955039902D-3
      
      b0 =  1.000000000000000D+0
      b1 =  6.506097115635800D-4
      b2 =  3.830289486850898D-3
      b3 =  1.247811760368034D-6

      a5ct = a5*CT
      b3ct = b3*CT

      ct_factor = (a3 + a4*s1 + a5ct)
      pt_num    = a0 + s1*(a1 + a2*s1) + CT*ct_factor
      pt_den    = b0 + b1*s1 + CT*(b2 + b3ct)
      pt        = (pt_num)/(pt_den)

      dct_dpt   = (pt_den)/(ct_factor + a5ct - (b2 + b3ct + b3ct)*pt)

C     start the 1.5 iterations through the modified Newton-Rapshon 
C     iterative method.  

      ct_diff = gsw_ct_from_pt(sa,pt) - CT
      pt_old  = pt
      pt      = pt_old - (ct_diff)/dct_dpt
      ptm     = 0.5D0*(pt + pt_old)

      dct_dpt = -(ptm + 273.15D0)*gsw_gibbs_pt0_pt0(sa,ptm)/cp0

      pt             = pt_old - (ct_diff)/dct_dpt
      ct_diff        = gsw_ct_from_pt(sa,pt) - CT
      pt_old         = pt
      GSW_PT_FROM_CT = pt_old - (ct_diff)/dct_dpt

      RETURN 
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GSW_CT_FROM_PT
C     !INTERFACE:
      Real*8 FUNCTION GSW_CT_FROM_PT(SA,PT)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  GSW_CT_FROM_PT
C     | o compute conservative temperature from potential 
C     |   temperature (PT)  at reference level 0 dbar and absolute
C     |   salinity (SA)
C     | o this is a more or less shameless copy of the teos-10 code
C     |   available at http://www.teos-10.org
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     SA :: Absolute Salinity                               (g/kg)
C     PT :: Potential Temperature                          (deg C)
      Real*8 sa, pt

C     !FUNCTIONS:
C     == Functions ==

C     !LOCAL VARIABLES:
C     == Local variables ==
      Real*8 pot_enthalpy, sfac
      Real*8 x2, x, y, cp0
CEOP

      sfac = 0.0248826675584615D0 

      x2   = sfac*sa
      x    = 0.D0
      if (x2.gt.0.D0) x = sqrt(x2)
C     normalize for F03 and F08
      y    = pt*0.025D0

      pot_enthalpy =  61.01362420681071D0 +
     &     y*(168776.46138048015D0 +
     &     y*(-2735.2785605119625D0 + y*(2574.2164453821433D0 + 
     &     y*(-1536.6644434977543D0 + y*(545.7340497931629D0 + 
     &     (-50.91091728474331D0 - 18.30489878927802D0*y)*y))))) + 
     &     x2*(268.5520265845071D0 + y*(-12019.028203559312D0 + 
     &     y *(3734.858026725145D0 + y*(-2046.7671145057618D0 + 
     &     y*(465.28655623826234D0 + (-0.6370820302376359D0 - 
     &     10.650848542359153D0*y)*y)))) + 
     &     x*(937.2099110620707D0 + y*(588.1802812170108D0 +
     &     y*(248.39476522971285D0 + (-3.871557904936333D0 -
     &     2.6268019854268356D0*y)*y)) + 
     &     x*(-1687.914374187449D0 + x*(246.9598888781377D0 + 
     &     x*(123.59576582457964D0 - 48.5891069025409D0*x)) + 
     &     y*( 936.3206544460336D0 + 
     &     y*(-942.7827304544439D0 + y*(369.4389437509002D0 + 
     &     (-33.83664947895248D0 - 9.987880382780322D0*y)*y))))))

      cp0 = 3991.86795711963D0

      gsw_ct_from_pt = pot_enthalpy/cp0

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GSW_GIBBS_PT0_PT0
C     !INTERFACE:
      Real*8 FUNCTION GSW_GIBBS_PT0_PT0(SA,PT0)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R GSW_GIBBS_PT0_PT0
C     | o helper routine that computes the specific Gibbs free
C     |   energy from potential temperature (PT) and absolute
C     |   salinity (SA) for pressure 0 dbar
C     | o this is a more or less shameless copy of the teos-10 code
C     |   available at http://www.teos-10.org
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     SA :: Absolute Salinity                               (g/kg)
C     PT :: Potential Temperature at p = 0 dbar             (deg C)
      Real*8 sa, pt0

C     !FUNCTIONS:
C     == Functions ==

C     !LOCAL VARIABLES:
C     == Local variables ==
      Real*8 sfac, x2, x, y, g03, g08
CEOP

      sfac = 0.0248826675584615

      x2   = sfac*sa
      x    = 0.D0
      if (x2.gt.0.D0) x = sqrt(x2)
      y    = pt0*0.025D0

      g03 = -24715.571866078 +
     &     y*(4420.4472249096725 +
     &     y*(-1778.231237203896 +
     &     y*(1160.5182516851419 +
     &     y*(-569.531539542516  + y*128.13429152494615))))

      g08 = x2*(1760.062705994408  + x*(-86.1329351956084 +
     &     x*( -137.1145018408982  + y*(296.20061691375236 +
     &     y* (-205.67709290374563 + 49.9394019139016*y))) + 
     &     y*(  -60.136422517125   + y*10.50720794170734)) +
     &     y*(-1351.605895580406   + y*(1097.1125373015109 +
     &     y*( -433.20648175062206 + 63.905091254154904*y))))

      gsw_gibbs_pt0_pt0 = (g03 + g08)*0.000625

      RETURN
      END

