C $Header: /u/gcmpack/MITgcm/model/src/do_oceanic_phys.F,v 1.109 2011/08/08 18:10:15 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP_OPTIONS.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_OPTIONS.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

CBOP
C     !ROUTINE: DO_OCEANIC_PHYS
C     !INTERFACE:
      SUBROUTINE DO_OCEANIC_PHYS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_OCEANIC_PHYS
C     | o Controlling routine for oceanic physics and
C     |   parameterization
C     *==========================================================*
C     | o originally, part of S/R thermodynamics
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif
#if defined (ALLOW_BALANCE_FLUXES) && !(defined ALLOW_AUTODIFF_TAMC)
#include "FFIELDS.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "AUTODIFF_MYFIELDS.h"
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# include "SURFACE.h"
# include "EOS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GGL90
#  include "GGL90.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_EXF
#  include "ctrl.h"
#  include "EXF_FIELDS.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
# endif
# ifdef ALLOW_THSICE
#  include "THSICE_VARS.h"
# endif
# ifdef ALLOW_SALT_PLUME
#  include "SALT_PLUME.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables
C     rhoK, rhoKm1  :: Density at current level, and level above
C     iMin, iMax    :: Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj        :: tile indices
C     i,j,k         :: loop indices
      _RL rhoKp1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER doDiagsRho
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

CEOP

#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy statement to end declaration part
      itdkey = 1
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DO_OCEANIC_PHYS',myThid)
#endif

      doDiagsRho = 0
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. fluidIsWater ) THEN
        IF ( DIAGNOSTICS_IS_ON('WRHOMASS',myThid) )
     &       doDiagsRho = doDiagsRho + 1
        IF ( DIAGNOSTICS_IS_ON('DRHODR  ',myThid) )
     &       doDiagsRho = doDiagsRho + 2
        IF ( DIAGNOSTICS_IS_ON('MXLDEPTH',myThid) )
     &       doDiagsRho = doDiagsRho + 4
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef  ALLOW_OBCS
      IF (useOBCS) THEN
C--   Calculate future values on open boundaries
C--   moved before SEAICE_MODEL call since SEAICE_MODEL needs seaice-obcs fields
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('OBCS_CALC',myThid)
# endif
       CALL OBCS_CALC( myTime+deltaTclock, myIter+1,
     I                 uVel, vVel, wVel, theta, salt, myThid )
      ENDIF
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_ADDFLUID
      IF ( fluidIsWater ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             addMass(i,j,k,bi,bj) =  0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_ADDFLUID */

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_SALT_PLUME
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          saltPlumeDepth(i,j,bi,bj) = 0. _d 0
          saltPlumeFlux(i,j,bi,bj)  = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_SEAICE
      IF ( useSEAICE ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
cph-adj-test(
CADJ STORE area   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hsnow  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE heff   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE empmr,qsw,theta   = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
cph-adj-test)
CADJ STORE atemp,aqh,precip    = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE swdown,lwdown       = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
cph# ifdef EXF_READ_EVAP
CADJ STORE evap                = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
cph# endif
CADJ STORE uvel,vvel           = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  ifdef SEAICE_CGRID
CADJ STORE stressdivergencex   = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE stressdivergencey   = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  endif
#  ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice                = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE vice                = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#   ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1       = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE seaice_sigma2       = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE seaice_sigma12      = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#   endif
#  endif
cph#  ifdef SEAICE_SALINITY
CADJ STORE salt                = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
cph#  endif
#  ifdef ATMOSPHERIC_LOADING
CADJ STORE pload               = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE siceload            = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  endif
#  ifdef NONLIN_FRSURF
CADJ STORE recip_hfacc         = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  endif
#  ifdef ANNUAL_BALANCE
CADJ STORE balance_itcount     = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  endif /* ANNUAL_BALANCE */
# endif
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SEAICE_MODEL',myThid)
# endif
        CALL TIMER_START('SEAICE_MODEL    [DO_OCEANIC_PHYS]', myThid)
        CALL SEAICE_MODEL( myTime, myIter, myThid )
        CALL TIMER_STOP ('SEAICE_MODEL    [DO_OCEANIC_PHYS]', myThid)
# ifdef ALLOW_COST
        CALL SEAICE_COST_SENSI ( myTime, myIter, myThid )
# endif
      ENDIF
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sst, sss           = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE qsw                = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# ifdef ALLOW_SEAICE
CADJ STORE area               = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif
#endif

#if (defined ALLOW_THSICE) && !(defined ALLOW_ATM2D)
      IF ( useThSIce .AND. fluidIsWater ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
cph(
#  ifdef NONLIN_FRSURF
CADJ STORE uice,vice        = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE salt,theta       = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE qnet,qsw, empmr  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE hFac_surfC       = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  endif
# endif
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('THSICE_MAIN',myThid)
# endif
C--     Step forward Therm.Sea-Ice variables
C       and modify forcing terms including effects from ice
        CALL TIMER_START('THSICE_MAIN     [DO_OCEANIC_PHYS]', myThid)
        CALL THSICE_MAIN( myTime, myIter, myThid )
        CALL TIMER_STOP( 'THSICE_MAIN     [DO_OCEANIC_PHYS]', myThid)
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_SHELFICE
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE salt, theta = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif
      IF ( useShelfIce .AND. fluidIsWater ) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('SHELFICE_THERMODYNAMICS',myThid)
#endif
C     compute temperature and (virtual) salt flux at the
C     shelf-ice ocean interface
       CALL TIMER_START('SHELFICE_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &       myThid)
       CALL SHELFICE_THERMODYNAMICS( myTime, myIter, myThid )
       CALL TIMER_STOP( 'SHELFICE_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &      myThid)
      ENDIF
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_ICEFRONT
      IF ( useICEFRONT .AND. fluidIsWater ) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('ICEFRONT_THERMODYNAMICS',myThid)
#endif
C     compute temperature and (virtual) salt flux at the
C     ice-front ocean interface
       CALL TIMER_START('ICEFRONT_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &       myThid)
       CALL ICEFRONT_THERMODYNAMICS( myTime, myIter, myThid )
       CALL TIMER_STOP( 'ICEFRONT_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &      myThid)
      ENDIF
#endif /* ALLOW_ICEFRONT */

C--   Freeze water in the ocean interior and let it rise to the surface
C     temporarily exclude from adjoint computations until
C     impact has been vetted in forward integrations
      IF ( allowInteriorFreezing ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta, salt = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE recip_hfacc = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif
        CALL FREEZE_INTERIOR(  myTime, myIter, myThid )
      ENDIF

C--   Freeze water at the surface
      IF ( allowFreezing ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif
        CALL FREEZE_SURFACE(  myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_OCN_COMPON_INTERF
C--    Apply imported data (from coupled interface) to forcing fields
C jmc: do not know precisely where to put this call (bf or af thSIce ?)
      IF ( useCoupler ) THEN
         CALL OCN_APPLY_IMPORT( .TRUE., myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_OCN_COMPON_INTERF */

#ifdef ALLOW_BALANCE_FLUXES
C     balance fluxes
      IF ( balanceEmPmR )
     &      CALL REMOVE_MEAN_RS( 1, EmPmR, maskInC, maskInC, rA, drF,
     &        'EmPmR', myTime, myThid )
      IF ( balanceQnet )
     &      CALL REMOVE_MEAN_RS( 1, Qnet,  maskInC, maskInC, rA, drF,
     &        'Qnet ', myTime, myThid )
#endif /* ALLOW_BALANCE_FLUXES */

#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#else  /* ALLOW_AUTODIFF_TAMC */
C     if fluid is not water, by-pass find_rho, gmredi, surfaceForcing
C     and all vertical mixing schemes, but keep OBCS_CALC
      IF ( fluidIsWater ) THEN
#endif /* ALLOW_AUTODIFF_TAMC */
      DO bj=myByLo(myThid),myByHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          itdkey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

#ifdef ALLOW_AUTODIFF_TAMC
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rhoKm1 (i,j)   = 0. _d 0
          rhoKp1 (i,j)   = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently used by GMRedi, IVDC, MXL-depth  and Diagnostics
           sigmaX(i,j,k) = 0. _d 0
           sigmaY(i,j,k) = 0. _d 0
           sigmaR(i,j,k) = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
cph all the following init. are necessary for TAF
cph although some of these are re-initialised later.
           rhoInSitu(i,j,k,bi,bj) = 0.
           IVDConvCount(i,j,k,bi,bj) = 0.
# ifdef ALLOW_GMREDI
           Kwx(i,j,k,bi,bj)  = 0. _d 0
           Kwy(i,j,k,bi,bj)  = 0. _d 0
           Kwz(i,j,k,bi,bj)  = 0. _d 0
#  ifdef GM_NON_UNITY_DIAGONAL
           Kux(i,j,k,bi,bj)  = 0. _d 0
           Kvy(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_EXTRA_DIAGONAL
           Kuz(i,j,k,bi,bj)  = 0. _d 0
           Kvz(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_BOLUS_ADVEC
           GM_PsiX(i,j,k,bi,bj)  = 0. _d 0
           GM_PsiY(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_VISBECK_VARIABLE_K
           VisbeckK(i,j,bi,bj)   = 0. _d 0
#  endif
# endif /* ALLOW_GMREDI */
# ifdef ALLOW_KPP
           KPPdiffKzS(i,j,k,bi,bj)  = 0. _d 0
           KPPdiffKzT(i,j,k,bi,bj)  = 0. _d 0
# endif /* ALLOW_KPP */
# ifdef ALLOW_GGL90
           GGL90viscArU(i,j,k,bi,bj)  = 0. _d 0
           GGL90viscArV(i,j,k,bi,bj)  = 0. _d 0
           GGL90diffKr(i,j,k,bi,bj)  = 0. _d 0
# endif /* ALLOW_GGL90 */
#endif /* ALLOW_AUTODIFF_TAMC */
          ENDDO
         ENDDO
        ENDDO

        iMin = 1-OLx
        iMax = sNx+OLx
        jMin = 1-OLy
        jMax = sNy+OLy

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE totphihyd(:,:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
# ifdef ALLOW_KPP
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_MSG('ENTERING UPWARD K LOOP',myThid)
#endif

C--     Start of diagnostic loop
        DO k=Nr,1,-1

#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick, is this formula correct now that we change the loop range?
C? Do we still need this?
cph kkey formula corrected.
cph Needed for rhoK, rhoKm1, in the case useGMREDI.
          kkey = (itdkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Always compute density (stored in common block) here; even when it is not
C     needed here, will be used anyway in calc_phi_hyd (data flow easier this way)
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('FIND_RHO_2D',myThid)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
          IF ( fluidIsWater ) THEN
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
CADJ STORE salt(:,:,k,bi,bj)  = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_DOWN_SLOPE
           IF ( useDOWN_SLOPE ) THEN
            CALL DWNSLP_CALC_RHO(
     I                  theta, salt,
     O                  rhoInSitu(1-OLx,1-OLy,k,bi,bj),
     I                  k, bi, bj, myTime, myIter, myThid )
           ENDIF
#endif /* ALLOW_DOWN_SLOPE */
#ifdef ALLOW_BBL
           IF ( useBBL ) THEN
C     pkg/bbl requires in-situ bbl density for depths equal to and deeper than the bbl.
C     To reduce computation and storage requirement, these densities are stored in the
C     dry grid boxes of rhoInSitu.  See BBL_CALC_RHO for details.
            CALL BBL_CALC_RHO(
     I                  theta, salt,
     O                  rhoInSitu,
     I                  k, bi, bj, myTime, myIter, myThid )

           ENDIF
#endif /* ALLOW_BBL */
           IF ( .NOT. ( useDOWN_SLOPE .OR. useBBL ) ) THEN
            CALL FIND_RHO_2D(
     I                iMin, iMax, jMin, jMax, k,
     I                theta(1-OLx,1-OLy,k,bi,bj),
     I                salt (1-OLx,1-OLy,k,bi,bj),
     O                rhoInSitu(1-OLx,1-OLy,k,bi,bj),
     I                k, bi, bj, myThid )
           ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
          ELSE
C-        fluid is not water:
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              rhoInSitu(i,j,k,bi,bj) = 0.
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

C--       Calculate gradients of potential density for isoneutral
C         slope terms (e.g. GM/Redi tensor or IVDC diffusivity)
          IF ( useGMRedi .OR. (k.GT.1 .AND. ivdc_kappa.NE.0.)
     &         .OR. useSALT_PLUME .OR. doDiagsRho.GE.1 ) THEN
            IF (k.GT.1) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k-1,bi,bj) = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
CADJ STORE rhokm1 (bi,bj)       = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
             CALL FIND_RHO_2D(
     I                 iMin, iMax, jMin, jMax, k,
     I                 theta(1-OLx,1-OLy,k-1,bi,bj),
     I                 salt (1-OLx,1-OLy,k-1,bi,bj),
     O                 rhoKm1,
     I                 k-1, bi, bj, myThid )
            ENDIF
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('GRAD_SIGMA',myThid)
#endif
cph Avoid variable aliasing for adjoint !!!
            DO j=jMin,jMax
             DO i=iMin,iMax
              rhoKp1(i,j) = rhoInSitu(i,j,k,bi,bj)
             ENDDO
            ENDDO
            CALL GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             rhoInSitu(1-OLx,1-OLy,k,bi,bj), rhoKm1, rhoKp1,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )
#ifdef ALLOW_AUTODIFF_TAMC
#ifdef GMREDI_WITH_STABLE_ADJOINT
cgf zero out adjoint fields to stabilize pkg/gmredi adjoint
cgf -> cuts adjoint dependency from slope to state
            CALL ZERO_ADJ_LOC( Nr, sigmaX, myThid)
            CALL ZERO_ADJ_LOC( Nr, sigmaY, myThid)
            CALL ZERO_ADJ_LOC( Nr, sigmaR, myThid)
#endif
#endif /* ALLOW_AUTODIFF_TAMC */
          ENDIF

C--       Implicit Vertical Diffusion for Convection
c ==> should use sigmaR !!!
          IF (k.GT.1 .AND. ivdc_kappa.NE.0.) THEN
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('CALC_IVDC',myThid)
#endif
            CALL CALC_IVDC(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        rhoKm1, rhoInSitu(1-OLx,1-OLy,k,bi,bj),
     I        myTime, myIter, myThid)
          ENDIF

#ifdef ALLOW_DIAGNOSTICS
          IF ( MOD(doDiagsRho,2).EQ.1 ) THEN
            CALL DIAGS_RHO_L( k, bi, bj,
     I                        rhoInSitu(1-OLx,1-OLy,k,bi,bj),
     I                        rhoKm1, wVel,
     I                        myTime, myIter, myThid )
          ENDIF
#endif

C--     end of diagnostic k loop (Nr:1)
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE IVDConvCount(:,:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
#endif

C--     Diagnose Mixed Layer Depth:
        IF ( useGMRedi .OR. doDiagsRho.GE.4 ) THEN
          CALL CALC_OCE_MXLAYER(
     I              rhoInSitu(1-OLx,1-OLy,1,bi,bj), sigmaR,
     I              bi, bj, myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_SALT_PLUME
        IF ( useSALT_PLUME ) THEN
          CALL SALT_PLUME_CALC_DEPTH(
     I              rhoInSitu(1-OLx,1-OLy,1,bi,bj), sigmaR,
     I              bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_DIAGNOSTICS
        IF ( MOD(doDiagsRho,4).GE.2 ) THEN
          CALL DIAGNOSTICS_FILL (sigmaR, 'DRHODR  ', 0, Nr,
     &         2, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--     Determines forcing terms based on external fields
C       relaxation terms, etc.
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EXTERNAL_FORCING_SURF',myThid)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE EmPmR(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
# ifdef EXACT_CONSERV
CADJ STORE PmEpR(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
# endif
# ifdef NONLIN_FRSURF
CADJ STORE hFac_surfC(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE recip_hFacC(:,:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
#  if (defined (ALLOW_PTRACERS))
CADJ STORE surfaceForcingS(:,:,bi,bj)   = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE surfaceForcingT(:,:,bi,bj)   = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
#  endif /* ALLOW_PTRACERS */
# endif /* NONLIN_FRSURF */
#endif
        CALL EXTERNAL_FORCING_SURF(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef EXACT_CONSERV
cph-test
cphCADJ STORE PmEpR(:,:,bi,bj)
cphCADJ &     = comlev1_bibj, key=itdkey,
cphCADJ &     kind = isbyte
# endif
#endif

#ifdef ALLOW_AUTODIFF_TAMC
cph needed for KPP
CADJ STORE surfaceForcingU(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE surfaceForcingV(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE surfaceForcingS(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE surfaceForcingT(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE surfaceForcingTice(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  ALLOW_KPP
C--     Compute KPP mixing coefficients
        IF (useKPP) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('KPP_CALC',myThid)
#endif
          CALL TIMER_START('KPP_CALC [DO_OCEANIC_PHYS]', myThid)
          CALL KPP_CALC(
     I                  bi, bj, myTime, myIter, myThid )
          CALL TIMER_STOP ('KPP_CALC [DO_OCEANIC_PHYS]', myThid)
#ifdef ALLOW_AUTODIFF_TAMC
        ELSE
          CALL KPP_CALC_DUMMY(
     I                  bi, bj, myTime, myIter, myThid )
#endif /* ALLOW_AUTODIFF_TAMC */
        ENDIF

#endif  /* ALLOW_KPP */

#ifdef  ALLOW_PP81
C--     Compute PP81 mixing coefficients
        IF (usePP81) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('PP81_CALC',myThid)
#endif
          CALL PP81_CALC(
     I                  bi, bj, myTime, myThid )
        ENDIF
#endif /* ALLOW_PP81 */

#ifdef  ALLOW_MY82
C--     Compute MY82 mixing coefficients
        IF (useMY82) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('MY82_CALC',myThid)
#endif
          CALL MY82_CALC(
     I                  bi, bj, myTime, myThid )
        ENDIF
#endif /* ALLOW_MY82 */

#ifdef  ALLOW_GGL90
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90TKE (:,:,:,bi,bj) = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C--     Compute GGL90 mixing coefficients
        IF (useGGL90) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GGL90_CALC',myThid)
#endif
          CALL TIMER_START('GGL90_CALC [DO_OCEANIC_PHYS]', myThid)
          CALL GGL90_CALC(
     I                  bi, bj, myTime, myIter, myThid )
          CALL TIMER_STOP ('GGL90_CALC [DO_OCEANIC_PHYS]', myThid)
        ENDIF
#endif /* ALLOW_GGL90 */

#ifdef ALLOW_TIMEAVE
        IF ( taveFreq.GT. 0. _d 0 ) THEN
          CALL TIMEAVE_SURF_FLUX( bi, bj, myTime, myIter, myThid)
        ENDIF
        IF (taveFreq.GT.0. .AND. ivdc_kappa.NE.0.) THEN
          CALL TIMEAVE_CUMULATE(ConvectCountTave, IVDConvCount,
     I                           Nr, deltaTclock, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_GMREDI
#ifdef ALLOW_AUTODIFF_TAMC
# ifndef GM_EXCLUDE_CLIPPING
cph storing here is needed only for one GMREDI_OPTIONS:
cph define GM_BOLUS_ADVEC
cph keep it although TAF says you dont need to.
cph but I have avoided the #ifdef for now, in case more things change
CADJ STORE sigmaX(:,:,:)        = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE sigmaY(:,:,:)        = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
CADJ STORE sigmaR(:,:,:)        = comlev1_bibj, key=itdkey,
CADJ &     kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Calculate iso-neutral slopes for the GM/Redi parameterisation
        IF (useGMRedi) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GMREDI_CALC_TENSOR',myThid)
#endif
          CALL GMREDI_CALC_TENSOR(
     I             iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
        ELSE
          CALL GMREDI_CALC_TENSOR_DUMMY(
     I             iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )
#endif /* ALLOW_AUTODIFF_TAMC */
        ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_DOWN_SLOPE
        IF ( useDOWN_SLOPE ) THEN
C--     Calculate Downsloping Flow for Down_Slope parameterization
         IF ( usingPCoords ) THEN
          CALL DWNSLP_CALC_FLOW(
     I                bi, bj, kSurfC, rhoInSitu,
     I                myTime, myIter, myThid )
         ELSE
          CALL DWNSLP_CALC_FLOW(
     I                bi, bj, kLowC, rhoInSitu,
     I                myTime, myIter, myThid )
         ENDIF
        ENDIF
#endif /* ALLOW_DOWN_SLOPE */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifndef ALLOW_AUTODIFF_TAMC
C---  if fluid Is Water: end
      ENDIF
#endif

#ifdef ALLOW_BBL
      IF ( useBBL ) THEN
       CALL BBL_CALC_RHS(
     I                          myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_BBL */

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) THEN
       CALL MYPACKAGE_CALC_RHS(
     I                          myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#ifdef ALLOW_GMREDI
      IF ( useGMRedi ) THEN
        CALL GMREDI_DO_EXCH( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_KPP
      IF (useKPP) THEN
        CALL KPP_DO_EXCH( myThid )
      ENDIF
#endif /* ALLOW_KPP */

#ifdef ALLOW_DIAGNOSTICS
      IF ( fluidIsWater .AND. useDiagnostics ) THEN
        CALL DIAGS_RHO_G(
     I                    rhoInSitu, uVel, vVel,
     I                    myTime, myIter, myThid )
        CALL DIAGS_OCEANIC_SURF_FLUX( myTime, myIter, myThid )
      ENDIF
      IF ( ivdc_kappa.NE.0 .AND. useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( IVDConvCount, 'CONVADJ ',
     &                               0, Nr, 0, 1, 1, myThid )
      ENDIF
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DO_OCEANIC_PHYS',myThid)
#endif

      RETURN
      END
