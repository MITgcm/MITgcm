#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C--  File apply_forcing.F:
C--   Contents
C--   o APPLY_FORCING_U
C--   o APPLY_FORCING_V
C--   o APPLY_FORCING_T
C--   o APPLY_FORCING_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_U
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_U(
     U                     gU_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_U
C     | o Contains problem specific forcing for zonal velocity.
C     *==========================================================*
C     | Adds terms to gU for forcing by external sources
C     | e.g. wind stress, bottom friction etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gU_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
C     kSurface  :: index of surface level
      INTEGER i, j
#ifdef USE_OLD_EXTERNAL_FORCING
      _RL     locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpVar
#else
      INTEGER kSurface
#endif
CEOP

#ifdef USE_OLD_EXTERNAL_FORCING

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          locVar(i,j) = gU(i,j,k,bi,bj)
        ENDDO
      ENDDO
      CALL EXTERNAL_FORCING_U(
     I              iMin, iMax, jMin, jMax, bi, bj, k,
     I              myTime, myThid )
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          tmpVar = gU(i,j,k,bi,bj) - locVar(i,j)
          gU(i,j,k,bi,bj) = locVar(i,j)
          gU_arr(i,j) = gU_arr(i,j) + tmpVar
        ENDDO
      ENDDO

#else  /* USE_OLD_EXTERNAL_FORCING */

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_U(
     U                       gU_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_ATM_PHYS
      IF ( useAtm_Phys ) CALL ATM_PHYS_TENDENCY_APPLY_U(
     U                       gU_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_U(
     U                       gU_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_FIZHI */

C     Ocean: Add momentum surface forcing (e.g., wind-stress) in surface level
      IF ( k .EQ. kSurface ) THEN
c      DO j=1,sNy
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNy+1]
       DO j=0,sNy+1
        DO i=1,sNx+1
          gU_arr(i,j) = gU_arr(i,j)
     &      +foFacMom*surfaceForcingU(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( kSurface.EQ.-1 ) THEN
       DO j=0,sNy+1
        DO i=1,sNx+1
         IF ( kSurfW(i,j,bi,bj).EQ.k ) THEN
          gU_arr(i,j) = gU_arr(i,j)
     &      +foFacMom*surfaceForcingU(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_EDDYPSI
         CALL TAUEDDY_TENDENCY_APPLY_U(
     U                 gU_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
#endif

#ifdef ALLOW_RBCS
      IF (useRBCS) THEN
        CALL RBCS_ADD_TENDENCY(
     U                 gU_arr,
     I                 k, bi, bj, -1,
     I                 myTime, 0, myThid )

      ENDIF
#endif /* ALLOW_RBCS */

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
        CALL OBCS_SPONGE_U(
     U                   gU_arr,
     I                   iMin,iMax,jMin,jMax, k, bi,bj,
     I                   myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) THEN
        CALL MYPACKAGE_TENDENCY_APPLY_U(
     U                 gU_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#endif /* USE_OLD_EXTERNAL_FORCING */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_V
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_V(
     U                     gV_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_V
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gV for forcing by external sources
C     | e.g. wind stress, bottom friction etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gV_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
C     kSurface  :: index of surface level
      INTEGER i, j
#ifdef USE_OLD_EXTERNAL_FORCING
      _RL     locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpVar
#else
      INTEGER kSurface
#endif
CEOP

#ifdef USE_OLD_EXTERNAL_FORCING

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          locVar(i,j) = gV(i,j,k,bi,bj)
        ENDDO
      ENDDO
      CALL EXTERNAL_FORCING_V(
     I              iMin, iMax, jMin, jMax, bi, bj, k,
     I              myTime, myThid )
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          tmpVar = gV(i,j,k,bi,bj) - locVar(i,j)
          gV(i,j,k,bi,bj) = locVar(i,j)
          gV_arr(i,j) = gV_arr(i,j) + tmpVar
        ENDDO
      ENDDO

#else  /* USE_OLD_EXTERNAL_FORCING */

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_V(
     U                       gV_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_ATM_PHYS
      IF ( useAtm_Phys ) CALL ATM_PHYS_TENDENCY_APPLY_V(
     U                       gV_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_V(
     U                       gV_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_FIZHI */

C     Ocean: Add momentum surface forcing (e.g., wind-stress) in surface level
      IF ( k .EQ. kSurface ) THEN
       DO j=1,sNy+1
c       DO i=1,sNx
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNx+1]
        DO i=0,sNx+1
          gV_arr(i,j) = gV_arr(i,j)
     &      +foFacMom*surfaceForcingV(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( kSurface.EQ.-1 ) THEN
       DO j=1,sNy+1
        DO i=0,sNx+1
         IF ( kSurfS(i,j,bi,bj).EQ.k ) THEN
          gV_arr(i,j) = gV_arr(i,j)
     &      +foFacMom*surfaceForcingV(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_EDDYPSI
         CALL TAUEDDY_TENDENCY_APPLY_V(
     U                 gV_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
#endif

#ifdef ALLOW_RBCS
      IF (useRBCS) THEN
        CALL RBCS_ADD_TENDENCY(
     U                 gV_arr,
     I                 k, bi, bj, -2,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_RBCS */

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
        CALL OBCS_SPONGE_V(
     U                   gV_arr,
     I                   iMin,iMax,jMin,jMax, k, bi,bj,
     I                   myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) THEN
        CALL MYPACKAGE_TENDENCY_APPLY_V(
     U                 gV_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#endif /* USE_OLD_EXTERNAL_FORCING */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_T
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_T(
     U                     gT_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_T
C     | o Contains problem specific forcing for temperature.
C     *==========================================================*
C     | Adds terms to gT for forcing by external sources
C     | e.g. heat flux, climatalogical relaxation, etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
C     kSurface  :: index of surface level
      INTEGER i, j
#ifdef USE_OLD_EXTERNAL_FORCING
      _RL     locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpVar
#else
      INTEGER kSurface
      INTEGER km, kc, kp
      _RL tmpVar(1:sNx,1:sNy)
      _RL tmpFac, delPI
      _RL recip_Cp
CEOP
#ifdef SHORTWAVE_HEATING
      _RL minusone
      PARAMETER (minusOne=-1.)
      _RL swfracb(2)
      INTEGER kp1
#endif
C---- Experiment specific declaration: starts here -------------------
C     iG, jG             :: Global index temps.
C     hC, hW, hE, hN, hS :: Fractional vertical distance open to fluid temps.
C     dFlux[WENS]        :: Diffusive flux normal to each cell face.
C     faceArea           :: Temp. for holding area normal to tempurature gradient.
      INTEGER iG, jG
      _RL hC, hW, hE, hN, hS
      _RL dFluxW, dFluxE, dFluxN, dFluxS
      _RL faceArea

C--   Forcing term
C     Add term which represents the diffusive flux from a circular cylinder of temperature tCylIm in the
C     interior of the simulation domain. Result is a tendency which is determined from the finite
C     volume formulated divergence of the diffusive heat flux due to the local cylinder
C     temperature, fluid temperature difference.
C     kDiffCyl :: Diffusion coefficient
C     tCylIn     :: Temperature of the inner boundary of the cylinder
C     tCylOut     :: Temperature of the outer boundary cylinder
C     iGSource :: Index space I (global) coordinate for source center.
C     jGSource :: Index space J (global) coordinate for source center.
C     rSource  :: Extent of the source term region. Loop will skip checking points outside
C              :: this region. Within this region the source heating will be added
C              :: to any points that are at a land - fluid boundary. rSource is in grid
C              :: points, so that points checked are ophi(iGlobal,jGlobal) such that
C              :: iGlobal=iGsource +/- rSource, jGlobal = jGsource +/- rSource.
C              :: rSource, iGSource and jGSource only need to define a quadrilateral that
C              :: includes the cylinder and no other land, they do not need to be exact.
C afe:
C  the below appears to be an attempt to make the heat flux somewhat general in regards
C  to bathymetry, but jmc pointed out some ways it could be better.  this is not
C  an issue at this point (July 04) since all experiments are being done with straight-
C  sided tank and cyclinder walls.
C  some todo items:
C  * add terms to top and bottom -- probably critical!!!
C  * make tCyl more flexible -- maybe have as separate inner and outer variables
C       or, eventually, a forcing field
C  * think about possible problems with differing heat diffusion rates in wall materials
C    (plexiglass, air, water in the compound tank case)
      _RL kDiffCyl
      _RL tCyl
      INTEGER rSource
      INTEGER iGSource
      INTEGER jGSource
C---- Experiment specific declaration:  ends  here -------------------
#endif

#ifdef USE_OLD_EXTERNAL_FORCING

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          locVar(i,j) = gT(i,j,k,bi,bj)
        ENDDO
      ENDDO
      CALL EXTERNAL_FORCING_T(
     I              iMin, iMax, jMin, jMax, bi, bj, k,
     I              myTime, myThid )
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          tmpVar = gT(i,j,k,bi,bj) - locVar(i,j)
          gT(i,j,k,bi,bj) = locVar(i,j)
          gT_arr(i,j) = gT_arr(i,j) + tmpVar
        ENDDO
      ENDDO

#else  /* USE_OLD_EXTERNAL_FORCING */

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingZCoords .AND. useShelfIce ) THEN
       kSurface = -1
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF
      recip_Cp = 1. _d 0 / HeatCapacity_Cp

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_T(
     U                       gT_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_ATM_PHYS
      IF ( useAtm_Phys ) CALL ATM_PHYS_TENDENCY_APPLY_T(
     U                       gT_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_T(
     U                       gT_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_FIZHI */

#ifdef ALLOW_ADDFLUID
      IF ( selectAddFluid.NE.0 .AND. temp_addMass.NE.UNSET_RL ) THEN
       IF ( ( selectAddFluid.GE.1 .AND. nonlinFreeSurf.GT.0 )
     &      .OR. convertFW2Salt.EQ.-1. _d 0 ) THEN
         DO j=1,sNy
          DO i=1,sNx
            gT_arr(i,j) = gT_arr(i,j)
     &        + addMass(i,j,k,bi,bj)*mass2rUnit
     &          *( temp_addMass - theta(i,j,k,bi,bj) )
     &          *recip_rA(i,j,bi,bj)
     &          *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &          *recip_deepFac2C(k)*recip_rhoFacC(k)
          ENDDO
         ENDDO
       ELSE
         DO j=1,sNy
          DO i=1,sNx
            gT_arr(i,j) = gT_arr(i,j)
     &        + addMass(i,j,k,bi,bj)*mass2rUnit
     &          *( temp_addMass - tRef(k) )
     &          *recip_rA(i,j,bi,bj)
     &          *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &          *recip_deepFac2C(k)*recip_rhoFacC(k)
          ENDDO
         ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_ADDFLUID */

#ifdef ALLOW_FRICTION_HEATING
      IF ( addFrictionHeating ) THEN
        IF ( fluidIsAir ) THEN
C         conversion from in-situ Temp to Pot.Temp
          tmpFac = (atm_Po/rC(k))**atm_kappa
C         conversion from W/m^2/r_unit to K/s
          tmpFac = (tmpFac/atm_Cp) * mass2rUnit
        ELSE
C         conversion from W/m^2/r_unit to K/s
          tmpFac = recip_Cp * mass2rUnit
        ENDIF
        DO j=1,sNy
          DO i=1,sNx
            gT_arr(i,j) = gT_arr(i,j)
     &         + frictionHeating(i,j,k,bi,bj)
     &          *tmpFac*recip_rA(i,j,bi,bj)
     &          *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_FRICTION_HEATING */

      IF ( fluidIsAir .AND. atm_Rq.NE.zeroRL .AND. Nr.NE.1 ) THEN
C--   Compressible fluid: account for difference between moist and dry air
C     specific volume in Enthalpy equation (+ V.dP term), since only the
C     dry air part is accounted for in the (dry) Pot.Temp formulation.
C     Used centered averaging from interface to center (consistent with
C     conversion term in KE eq) and same discretisation ( [T*Q]_bar_k )
C     as for Theta_v in CALC_PHI_HYD

C     conversion from in-situ Temp to Pot.Temp
        tmpFac = (atm_Po/rC(k))**atm_kappa
C     conversion from W/kg to K/s
        tmpFac = tmpFac/atm_Cp
        km = k-1
        kc = k
        kp = k+1
        IF ( k.EQ.1 ) THEN
          DO j=1,sNy
           DO i=1,sNx
            tmpVar(i,j) = 0.
           ENDDO
          ENDDO
        ELSE
          delPI = atm_Cp*( (rC(km)/atm_Po)**atm_kappa
     &                   - (rC(kc)/atm_Po)**atm_kappa )
          DO j=1,sNy
           DO i=1,sNx
            tmpVar(i,j) = wVel(i,j,kc,bi,bj)*delPI*atm_Rq
     &                  *( theta(i,j,km,bi,bj)*salt(i,j,km,bi,bj)
     &                   + theta(i,j,kc,bi,bj)*salt(i,j,kc,bi,bj)
     &                   )*maskC(i,j,km,bi,bj)*0.25 _d 0
           ENDDO
          ENDDO
        ENDIF
        IF ( k.LT.Nr ) THEN
          delPI = atm_Cp*( (rC(kc)/atm_Po)**atm_kappa
     &                   - (rC(kp)/atm_Po)**atm_kappa )
          DO j=1,sNy
           DO i=1,sNx
            tmpVar(i,j) = tmpVar(i,j)
     &                  + wVel(i,j,kp,bi,bj)*delPI*atm_Rq
     &                  *( theta(i,j,kc,bi,bj)*salt(i,j,kc,bi,bj)
     &                   + theta(i,j,kp,bi,bj)*salt(i,j,kp,bi,bj)
     &                   )*maskC(i,j,kp,bi,bj)*0.25 _d 0
           ENDDO
          ENDDO
        ENDIF
        DO j=1,sNy
          DO i=1,sNx
            gT_arr(i,j) = gT_arr(i,j)
     &         + tmpVar(i,j)*tmpFac
     &          *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
          ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
C     conversion to W/m^2
          tmpFac = rUnit2mass
          CALL DIAGNOSTICS_SCALE_FILL( tmpVar, tmpFac, 1,
     &                     'MoistCor', kc, 1, 3, bi,bj,myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF

C     Ocean: Add temperature surface forcing (e.g., heat-flux) in surface level
      IF ( k .EQ. kSurface ) THEN
       DO j=1,sNy
        DO i=1,sNx
          gT_arr(i,j) = gT_arr(i,j)
     &      +surfaceForcingT(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( kSurface.EQ.-1 ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF ( kSurfC(i,j,bi,bj).EQ.k ) THEN
          gT_arr(i,j) = gT_arr(i,j)
     &      +surfaceForcingT(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      IF (linFSConserveTr) THEN
       DO j=1,sNy
        DO i=1,sNx
          IF (k .EQ. kSurfC(i,j,bi,bj)) THEN
            gT_arr(i,j) = gT_arr(i,j)
     &        +TsurfCor*recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
          ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef SHORTWAVE_HEATING
C Penetrating SW radiation
c     IF ( usePenetratingSW ) THEN
       swfracb(1)=abs(rF(k))
       swfracb(2)=abs(rF(k+1))
       CALL SWFRAC(
     I             2, minusOne,
     U             swfracb,
     I             myTime, 1, myThid )
       kp1 = k+1
       IF (k.EQ.Nr) THEN
        kp1 = k
        swfracb(2)=0. _d 0
       ENDIF
       DO j=1,sNy
        DO i=1,sNx
         gT_arr(i,j) = gT_arr(i,j)
     &   -Qsw(i,j,bi,bj)*(swfracb(1)*maskC(i,j,k,bi,bj)
     &                   -swfracb(2)*maskC(i,j,kp1, bi,bj))
     &    *recip_Cp*mass2rUnit
     &    *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
c     ENDIF
#endif

C---- Experiment specific code: starts here --------------------------
      kDiffCyl = 3. _d -7
      rSource  = 3
      iGSource = 30
      jGSource = 8
      DO j=1,sNy
       DO i=1,sNx
        dFluxW = 0.
        dFluxE = 0.
        dFluxN = 0.
        dFluxS = 0.
        jG = myYGlobalLo-1+(bj-1)*sNy+J
        iG = myXGlobalLo-1+(bi-1)*sNx+I
c      IF(jG .GE. jGSource-rSource .AND. jG .LE. jGSource+rSource) THEN
c     the following bites the big one
      IF(jG .LE. 10) THEN
         tCyl = tCylIn
      ELSE
         tCyl = tCylOut
      ENDIF
c      IF(iG .GE. iGSource-rSource .AND. iG .LE. iGSource+rSource) THEN
          hC = hFacC(i  ,j  ,k,bi,bj)
          hW = hFacW(i  ,j  ,k,bi,bj)
          hE = hFacW(i+1,j  ,k,bi,bj)
          hN = hFacS(i  ,j+1,k,bi,bj)
          hS = hFacS(i  ,j  ,k,bi,bj)
          IF ( hC .NE. 0. .AND .hW .EQ. 0. ) THEN
C          Cylinder to west
           faceArea = drF(k)*dyG(i,j,bi,bj)
           dFluxW =
     &      -faceArea*kDiffCyl*(theta(i,j,k,bi,bj) - tCyl)
     &      *recip_dxC(i,j,bi,bj)
          ENDIF
          IF ( hC .NE. 0. .AND .hE .EQ. 0. ) THEN
C          Cylinder to east
           faceArea = drF(k)*dyG(i+1,j,bi,bj)
           dFluxE =
     &      -faceArea*kDiffCyl*(tCyl - theta(i,j,k,bi,bj))
     &      *recip_dxC(i,j,bi,bj)
          ENDIF
          IF ( hC .NE. 0. .AND .hN .EQ. 0. ) THEN
C          Cylinder to north
           faceArea = drF(k)*dxG(i,j+1,bi,bj)
           dFluxN =
     &      -faceArea*kDiffCyl*(tCyl-theta(i,j,k,bi,bj))
     &      *recip_dyC(i,j,bi,bj)
          ENDIF
          IF ( hC .NE. 0. .AND .hS .EQ. 0. ) THEN
C          Cylinder to south
           faceArea = drF(k)*dxG(i,j,bi,bj)
           dFluxS =
     &      -faceArea*kDiffCyl*(theta(i,j,k,bi,bj) - tCyl)
     &      *recip_dyC(i,j,bi,bj)
          ENDIF
c      ENDIF
c      ENDIF
C       Net tendency term is minus flux divergence divided by the volume.
        gT_arr(i,j) = gT_arr(i,j)
     &  -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &  *recip_rA(i,j,bi,bj)
     &  *(
     &    dFluxE-dFluxW
     &   +dFluxN-dFluxS
     &   )

       ENDDO
      ENDDO
C---- Experiment specific code:  ends  here --------------------------

#ifdef ALLOW_FRAZIL
      IF ( useFRAZIL )
     &     CALL FRAZIL_TENDENCY_APPLY_T(
     U                 gT_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
#endif /* ALLOW_FRAZIL */

#ifdef ALLOW_SHELFICE
      IF ( useShelfIce )
     &     CALL SHELFICE_FORCING_T(
     U                   gT_arr,
     I                   iMin,iMax,jMin,jMax, k, bi,bj,
     I                   myTime, 0, myThid )
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_ICEFRONT
      IF ( useICEFRONT )
     &     CALL ICEFRONT_TENDENCY_APPLY_T(
     U                   gT_arr,
     I                   k, bi, bj, myTime, 0, myThid )
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME )
     &     CALL SALT_PLUME_TENDENCY_APPLY_T(
     U                     gT_arr,
     I                     iMin,iMax,jMin,jMax, k, bi,bj,
     I                     myTime, 0, myThid )
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_RBCS
      IF (useRBCS) THEN
        CALL RBCS_ADD_TENDENCY(
     U                 gT_arr,
     I                 k, bi, bj, 1,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_RBCS */

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
        CALL OBCS_SPONGE_T(
     U                   gT_arr,
     I                   iMin,iMax,jMin,jMax, k, bi,bj,
     I                   myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_BBL
      IF ( useBBL ) CALL BBL_TENDENCY_APPLY_T(
     U                       gT_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_BBL */

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) THEN
        CALL MYPACKAGE_TENDENCY_APPLY_T(
     U                 gT_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#endif /* USE_OLD_EXTERNAL_FORCING */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_S
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_S(
     U                     gS_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_S
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gS for forcing by external sources
C     | e.g. fresh-water flux, climatalogical relaxation, etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
C     kSurface  :: index of surface level
      INTEGER i, j
#ifdef USE_OLD_EXTERNAL_FORCING
      _RL     locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpVar
#else
      INTEGER kSurface
#endif
CEOP

#ifdef USE_OLD_EXTERNAL_FORCING

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          locVar(i,j) = gS(i,j,k,bi,bj)
        ENDDO
      ENDDO
      CALL EXTERNAL_FORCING_S(
     I              iMin, iMax, jMin, jMax, bi, bj, k,
     I              myTime, myThid )
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          tmpVar = gS(i,j,k,bi,bj) - locVar(i,j)
          gS(i,j,k,bi,bj) = locVar(i,j)
          gS_arr(i,j) = gS_arr(i,j) + tmpVar
        ENDDO
      ENDDO

#else  /* USE_OLD_EXTERNAL_FORCING */

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingZCoords .AND. useShelfIce ) THEN
       kSurface = -1
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_S(
     U                       gS_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_ATM_PHYS
      IF ( useAtm_Phys ) CALL ATM_PHYS_TENDENCY_APPLY_S(
     U                       gS_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_S(
     U                       gS_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_FIZHI */

#ifdef ALLOW_ADDFLUID
      IF ( selectAddFluid.NE.0 .AND. salt_addMass.NE.UNSET_RL ) THEN
       IF ( ( selectAddFluid.GE.1 .AND. nonlinFreeSurf.GT.0 )
     &      .OR. convertFW2Salt.EQ.-1. _d 0 ) THEN
         DO j=1,sNy
          DO i=1,sNx
            gS_arr(i,j) = gS_arr(i,j)
     &        + addMass(i,j,k,bi,bj)*mass2rUnit
     &          *( salt_addMass - salt(i,j,k,bi,bj) )
     &          *recip_rA(i,j,bi,bj)
     &          *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &          *recip_deepFac2C(k)*recip_rhoFacC(k)
          ENDDO
         ENDDO
       ELSE
         DO j=1,sNy
          DO i=1,sNx
            gS_arr(i,j) = gS_arr(i,j)
     &        + addMass(i,j,k,bi,bj)*mass2rUnit
     &          *( salt_addMass - sRef(k) )
     &          *recip_rA(i,j,bi,bj)
     &          *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &          *recip_deepFac2C(k)*recip_rhoFacC(k)
          ENDDO
         ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_ADDFLUID */

C     Ocean: Add salinity surface forcing (e.g., fresh-water) in surface level
      IF ( k .EQ. kSurface ) THEN
       DO j=1,sNy
        DO i=1,sNx
          gS_arr(i,j) = gS_arr(i,j)
     &      +surfaceForcingS(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( kSurface.EQ.-1 ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF ( kSurfC(i,j,bi,bj).EQ.k ) THEN
          gS_arr(i,j) = gS_arr(i,j)
     &      +surfaceForcingS(i,j,bi,bj)
     &      *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      IF (linFSConserveTr) THEN
       DO j=1,sNy
        DO i=1,sNx
          IF (k .EQ. kSurfC(i,j,bi,bj)) THEN
            gS_arr(i,j) = gS_arr(i,j)
     &        +SsurfCor*recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
          ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_SHELFICE
      IF ( useShelfIce )
     &     CALL SHELFICE_FORCING_S(
     U                   gS_arr,
     I                   iMin,iMax,jMin,jMax, k, bi,bj,
     I                   myTime, 0, myThid )
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_ICEFRONT
      IF ( useICEFRONT )
     &     CALL ICEFRONT_TENDENCY_APPLY_S(
     U                   gS_arr,
     I                   k, bi, bj, myTime, 0, myThid )
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME )
     &     CALL SALT_PLUME_TENDENCY_APPLY_S(
     U                     gS_arr,
     I                     iMin,iMax,jMin,jMax, k, bi,bj,
     I                     myTime, 0, myThid )
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_RBCS
      IF (useRBCS) THEN
        CALL RBCS_ADD_TENDENCY(
     U                 gS_arr,
     I                 k, bi, bj, 2,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_RBCS */

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
        CALL OBCS_SPONGE_S(
     U                   gS_arr,
     I                   iMin,iMax,jMin,jMax, k, bi,bj,
     I                   myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_BBL
      IF ( useBBL ) CALL BBL_TENDENCY_APPLY_S(
     U                       gS_arr,
     I                       iMin,iMax,jMin,jMax, k, bi,bj,
     I                       myTime, 0, myThid )
#endif /* ALLOW_BBL */

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) THEN
        CALL MYPACKAGE_TENDENCY_APPLY_S(
     U                 gS_arr,
     I                 iMin,iMax,jMin,jMax, k, bi,bj,
     I                 myTime, 0, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#endif /* USE_OLD_EXTERNAL_FORCING */

      RETURN
      END
