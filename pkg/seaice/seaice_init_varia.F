C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_init_varia.F,v 1.30 2008/04/06 03:13:40 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_INIT_VARIA( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_INIT_VARIA                             |
C     | o Initialization of sea ice model.                       |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SEAICE.h"
#include "SEAICE_DIAGS.h"
#include "SEAICE_PARAMS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_TOPOLOGY.h"
# include "W2_EXCH2_PARAMS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
# include "OBCS.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      _RL PSTAR
      _RS  mask_uice
      INTEGER myIter, myTile
#ifdef SEAICE_ALLOW_TD_IF
cif(
cif   Helper variable for determining the fraction of sw radiation
cif   penetrating the model's shallowest layer
      _RL swfracba(2)
      _RL FACTORM,dummyTime
      INTEGER IMAX
cif)
#endif

#ifdef ALLOW_OBCS
      INTEGER I_obc, J_obc, kSurface
      IF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF
#endif /* ALLOW_OBCS */

#ifdef SEAICE_ALLOW_TD_IF
cif(
#ifdef SHORTWAVE_HEATING
      IMAX = 2
      FACTORM = -1.0
      dummyTime = 1.0
      swfracba(1) = abs(rF(1)) 
      swfracba(2) = abs(rF(2))
      CALL SWFRAC(
     I       IMAX,FACTORM,
     I       dummyTime,myThid,
     U       swfracba)
      SWFRACB = swfracba(2)
#endif
cif)
#endif

C--   Initialise all variables in common blocks:
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,3
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HEFF(i,j,k,bi,bj)=0. _d 0
           AREA(i,j,k,bi,bj)=0. _d 0
           UICE(i,j,k,bi,bj)=0. _d 0
           VICE(i,j,k,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO
#ifdef SEAICE_MULTICATEGORY
        DO k=1,MULTDIM
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           TICES(i,j,k,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ETA (i,j,bi,bj)   = 0. _d 0
          ZETA(i,j,bi,bj)   = 0. _d 0
          DRAGS(i,j,bi,bj)  = 0. _d 0
          DRAGA(i,j,bi,bj)  = 0. _d 0
          FORCEX(i,j,bi,bj) = 0. _d 0
          FORCEY(i,j,bi,bj) = 0. _d 0
          UICEC(i,j,bi,bj)  = 0. _d 0
          VICEC(i,j,bi,bj)  = 0. _d 0
#ifdef SEAICE_CGRID
          seaiceMassC(i,j,bi,bj)=0. _d 0
          seaiceMassU(i,j,bi,bj)=0. _d 0
          seaiceMassV(i,j,bi,bj)=0. _d 0
          seaiceMaskU(i,j,bi,bj)=0. _d 0
          seaiceMaskV(i,j,bi,bj)=0. _d 0
# ifdef SEAICE_ALLOW_EVP
          stressDivergenceX(i,j,bi,bj) = 0. _d 0
          stressDivergenceY(i,j,bi,bj) = 0. _d 0
          seaice_sigma1 (i,j,bi,bj) = 0. _d 0
          seaice_sigma2 (i,j,bi,bj) = 0. _d 0
          seaice_sigma12(i,j,bi,bj) = 0. _d 0
# endif /* SEAICE_ALLOW_EVP */
#else /* SEAICE_CGRID */
          AMASS(i,j,bi,bj)  = 0. _d 0
          DAIRN(i,j,bi,bj)  = 0. _d 0
          UVM(i,j,bi,bj)    = 0. _d 0
          WINDX(i,j,bi,bj)  = 0. _d 0
          WINDY(i,j,bi,bj)  = 0. _d 0
          GWATX(i,j,bi,bj)  = 0. _d 0
          GWATY(i,j,bi,bj)  = 0. _d 0
          KGEO(i,j,bi,bj)   = 0
#endif /* SEAICE_CGRID */
          DWATN(i,j,bi,bj)  = 0. _d 0
          PRESS0(i,j,bi,bj) = 0. _d 0
          FORCEX0(i,j,bi,bj)= 0. _d 0
          FORCEY0(i,j,bi,bj)= 0. _d 0
          ZMAX(i,j,bi,bj)   = 0. _d 0
          ZMIN(i,j,bi,bj)   = 0. _d 0
          HSNOW(i,j,bi,bj)  = 0. _d 0
#ifdef SEAICE_SALINITY
          HSALT(i,j,bi,bj)  = 0. _d 0
#endif
          HEFFM(i,j,bi,bj)  = 0. _d 0
          YNEG (i,j,bi,bj)  = 0. _d 0
          RIVER(i,j,bi,bj)  = 0. _d 0
          TMIX(i,j,bi,bj)   = 0. _d 0
          TICE(i,j,bi,bj)   = 0. _d 0
          TAUX(i,j,bi,bj)   = 0. _d 0
          TAUY(i,j,bi,bj)   = 0. _d 0
#ifdef ALLOW_SEAICE_COST_EXPORT
          uHeffExportCell(i,j,bi,bj) = 0. _d 0
          vHeffExportCell(i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Initialize grid info
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM(i,j,bi,bj)=ONE
          IF (_hFacC(i,j,1,bi,bj).eq.0.) HEFFM(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
#ifdef SEAICE_CGRID
          seaiceMaskU(i,j,bi,bj)=   0.0 _d 0
          seaiceMaskV(i,j,bi,bj)=   0.0 _d 0
          mask_uice=HEFFM(i,j,bi,bj)+HEFFM(i-1,j  ,bi,bj)
          IF(mask_uice.GT.1.5) seaiceMaskU(i,j,bi,bj)=ONE
          mask_uice=HEFFM(i,j,bi,bj)+HEFFM(i  ,j-1,bi,bj)
          IF(mask_uice.GT.1.5) seaiceMaskV(i,j,bi,bj)=ONE
#else
          UVM(i,j,bi,bj)=0. _d 0
          mask_uice=HEFFM(i,j,  bi,bj)+HEFFM(i-1,j-1,bi,bj)
     &             +HEFFM(i,j-1,bi,bj)+HEFFM(i-1,j,  bi,bj)
          IF(mask_uice.GT.3.5) UVM(i,j,bi,bj)=ONE
#endif /* SEAICE_CGRID */
         ENDDO
        ENDDO

#ifdef ALLOW_OBCS
        IF (useOBCS) THEN
C--   If OBCS is turned on, close southern and western boundaries
#ifdef ALLOW_OBCS_SOUTH
         DO i=1-Olx,sNx+Olx
C Southern boundary
          J_obc = OB_Js(i,bi,bj)
          IF (J_obc.NE.0) THEN
#ifdef SEAICE_CGRID
           seaiceMaskU(i,J_obc,bi,bj)=   0.0 _d 0
           seaiceMaskV(i,J_obc,bi,bj)=   0.0 _d 0
#else
           UVM(i,J_obc,bi,bj)=0. _d 0
#endif /* SEAICE_CGRID */
          ENDIF
         ENDDO
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_WEST
         DO j=1-Oly,sNy+Oly
C Western boundary
          I_obc=OB_Iw(j,bi,bj)
          IF (I_obc.NE.0) THEN
#ifdef SEAICE_CGRID
           seaiceMaskU(I_obc,j,bi,bj)=   0.0 _d 0
           seaiceMaskV(I_obc,j,bi,bj)=   0.0 _d 0
#else
           UVM(I_obc,j,bi,bj)=0. _d 0
#endif /* SEAICE_CGRID */
          ENDIF
         ENDDO
#endif /* ALLOW_OBCS_WEST */
        ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_EXCH2
#ifndef SEAICE_CGRID
C--   Special stuff for cubed sphere: assume grid is rectangular and
C     set UV mask to zero except for Arctic and Antarctic cube faces.
        IF (useCubedSphereExchange) THEN
         myTile = W2_myTileList(bi)
         IF ( exch2_myFace(myTile) .EQ. 1 .OR.
     &        exch2_myFace(myTile) .EQ. 2 .OR.
     &        exch2_myFace(myTile) .EQ. 4 .OR.
     &        exch2_myFace(myTile) .EQ. 5 ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            UVM(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDDO
         ELSEIF ( exch2_isWedge(myTile) .EQ. 1 ) THEN
          i=1
          DO j=1-OLy,sNy+OLy
           UVM(i,j,bi,bj)=0. _d 0
          ENDDO
         ELSEIF ( exch2_isSedge(myTile) .EQ. 1 ) THEN
          j=1
          DO i=1-OLx,sNx+OLx
           UVM(i,j,bi,bj)=0. _d 0
          ENDDO
         ENDIF
        ENDIF
#endif /* SEAICE_CGRID */
#endif /* ALLOW_EXCH2 */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          TICE(i,j,bi,bj)=273.0 _d 0
#ifdef SEAICE_MULTICATEGORY
          DO k=1,MULTDIM
           TICES(i,j,k,bi,bj)=273.0 _d 0
          ENDDO
#endif /* SEAICE_MULTICATEGORY */
#ifndef SEAICE_CGRID
          AMASS      (i,j,bi,bj)=1000.0 _d 0
#else
          seaiceMassC(i,j,bi,bj)=1000.0 _d 0
          seaiceMassU(i,j,bi,bj)=1000.0 _d 0
          seaiceMassV(i,j,bi,bj)=1000.0 _d 0
#endif
         ENDDO
        ENDDO

#ifndef SEAICE_CGRID
C--   Choose a proxy level for geostrophic velocity,
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef SEAICE_TEST_ICE_STRESS_1
          KGEO(i,j,bi,bj) = 1
#else /* SEAICE_TEST_ICE_STRESS_1 */
          IF (klowc(i,j,bi,bj) .LT. 2) THEN
           KGEO(i,j,bi,bj) = 1
          ELSE
           KGEO(i,j,bi,bj) = 2
           DO WHILE ( abs(rC(KGEO(i,j,bi,bj))) .LT. 50.0 .AND.
     &          KGEO(i,j,bi,bj) .LT. (klowc(i,j,bi,bj)-1) )
              KGEO(i,j,bi,bj) = KGEO(i,j,bi,bj) + 1
           ENDDO
          ENDIF
#endif /* SEAICE_TEST_ICE_STRESS_1 */
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */

       ENDDO
      ENDDO

C--   Update overlap regions
#ifdef SEAICE_CGRID
      CALL EXCH_UV_XY_RL(seaiceMaskU,seaiceMaskV,.FALSE.,myThid)
#else
      _EXCH_XY_R8(UVM, myThid)
#endif

C--   Now lets look at all these beasts
      IF ( debugLevel .GE. debLevB ) THEN
         myIter=0
         CALL PLOT_FIELD_XYRL( HEFFM   , 'Current HEFFM   ' ,
     &        myIter, myThid )
#ifdef SEAICE_CGRID
         CALL PLOT_FIELD_XYRL( seaiceMaskU, 'Current seaiceMaskU',
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( seaiceMaskV, 'Current seaiceMaskV',
     &        myIter, myThid )
#else
         CALL PLOT_FIELD_XYRL( UVM     , 'Current UVM     ' ,
     &        myIter, myThid )
#endif
      ENDIF

C--   Set model variables to initial/restart conditions
      IF ( .NOT. ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ') ) THEN

         CALL SEAICE_READ_PICKUP ( myThid )

      ELSE

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           TMIX(i,j,bi,bj)=TICE(i,j,bi,bj)
           DO k=1,3
            HEFF(i,j,k,bi,bj)=SEAICE_initialHEFF*HEFFM(i,j,bi,bj)
            UICE(i,j,k,bi,bj)=ZERO
            VICE(i,j,k,bi,bj)=ZERO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea-ice thickness from file if available.
       IF ( HeffFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( HeffFile, ' ', ZETA, 0, myThid )
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DO k=1,3
             HEFF(i,j,k,bi,bj) = MAX(ZETA(i,j,bi,bj),ZERO)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO k=1,3
            IF(HEFF(i,j,k,bi,bj).GT.ZERO)
     &           AREA(i,j,k,bi,bj)=ONE
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea-ice area from file if available.
       IF ( AreaFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( AreaFile, ' ', ZETA, 0, myThid )
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DO k=1,3
             AREA(i,j,k,bi,bj) = MAX(ZETA(i,j,bi,bj),ZERO)
             AREA(i,j,k,bi,bj) = MIN(AREA(i,j,k,bi,bj),ONE)
             IF ( AREA(i,j,k,bi,bj) .LE. ZERO )
     &            HEFF(i,j,k,bi,bj) = ZERO
             IF ( HEFF(i,j,k,bi,bj) .LE. ZERO )
     &            AREA(i,j,k,bi,bj) = ZERO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HSNOW(i,j,bi,bj)=0.2*AREA(i,j,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial snow thickness from file if available.
       IF ( HsnowFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( HsnowFile, ' ', ZETA, 0, myThid )
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSNOW(i,j,bi,bj) = MAX(ZETA(i,j,bi,bj),ZERO)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

#ifdef SEAICE_SALINITY
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HSALT(i,j,bi,bj)=HEFF(i,j,1,bi,bj)*salt(i,j,1,bi,bj)*
     &            ICE2WATR*rhoConstFresh*SEAICE_salinity
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea ice salinity from file if available.
       IF ( HsaltFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( HsaltFile, ' ', ZETA, 0, myThid )
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSALT(i,j,bi,bj) = ZETA(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* SEAICE_SALINITY */

      ENDIF

C---  Complete initialization
      PSTAR = SEAICE_strength
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ZETA(i,j,bi,bj)=HEFF(i,j,1,bi,bj)*(1.0 _d 11)
          ETA(i,j,bi,bj)=ZETA(i,j,bi,bj)/4.0 _d 0
          PRESS0(i,j,bi,bj)=PSTAR*HEFF(i,j,1,bi,bj)
     &         *EXP(-20.0 _d 0*(ONE-AREA(i,j,1,bi,bj)))
          ZMAX(i,j,bi,bj)=(5.0 _d +12/(2.0 _d +04))*PRESS0(i,j,bi,bj)
          ZMIN(i,j,bi,bj)=SEAICE_zetaMin
          PRESS0(i,j,bi,bj)=PRESS0(i,j,bi,bj)*HEFFM(i,j,bi,bj)
         ENDDO
        ENDDO
        IF ( useRealFreshWaterFlux .AND. .NOT.useThSIce ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           sIceLoad(i,j,bi,bj) = HEFF(i,j,1,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(i,j,bi,bj)* 330. _d 0

          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
