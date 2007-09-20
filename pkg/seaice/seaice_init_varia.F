C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_init_varia.F,v 1.20 2007/09/20 14:32:37 dimitri Exp $
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
#include "SEAICE.h"
CML#include "SEAICE_GRID.h"
#include "SEAICE_DIAGS.h"
#include "SEAICE_PARAMS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */
#ifdef ALLOW_OBCS
#include "OBCS.h"
#include "OBCS_OPTIONS.h"
#endif /* ALLOW_OBCS */

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

#ifdef ALLOW_OBCS
      INTEGER I_obc, J_obc, kSurface
      _RL obc_mask

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
       kSurface        = Nr 
      else
       kSurface        = 1
      endif
#endif /* ALLOW_OBCS */

cph(
cph   make sure TAF sees proper initialisation
cph   to avoid partial recomputation issues
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c
        DO K=1,3
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           HEFF(I,J,k,bi,bj)=0. _d 0
           AREA(I,J,k,bi,bj)=0. _d 0
           UICE(I,J,k,bi,bj)=0. _d 0
           VICE(I,J,k,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO
c
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          ZETA(I,J,bi,bj)=0. _d 0
          HSNOW(I,J,bi,bj)=0. _d 0
#ifdef SEAICE_SALINITY
          HSALT(I,J,bi,bj)=0. _d 0
#endif
         ENDDO
        ENDDO
c
       ENDDO
      ENDDO
cph)

C--   Initialize grid info
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM(i,j,bi,bj)=ONE
          IF (_hFacC(i,j,1,bi,bj).eq.0.) HEFFM(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO

#ifdef ALLOW_OBCS
        IF (useOBCS) THEN
C     Apply OBCS T/S mask (taken from pkg/obcs/obcs_apply_ts.F)
C     Additionally apply a mask along all open boundaries
C     Until proper seaice open boundaries are implemented, this
C     masking is needed to avoid communication across open boundaries.
#ifdef ALLOW_OBCS_NORTH
         DO I=1-Olx,sNx+Olx
C Northern boundary
          J_obc = OB_Jn(I,bi,bj)
          IF (J_obc.NE.0) THEN
           HEFFM(I,J_obc,bi,bj)=0. _d 0
           obc_mask = _maskS(I,J_obc,kSurface,bi,bj)
           DO J = J_obc, J_obc+Oly
            IF (obc_mask.eq.0.) HEFFM(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDIF
         ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
         DO I=1-Olx,sNx+Olx
C Southern boundary
          J_obc = OB_Js(I,bi,bj)
          IF (J_obc.NE.0) THEN
           HEFFM(I,J_obc,bi,bj)=0. _d 0
           obc_mask = _maskS(I,J_obc+1,kSurface,bi,bj)
           DO J = J_obc-Oly, J_obc
            IF (obc_mask.eq.0.) HEFFM(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDIF
         ENDDO
#endif
#ifdef ALLOW_OBCS_EAST
         DO J=1-Oly,sNy+Oly
C Eastern boundary
          I_obc = OB_Ie(J,bi,bj)
          IF (I_obc.NE.0) THEN
           HEFFM(I_obc,J,bi,bj)=0. _d 0
           obc_mask = _maskW(I_obc,J,kSurface,bi,bj)
           DO I = I_obc, I_obc+Olx
            IF (obc_mask.eq.0.) HEFFM(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDIF
         ENDDO
#endif
#ifdef ALLOW_OBCS_WEST
         DO J=1-Oly,sNy+Oly
C Western boundary
          I_obc=OB_Iw(J,bi,bj)
          IF (I_obc.NE.0) THEN
           HEFFM(I_obc,J,bi,bj)=0. _d 0
           obc_mask = _maskW(I_obc+1,J,kSurface,bi,bj)
           DO I = I_obc-Olx, I_obc
            IF (obc_mask.eq.0.) HEFFM(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDIF
         ENDDO
#endif
         _EXCH_XY_R8(HEFFM, myThid)
        ENDIF
#endif /* ALLOW_OBCS */

        DO J=1-OLy+1,sNy+OLy
         DO I=1-OLx+1,sNx+OLx
#ifdef SEAICE_CGRID
          seaiceMaskU(I,J,bi,bj)=   0.0 _d 0
          seaiceMaskV(I,J,bi,bj)=   0.0 _d 0
          mask_uice=HEFFM(I,J,bi,bj)+HEFFM(I-1,J  ,bi,bj)
          IF(mask_uice.GT.1.5) seaiceMaskU(I,J,bi,bj)=ONE
          mask_uice=HEFFM(I,J,bi,bj)+HEFFM(I  ,J-1,bi,bj)
          IF(mask_uice.GT.1.5) seaiceMaskV(I,J,bi,bj)=ONE
#else
          UVM(i,j,bi,bj)=0. _d 0
          mask_uice=HEFFM(I,J,  bi,bj)+HEFFM(I-1,J-1,bi,bj)
     &             +HEFFM(I,J-1,bi,bj)+HEFFM(I-1,J,  bi,bj)
          IF(mask_uice.GT.3.5) UVM(I,J,bi,bj)=ONE
#endif /* SEAICE_CGRID */
         ENDDO
        ENDDO

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
          DO J=1-OLy,sNy+OLy
           DO I=1-OLx,sNx+OLx
            UVM(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDDO
         ELSEIF ( exch2_isWedge(myTile) .EQ. 1 ) THEN
          I=1
          DO J=1-OLy,sNy+OLy
           UVM(i,j,bi,bj)=0. _d 0
          ENDDO
         ELSEIF ( exch2_isSedge(myTile) .EQ. 1 ) THEN
          J=1
          DO I=1-OLx,sNx+OLx
           UVM(i,j,bi,bj)=0. _d 0
          ENDDO
         ENDIF
        ENDIF
#endif /* SEAICE_CGRID */
#endif /* ALLOW_EXCH2 */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          TICE(I,J,bi,bj)=273.0 _d 0
#ifdef SEAICE_MULTICATEGORY
          DO k=1,MULTDIM
           TICES(I,J,k,bi,bj)=273.0 _d 0
          ENDDO
#endif /* SEAICE_MULTICATEGORY */
          UICEC      (I,J,bi,bj)=0. _d 0
          VICEC      (I,J,bi,bj)=0. _d 0
          DWATN      (I,J,bi,bj)=0. _d 0
#ifndef SEAICE_CGRID
          DAIRN      (I,J,bi,bj)=0. _d 0
          AMASS      (I,J,bi,bj)=1000.0 _d 0
#else
          seaiceMassC(I,J,bi,bj)=1000.0 _d 0
          seaiceMassU(I,J,bi,bj)=1000.0 _d 0
          seaiceMassV(I,J,bi,bj)=1000.0 _d 0
#endif
          GWATX      (I,J,bi,bj)=0. _d 0
          GWATY      (I,J,bi,bj)=0. _d 0
         ENDDO
        ENDDO

C--   Choose a proxy level for geostrophic velocity,
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef SEAICE_TEST_ICE_STRESS_1
          KGEO(I,J,bi,bj) = 1
#else /* SEAICE_TEST_ICE_STRESS_1 */
          IF (klowc(i,j,bi,bj) .LT. 2) THEN
           KGEO(I,J,bi,bj) = 1
          ELSE
           KGEO(I,J,bi,bj) = 2
           DO WHILE ( abs(rC(KGEO(I,J,bi,bj))) .LT. 50.0 .AND.
     &          KGEO(I,J,bi,bj) .LT. (klowc(i,j,bi,bj)-1) )
              KGEO(I,J,bi,bj) = KGEO(I,J,bi,bj) + 1
           ENDDO
          ENDIF
#endif /* SEAICE_TEST_ICE_STRESS_1 */
         ENDDO
        ENDDO

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

#if (defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_EVP))
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           stressDivergenceX(I,J,bi,bj) = 0. _d 0
           stressDivergenceY(I,J,bi,bj) = 0. _d 0
           seaice_sigma1 (I,J,bi,bj) = 0. _d 0
           seaice_sigma2 (I,J,bi,bj) = 0. _d 0
           seaice_sigma12(I,J,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif /* SEAICE_ALLOW_EVP and SEAICE_CGRID */

C--   Set model variables to initial/restart conditions
      IF ( nIter0 .NE. 0 ) THEN

         CALL SEAICE_READ_PICKUP ( myThid )

      ELSE

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           YNEG(I,J,bi,bj)=ZERO
           TMIX(I,J,bi,bj)=TICE(I,J,bi,bj)
           DO k=1,3
            HEFF(I,J,k,bi,bj)=SEAICE_initialHEFF*HEFFM(i,j,bi,bj)
            UICE(I,J,k,bi,bj)=ZERO
            VICE(I,J,k,bi,bj)=ZERO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea-ice thickness from file if available.
       IF ( HeffFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL READ_FLD_XY_RL( HeffFile, ' ', ZETA, 0, myThid )
        _END_MASTER(myThid)
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DO k=1,3
             HEFF(I,J,k,bi,bj) = MAX(ZETA(i,j,bi,bj),ZERO)
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
            IF(HEFF(I,J,k,bi,bj).GT.ZERO) 
     &           AREA(I,J,k,bi,bj)=ONE
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
         
C--   Read initial area thickness from file if available.
       IF ( AreaFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL READ_FLD_XY_RL( AreaFile, ' ', ZETA, 0, myThid )
        _END_MASTER(myThid)
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DO k=1,3
             AREA(I,J,k,bi,bj) = MAX(ZETA(i,j,bi,bj),ZERO)
             AREA(I,J,k,bi,bj) = MIN(AREA(i,j,k,bi,bj),ONE)
             IF ( AREA(I,J,k,bi,bj) .LE. ZERO )
     &            HEFF(I,J,k,bi,bj) = ZERO
             IF ( HEFF(I,J,k,bi,bj) .LE. ZERO )
     &            AREA(I,J,k,bi,bj) = ZERO
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
           HSNOW(I,J,bi,bj)=0.2*AREA(i,j,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial snow thickness from file if available.
       IF ( HsnowFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL READ_FLD_XY_RL( HsnowFile, ' ', ZETA, 0, myThid )
        _END_MASTER(myThid)
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSNOW(I,J,bi,bj) = MAX(ZETA(i,j,bi,bj),ZERO)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

#ifdef SEAICE_SALINITY
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           HSALT(I,J,bi,bj)=HEFF(I,J,1,bi,bj)*
     &            ICE2WATR*rhoConstFresh*SEAICE_salinity
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea ice salinity from file if available.
       IF ( HsaltFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL READ_FLD_XY_RL( HsaltFile, ' ', ZETA, 0, myThid )
        _END_MASTER(myThid)
        _EXCH_XY_R8(ZETA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSALT(I,J,bi,bj) = ZETA(i,j,bi,bj)
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
          ZETA(I,J,bi,bj)=HEFF(I,J,1,bi,bj)*(1.0 _d 11)
          ETA(I,J,bi,bj)=ZETA(I,J,bi,bj)/4.0 _d 0
          PRESS0(I,J,bi,bj)=PSTAR*HEFF(I,J,1,bi,bj)
     &         *EXP(-20.0 _d 0*(ONE-AREA(I,J,1,bi,bj)))
          ZMAX(I,J,bi,bj)=(5.0 _d +12/(2.0 _d +04))*PRESS0(I,J,bi,bj)
          ZMIN(I,J,bi,bj)=SEAICE_zetaMin
          PRESS0(I,J,bi,bj)=PRESS0(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
        IF ( useRealFreshWaterFlux .AND. .NOT.useThSIce ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           sIceLoad(i,j,bi,bj) = HEFF(I,J,1,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(I,J,bi,bj)* 330. _d 0
          
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
