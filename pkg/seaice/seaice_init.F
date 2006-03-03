C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_init.F,v 1.29 2006/03/03 07:52:01 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_INIT                                   |
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
#include "SEAICE_GRID.h"
#include "SEAICE_DIAGS.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface
 
C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      _RS  mask_uice
      INTEGER myIter, myTile

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET(FUtave   ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(FVtave   ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(EmPmRtave,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(QNETtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(QSWtave  ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(UICEtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(VICEtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(HEFFtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(AREAtave ,1,bi,bj,myThid)
        DO k=1,Nr
         SEAICE_TimeAve(k,bi,bj)=ZERO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

cph(
cph   make sure TAF sees proper initialisation
cph   to avoid partial recomputation issues
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c
        DO K=1,3
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           HEFF(I,J,k,bi,bj)=ZERO
           AREA(I,J,k,bi,bj)=ZERO
           UICE(I,J,k,bi,bj)=ZERO
           VICE(I,J,k,bi,bj)=ZERO
          ENDDO
         ENDDO
        ENDDO
c
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          HSNOW(I,J,bi,bj)=ZERO
          ZETA(I,J,bi,bj)=ZERO
         ENDDO
        ENDDO
c
       ENDDO
      ENDDO
cph)

C--   Initialize grid info
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          CSTICE(i,j,bi,bj) =cos(yC(I,J,bi,bj)*deg2rad)
          CSUICE(i,j,bi,bj) =cos(yG(I,J,bi,bj)*deg2rad)
CML(
C     inverses of CSTICE and CSUICE. Lets hope we are never
C     at the poles
          IF ( CSTICE(I,J,bi,bj) .ne. 0. _d 0 ) THEN
           RECIP_CSTICE(I,J,bi,bj) = 1./CSTICE(I,J,bi,bj)
          ELSE	
           RECIP_CSTICE(I,J,bi,bj) =0. _d 0
          ENDIF
          IF ( CSUICE(I,J,bi,bj) .ne. 0. _d 0 ) THEN
           RECIP_CSUICE(I,J,bi,bj) = 1./CSUICE(I,J,bi,bj)
          ELSE	
           RECIP_CSUICE(I,J,bi,bj) =0. _d 0
          ENDIF
CML)
          SINEICE(i,j,bi,bj)=sin(yC(I,J,bi,bj)*deg2rad)
          TNGTICE(i,j,bi,bj)=SINEICE(i,j,bi,bj)*RECIP_CSTICE(i,j,bi,bj)
          SINEICE(i,j,bi,bj)=sin(yG(I,J,bi,bj)*deg2rad)
          TNGICE(i,j,bi,bj) =SINEICE(i,j,bi,bj)*RECIP_CSUICE(i,j,bi,bj)
          DXTICE(i,j,bi,bj)=dxF(i,j,bi,bj)*RECIP_CSTICE(i,j,bi,bj)
          DXUICE(i,j,bi,bj)=dxV(i,j,bi,bj)*RECIP_CSUICE(i,j,bi,bj)
          DYTICE(i,j,bi,bj)=dyF(i,j,bi,bj)
          DYUICE(i,j,bi,bj)=dyU(i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM(i,j,bi,bj)=ONE
          IF (_hFacC(i,j,1,bi,bj).eq.0.) HEFFM(i,j,bi,bj)=ZERO
         ENDDO
        ENDDO
        DO J=2-OLy,sNy+OLy
         DO I=2-OLx,sNx+OLx
          UVM(i,j,bi,bj)=ZERO
          mask_uice=HEFFM(I,J,  bi,bj)+HEFFM(I-1,J-1,bi,bj)
     &             +HEFFM(I,J-1,bi,bj)+HEFFM(I-1,J,  bi,bj)
          IF(mask_uice.GT.3.5) UVM(I,J,bi,bj)=ONE
         ENDDO
        ENDDO

#ifdef ALLOW_EXCH2
C--   Special stuff for cubed sphere: assume grid is rectangular and
C     set UV mask to zero except for Arctic and Antarctic cube faces.
        IF (useCubedSphereExchange) THEN
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           CSTICE(i,j,bi,bj) = ONE
           CSUICE(i,j,bi,bj) = ONE
CML(
           RECIP_CSTICE(I,J,bi,bj) = ONE
           RECIP_CSUICE(I,J,bi,bj) = ONE
CML)
           TNGTICE(i,j,bi,bj)= ZERO
           TNGICE(i,j,bi,bj) = ZERO
           DXTICE(i,j,bi,bj) = dxF(i,j,bi,bj)
           DXUICE(i,j,bi,bj) = dxV(i,j,bi,bj)
          ENDDO
         ENDDO
         myTile = W2_myTileList(bi)
         IF ( exch2_myFace(myTile) .EQ. 1 .OR.
     &        exch2_myFace(myTile) .EQ. 2 .OR.
     &        exch2_myFace(myTile) .EQ. 4 .OR.
     &        exch2_myFace(myTile) .EQ. 5 ) THEN
          DO J=1-OLy,sNy+OLy
           DO I=1-OLx,sNx+OLx
            UVM(i,j,bi,bj)=ZERO
           ENDDO
          ENDDO
         ELSEIF ( exch2_isWedge(myTile) .EQ. 1 ) THEN
          I=1
          DO J=1-OLy,sNy+OLy
           UVM(i,j,bi,bj)=ZERO
          ENDDO
         ELSEIF ( exch2_isSedge(myTile) .EQ. 1 ) THEN
          J=1
          DO I=1-OLx,sNx+OLx
           UVM(i,j,bi,bj)=ZERO
          ENDDO
         ENDIF
C--   Make sure that DXTICE and DYTICE do not contain any zeros
         DO J=1,sNy
          DO I=1-OLx,0
           IF(DXTICE(i,j,bi,bj).EQ.0)
     &            DXTICE(i,j,bi,bj)=DXTICE(1,j,bi,bj)
           IF(DYTICE(i,j,bi,bj).EQ.0)
     &            DYTICE(i,j,bi,bj)=DYTICE(1,j,bi,bj)
          ENDDO
          DO I=sNx+1,sNx+OLx
           IF(DXTICE(i,j,bi,bj).EQ.0)
     &            DXTICE(i,j,bi,bj)=DXTICE(sNx,j,bi,bj)
           IF(DYTICE(i,j,bi,bj).EQ.0)
     &            DYTICE(i,j,bi,bj)=DYTICE(sNx,j,bi,bj)
          ENDDO
         ENDDO
         DO J=1-OLy,0
          DO I=1-OLx,sNx+OLx
           IF(DXTICE(i,j,bi,bj).EQ.0)
     &            DXTICE(i,j,bi,bj)=DXTICE(i,1,bi,bj)
           IF(DYTICE(i,j,bi,bj).EQ.0)
     &            DYTICE(i,j,bi,bj)=DYTICE(i,1,bi,bj)
          ENDDO
         ENDDO
         DO J=sNy+1,sNy+OLy
          DO I=1-OLx,sNx+OLx
           IF(DXTICE(i,j,bi,bj).EQ.0)
     &            DXTICE(i,j,bi,bj)=DXTICE(i,sNy,bi,bj)
           IF(DYTICE(i,j,bi,bj).EQ.0)
     &            DYTICE(i,j,bi,bj)=DYTICE(i,sNy,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_EXCH2 */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          TICE(I,J,bi,bj)=273.0 _d 0
#ifdef SEAICE_MULTILEVEL
          DO k=1,MULTDIM
           TICES(I,J,k,bi,bj)=273.0 _d 0
          ENDDO
#endif
          UICEC(I,J,bi,bj)=ZERO
          VICEC(I,J,bi,bj)=ZERO
          AMASS(I,J,bi,bj)=1000.0 _d 0
          GWATX(I,J,bi,bj)=ZERO
          GWATY(I,J,bi,bj)=ZERO
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
      _EXCH_XY_R8(UVM, myThid)

C--   Now lets look at all these beasts
      IF ( debugLevel .GE. debLevB ) THEN
         myIter=0
         CALL PLOT_FIELD_XYRL( CSTICE  , 'Current CSTICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( CSUICE  , 'Current CSUICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( TNGTICE , 'Current TNGTICE ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( TNGICE  , 'Current TNGICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( SINEICE , 'Current SINEICE ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DXTICE  , 'Current DXTICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DXUICE  , 'Current DXUICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DYTICE  , 'Current DYTICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DYUICE  , 'Current DYUICE  ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( HEFFM   , 'Current HEFFM   ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( UVM     , 'Current UVM     ' ,
     &        myIter, myThid )
      ENDIF

C--   Set model variables to initial/restart conditions
      IF ( nIter0 .NE. 0 ) THEN

         CALL SEAICE_READ_PICKUP ( myThid )

      ELSE

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HSNOW(I,J,bi,bj)=0.2*HEFFM(i,j,bi,bj)
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
            IF(HEFF(I,J,k,bi,bj).GT.ZERO) AREA(I,J,k,bi,bj)=ONE
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
         
      ENDIF

C---  Complete initialization
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ZETA(I,J,bi,bj)=HEFF(I,J,1,bi,bj)*(1.0 _d 11)
          ETA(I,J,bi,bj)=ZETA(I,J,bi,bj)/4.0 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
