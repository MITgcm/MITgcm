C $Header:

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
#include "SEAICE_EXTERNAL.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE
C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      _RS  mask_uice
      CHARACTER*(MAX_LEN_FNAM) fn

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

C---  initialize grid info
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          TNGTICE(i,j,bi,bj)=tanPhiAtU(i,j,bi,bj)
          TNGICE(i,j,bi,bj) =tanPhiAtV(i,j+1,bi,bj)
          CSTICE(i,j,bi,bj) =cos(atan(tanPhiAtU(i,j,bi,bj)))
          CSUICE(i,j,bi,bj) =cos(atan(tanPhiAtV(i,j+1,bi,bj)))
          SINEICE(i,j,bi,bj)=sin(atan(tanPhiAtV(i,j+1,bi,bj)))
          DXTICE(i,j,bi,bj)=dxF(i,j,bi,bj)/CSTICE(i,j,bi,bj)
          DXUICE(i,j,bi,bj)=dxC(i+1,j,bi,bj)/CSUICE(i,j,bi,bj)
          DYTICE(i,j,bi,bj)=dyF(i,j,bi,bj)
          DYUICE(i,j,bi,bj)=dyC(i,j+1,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM(i,j,bi,bj)=ONE
          IF (_hFacC(i,j,1,bi,bj).eq.0.) HEFFM(i,j,bi,bj)=ZERO
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          UVM(i,j,bi,bj)=ZERO
          mask_uice=HEFFM(I,J,  bi,bj)+HEFFM(I+1,J+1,bi,bj)
     &             +HEFFM(I,J+1,bi,bj)+HEFFM(I+1,J,  bi,bj)
          IF(mask_uice.GT.3.5) UVM(I,J,bi,bj)=ONE
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          TICE(I,J,bi,bj)=273.0 _d 0
          UICEC(I,J,bi,bj)=ZERO
          VICEC(I,J,bi,bj)=ZERO
          AMASS(I,J,bi,bj)=1000.0 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlap regions
      _EXCH_XY_R8(UVM, myThid)
      _EXCH_XY_R8(TNGTICE, myThid)
      _EXCH_XY_R8(TNGICE, myThid)
      _EXCH_XY_R8(CSTICE, myThid)
      _EXCH_XY_R8(CSUICE, myThid)
      _EXCH_XY_R8(SINEICE, myThid)
      _EXCH_XY_R8(DXTICE, myThid)
      _EXCH_XY_R8(DXUICE, myThid)
      _EXCH_XY_R8(DYTICE, myThid)
      _EXCH_XY_R8(DYUICE, myThid)

C--   Set model variables to initial/restart conditions
      IF ( nIter0 .EQ. 0 ) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
               DO j=1-OLy,sNy+OLy
                  DO i=1-OLx,sNx+OLx
                     HSNOW(I,J,bi,bj)=0.2 _d 0
                     YNEG(I,J,bi,bj)=ZERO
                     TMIX(I,J,bi,bj)=TICE(I,J,bi,bj)
                     DO k=1,3
                        HEFF(I,J,k,bi,bj)=ONE
                        AREA(I,J,k,bi,bj)=HEFFM(i,j,bi,bj)
                        UICE(I,J,k,bi,bj)=ZERO
                        VICE(I,J,k,bi,bj)=ZERO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ELSE
         CALL SEAICE_READ_PICKUP ( myThid )
      ENDIF

C---  Complete initialization
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ZETA(I,J,bi,bj)=HEFF(I,J,1,bi,bj)*(1.0 _d 11)
          ETA(I,J,bi,bj)=ZETA(I,J,bi,bj)/4.0 _d 0
          surfaceTendencyTice(i,j,bi,bj) = ZERO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif ALLOW_SEAICE

      RETURN
      END
