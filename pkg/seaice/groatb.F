C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE groatb( A22, myThid )
C     /==========================================================\
C     | SUBROUTINE groatb                                        |
C     | o Calculate ice growth                                   |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_FFIELDS.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL A22
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      INTEGER KOPEN
      _RL U1, V1

      _RL HICE (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL AR   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL UG   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

C if this is not done before then do it here
      _EXCH_XY_R8(GAIRX, myThid)
      _EXCH_XY_R8(GAIRY, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C DETERMINE AMOUNT OF OPEN WATER AND ICE THICKNESS
        DO J=1,sNy
         DO I=1,sNx
          AREA(I,J,2,bi,bj)=MAX(A22,AREA(I,J,2,bi,bj))
          FHEFF(I,J,bi,bj)=0.0
          HICE(I,J)=HEFF(I,J,2,bi,bj)/AREA(I,J,2,bi,bj)
         ENDDO
        ENDDO

C NOW DETERMINE MIXED LAYER TEMPERATURE
        DO J=1,sNy
         DO I=1,sNx
          TMIX(I,J,bi,bj)=theta(I,J,1,bi,bj)+273.16E+00
#ifdef SEAICE_DEBUG
          TMIX(I,J,bi,bj)=MAX(TMIX(I,J,bi,bj),271.2E+00)
#endif SEAICE_DEBUG
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
          U1=0.25*(GAIRX(I-1,J-1,bi,bj)+GAIRX(I-1,J,bi,bj)
     1      +GAIRX(I,J-1,bi,bj)+GAIRX(I,J,bi,bj))
          V1=0.25*(GAIRY(I-1,J-1,bi,bj)+GAIRY(I-1,J,bi,bj)
     1      +GAIRY(I,J-1,bi,bj)+GAIRY(I,J,bi,bj))
          UG(I,J)=SQRT(U1**2+V1**2)
         ENDDO
        ENDDO

C NOW DETERMINE GROWTH RATES
C FIRST DO OPEN WATER
        KOPEN=-1
        CALL BUDGET(UG, TMIX, HICE, FO, KOPEN, bi, bj)
C NOW DO ICE
        KOPEN=1
        CALL BUDGET(UG, TICE, HICE, FICE, KOPEN, bi, bj)

       ENDDO
      ENDDO

#endif ALLOW_SEAICE

      RETURN
      END
