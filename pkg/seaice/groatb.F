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
       COMMON /SEAICE_BOUND_RL/
     &    MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE,
     &    SEAICE_EPS, SEAICE_EPS_SQ
      _RL SEAICE_EPS, SEAICE_EPS_SQ
      _RL MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL A22
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      INTEGER KOPEN
      _RL U1, V1, SPEED_SQ
      _RL HICE (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL UG   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

#ifdef SEAICE_MULTILEVEL
      INTEGER it
      _RL RK
      _RL HICEP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL FICEP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
#endif

C if this is not done before then do it here
      _EXCH_XY_R8(UWIND, myThid)
      _EXCH_XY_R8(VWIND, myThid)

c$taf loop = parallel
      DO bj=myByLo(myThid),myByHi(myThid)
c$taf loop = parallel
       DO bi=myBxLo(myThid),myBxHi(myThid)

C DETERMINE AMOUNT OF OPEN WATER AND ICE THICKNESS
        DO J=1,sNy
         DO I=1,sNx
          AREA(I,J,2,bi,bj)=MAX(A22,AREA(I,J,2,bi,bj))
          FHEFF(I,J,bi,bj)=0.0 _d 0
#ifdef SEAICE_MULTILEVEL
          FICEP(I,J)=0.0 _d 0
#endif
          HICE(I,J)=HEFF(I,J,2,bi,bj)/AREA(I,J,2,bi,bj)
         ENDDO
        ENDDO

C NOW DETERMINE MIXED LAYER TEMPERATURE
        DO J=1,sNy
         DO I=1,sNx
          TMIX(I,J,bi,bj)=theta(I,J,1,bi,bj)+273.16 _d +00
#ifdef SEAICE_DEBUG
          TMIX(I,J,bi,bj)=MAX(TMIX(I,J,bi,bj),271.2 _d +00)
#endif
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
          U1=0.25 _d 0*(UWIND(I+1,J+1,bi,bj)+UWIND(I+1,J,bi,bj)
     1      +UWIND(I,J+1,bi,bj)+UWIND(I,J,bi,bj))
          V1=0.25 _d 0*(VWIND(I+1,J+1,bi,bj)+VWIND(I+1,J,bi,bj)
     1      +VWIND(I,J+1,bi,bj)+VWIND(I,J,bi,bj))
          SPEED_SQ = U1**2 + V1**2
          IF ( SPEED_SQ .LE. SEAICE_EPS_SQ ) THEN
             UG(I,J)=SEAICE_EPS
          ELSE
             UG(I,J)=SQRT(SPEED_SQ)
          ENDIF
         ENDDO
        ENDDO

C NOW DETERMINE GROWTH RATES
C FIRST DO OPEN WATER
        KOPEN=-1
        CALL BUDGET(UG, TMIX, HICE, FO, KOPEN, bi, bj)
C NOW DO ICE
        KOPEN=1
#ifdef SEAICE_MULTILEVEL
        DO IT=1,7
         DO J=1,sNy
          DO I=1,sNx
           RK=IT*1.0
           HICEP(I,J)=(HICE(I,J)/7.0 _d 0)*((2.0 _d 0*RK)-1.0 _d 0)
           TICE(I,J,bi,bj)=TICES(I,J,IT,bi,bj)
          ENDDO
         ENDDO
         CALL BUDGET(UG, TICE, HICEP, FICE, KOPEN, bi, bj)
         DO J=1,sNy
          DO I=1,sNx
           FICEP(I,J)=(FICE(I,J,bi,bj)/7.0 _d 0)+FICEP(I,J)
           TICES(I,J,IT,bi,bj)=TICE(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          FICE(I,J,bi,bj)=FICEP(I,J)
         ENDDO
        ENDDO
#else  /* SEAICE_MULTILEVEL */
        CALL BUDGET(UG, TICE, HICE, FICE, KOPEN, bi, bj)
#endif /* SEAICE_MULTILEVEL */

       ENDDO
      ENDDO

#endif /* ALLOW_SEAICE */

      RETURN
      END
