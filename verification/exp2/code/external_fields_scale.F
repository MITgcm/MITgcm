
#include "CPP_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE EXTERNAL_FIELDS_SCALE( myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FIELDS_SCALE                         |
C     | o Rescale fields from external source           .        |
C     |==========================================================|
C     | Notes                                                    |
C     | Conversion of flux fields are described in FFIELDS.h     |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface
 
C     === Functions ===
      LOGICAL DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Local arrays ===

C     === Local variables ===
      INTEGER bi,bj,i,j

      _BARRIER

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
c     Zonal wind stress fu:
          fu(i,j,bi,bj) = fu(i,j,bi,bj)
     &           *horiVertRatio*recip_rhoNil*recip_dRf(1)
c     Meridional wind stress fv:
          fv(i,j,bi,bj) = fv(i,j,bi,bj)
     &           *horiVertRatio*recip_rhoNil*recip_dRf(1)
#ifdef USE_NATURAL_BCS
c     Freshwater flux EmPmR:
          EmPmR(i,j,bi,bj) = 
     &          -EmPmR(i,j,bi,bj)*recip_dRf(1)*salt(i,j,1,bi,bj)
#else 
c     Freshwater flux EmPmR:
          EmPmR(i,j,bi,bj) = 
     &          -EmPmR(i,j,bi,bj)*recip_dRf(1)*35.
#endif          
c     Net heat flux Qnet:
          Qnet(i,j,bi,bj) = 
     &        - Qnet(i,j,bi,bj)*recip_Cp*recip_rhoNil*recip_dRf(1)
#ifdef SHORTWAVE_HEATING
c     Short wave radiative flux Qsw:
          Qsw(i,j,bi,bj) = 
     &         - Qsw(i,j,bi,bj)*recip_Cp*recip_rhoNil*recip_dRf(1)
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
