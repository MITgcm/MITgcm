
#include "CPP_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_SURF_U( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FORCING_SURF_U                       |
C     | o Determines forcing terms based on external fields      |
C     |   relaxation terms etc.                                  |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
      INTEGER iMin, iMax, jMin, jMax, bi, bj
CEndOfInterface
 
C     === Functions ===

C     === Local arrays ===

C     === Local variables ===
      INTEGER i,j

c     Zonal wind stress fu:
       DO j=jMin,jMax
        DO i=iMin,iMax
          surfaceTendencyU(i,j,bi,bj) = fu(i,j,bi,bj)
        ENDDO
       ENDDO

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_SURF_V( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FORCING_SURF_V                       |
C     | o Determines forcing terms based on external fields      |
C     |   relaxation terms etc.                                  |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
      INTEGER iMin, iMax, jMin, jMax, bi, bj
CEndOfInterface
 
C     === Functions ===

C     === Local arrays ===

C     === Local variables ===
      INTEGER i,j

c     Zonal wind stress fv:
       DO j=jMin,jMax
        DO i=iMin,iMax
          surfaceTendencyV(i,j,bi,bj) = fV(i,j,bi,bj)
        ENDDO
       ENDDO

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_SURF_T( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FORCING_SURF_T                       |
C     | o Determines forcing terms based on external fields      |
C     |   relaxation terms etc.                                  |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
      INTEGER iMin, iMax, jMin, jMax, bi, bj
CEndOfInterface
 
C     === Functions ===

C     === Local arrays ===

C     === Local variables ===
      INTEGER i,j

c     Net heat flux Qnet:
       DO j=jMin,jMax
        DO i=iMin,iMax
          surfaceTendencyT(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &         - lambdaThetaClimRelax*
     &           (theta(i,j,1,bi,bj)-SST(i,j,bi,bj))
        ENDDO
       ENDDO

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_SURF_S( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FORCING_SURF_S                       |
C     | o Determines forcing terms based on external fields      |
C     |   relaxation terms etc.                                  |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
      INTEGER iMin, iMax, jMin, jMax, bi, bj
CEndOfInterface
 
C     === Functions ===

C     === Local arrays ===

C     === Local variables ===
      INTEGER i,j

c     Net heat flux Qnet:
       DO j=jMin,jMax
        DO i=iMin,iMax
#ifdef USE_NATURAL_BCS
c     Freshwater flux EmPmR:
          surfaceTendencyS(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &         - lambdaSaltClimRelax*
     &           (salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
#else 
c     Freshwater flux EmPmR:
          surfaceTendencyS(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &         - lambdaSaltClimRelax*
     &           (salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
#endif          
        ENDDO
       ENDDO

      RETURN
      END
