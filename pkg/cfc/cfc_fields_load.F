#include "GCHEM_OPTIONS.h"

CBOP
C     !ROUTINE: CFC_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE CFC_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CFC_FIELDS_LOAD
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CFC.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_SIZE.h"
# include "GCHEM_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime     :: current time in simulation
C     myIter     :: current iteration number
C     myThid     :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef GCHEM_ALLOW_FFIELDS
C     !LOCAL VARIABLES:
       INTEGER bi, bj, i, j
      _RL Pa2atm, wind2pisVel
CEOP

      Pa2Atm = 1.01325 _d 5
      wind2pisVel = 0.31 _d 0 / 3.6 _d 5

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fIce(i,j,bi,bj)    = gchemIce(i,j,bi,bj)
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c following Wannikof (1992)
          pisVel(i,j,bi,bj)=wind2pisVel*gchemWind(i,j,bi,bj)**2
C     Atm pressure in Pascals, convert to atm
          AtmosP(i,j,bi,bj)  = gchemAtmosp(i,j,bi,bj)/Pa2atm
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif /* GCHEM_ALLOW_FFIELDS */
      RETURN
      END
