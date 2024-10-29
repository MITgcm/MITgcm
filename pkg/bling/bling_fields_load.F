#include "BLING_OPTIONS.h"

CBOP
      SUBROUTINE BLING_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C     *========================================================*
C     | subroutine bling_fields_load
C     | o Provide forcing fields needed for CO2, O2 flux terms,
C     |   silica for pH calculation
C     *========================================================*

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_EXF.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_BLING

C !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
CEOP

C     copy gchem-forcing fields

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fIce(i,j,bi,bj)    = gchemIce(i,j,bi,bj)
          wind(i,j,bi,bj)    = gchemWind(i,j,bi,bj)
C     Atm pressure in Pascals, convert to atm
          AtmosP(i,j,bi,bj)  = gchemAtmosp(i,j,bi,bj)/Pa2atm
          silica(i,j,bi,bj)  = gchemSi(i,j,bi,bj)
c         PAR(i,j,bi,bj)     = gchemPAR(i,j,bi,bj)
          inputFe(i,j,bi,bj) = gchemFe(i,j,bi,bj)
          apCO2(i,j,bi,bj)   = gchemapCO2(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_BLING */
      RETURN
      END
