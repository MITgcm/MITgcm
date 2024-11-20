#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_INI_FORCING( myThid )

C     ==========================================================
C     | subroutine bling_ini_forcing
C     | o Initialize forcing fields
C     ==========================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

c !LOCAL VARIABLES: ===================================================
       INTEGER bi,bj,i,j
#ifdef USE_BLING_V1
       CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

#ifdef USE_BLING_V1
#ifdef USE_SIBLING
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not have USE_SIBLING ',
     &       ' with USE_BLING_V1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#ifdef USE_BLING_DVM
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not have USE_BLING_DVM ',
     &       ' with USE_BLING_V1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#ifdef ADVECT_PHYTO
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not use ADVECT_PHYTO ',
     &       ' with USE_BLING_V1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#endif /* USE_BLING_V1 */

C Initialize to zero. Reasonable values are given in pkg/gchem
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             apco2(i,j,bi,bj)   = 0. _d 0 
             wind(i,j,bi,bj)    = 0. _d 0
             atmosP(i,j,bi,bj)  = 0. _d 0
             fIce(i,j,bi,bj)    = 0. _d 0
             fluxCO2(i,j,bi,bj) = 0. _d 0
             InputFe(i,j,bi,bj) = 0. _d 0
             Silica(i,j,bi,bj)  = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_BLING */
      RETURN
      END
