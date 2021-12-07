#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

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
#ifdef ALLOW_EXF
# include "EXF_INTERP_SIZE.h"
#endif
#include "BLING_VARS.h"
#include "BLING_LOAD.h"

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

C First call requires that we initialize everything to zero for safety
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         BLING_ldRec(bi,bj) = 0
       ENDDO
      ENDDO
      CALL LEF_ZERO( dicwind0,myThid )
      CALL LEF_ZERO( dicwind1,myThid )
      CALL LEF_ZERO( atmosp0,myThid )
      CALL LEF_ZERO( atmosp1,myThid )
      CALL LEF_ZERO( silica0,myThid )
      CALL LEF_ZERO( silica1,myThid )
      CALL LEF_ZERO( ice0,myThid )
      CALL LEF_ZERO( ice1,myThid )
      CALL LEF_ZERO( feinput0,myThid )
      CALL LEF_ZERO( feinput1,myThid )

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

c set reasonable values to those that need at least something
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             apco2(i,j,bi,bj) = bling_pCO2*maskC(i,j,1,bi,bj)
             wind(i,j,bi,bj)     = 5. _d 0*maskC(i,j,1,bi,bj)
             atmosP(i,j,bi,bj)   = 1. _d 0*maskC(i,j,1,bi,bj)
             fIce(i,j,bi,bj)     = 0. _d 0
             fluxCO2(i,j,bi,bj)  = 0. _d 0
             InputFe(i,j,bi,bj)  = 1. _d -11*maskC(i,j,1,bi,bj)
             silica(i,j,bi,bj)   = 7.6838 _d -3*maskC(i,j,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
       IF ( apco2file .NE. ' ' ) THEN
      CALL EXF_INIT_FLD(
     I     'apco2', apco2file, apco2mask,
     I     apco2period, exf_inscal_apco2, apco2const,
     O     apco2, apco20, apco21,
# ifdef USE_EXF_INTERPOLATION
     I     apco2_lon0, apco2_lon_inc,
     I     apco2_lat0, apco2_lat_inc,
     I     apco2_nlon, apco2_nlat, xC, yC, apco2_interpMethod,
# endif
     I     myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_EXF */

#endif /* ALLOW_BLING */
      RETURN
      END
