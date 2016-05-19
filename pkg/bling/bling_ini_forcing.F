C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_ini_forcing.F,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_INI_FORCING( myThid )

C     ==========================================================
C     | subroutine bling_ini_forcing
C     | o Initialize forcing fields
C     ==========================================================

      implicit none
      
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "BLING_LOAD.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

c !LOCAL VARIABLES: ===================================================
       INTEGER bi,bj,i,j
#if (defined (READ_PAR) && defined (USE_QSW))
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
#ifdef READ_PAR
      CALL LEF_ZERO( par0,myThid )
      CALL LEF_ZERO( par1,myThid )
#endif
      CALL LEF_ZERO( feinput0,myThid )
      CALL LEF_ZERO( feinput1,myThid )

#ifdef READ_PAR
#ifdef USE_QSW
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not use READ_PAR ',
     &       ' and USE_QSW'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#endif

c set reasonable values to those that need at least something
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
             WIND(i,j,bi,bj)   = 5. _d 0*maskC(i,j,1,bi,bj)
             AtmosP(i,j,bi,bj) = 1. _d 0*maskC(i,j,1,bi,bj)
             SILICA(i,j,bi,bj) = 7.6838 _d -3*maskC(i,j,1,bi,bj)
             FIce(i,j,bi,bj)   = 0. _d 0
             FluxCO2(i,j,bi,bj)= 0. _d 0
             InputFe(i,j,bi,bj)= 1. _d -11*maskC(i,j,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

c       total_atmos_carbon = 0. _d 0

#endif /* ALLOW_BLING */
      RETURN
      END
