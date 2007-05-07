C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_ini_forcing.F,v 1.7 2007/05/07 15:58:19 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_FORCING_INT

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_INI_FORCING( myThid )

C !DESCRIPTION:
c initializes forcing fields to zero
c (or to reasonable values)

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#include "DIC_BIOTIC.h"
#include "DIC_LOAD.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

c !LOCAL VARIABLES: ===================================================
       INTEGER bi,bj,i,j
       CHARACTER*(MAX_LEN_MBUF) msgBuf
c
C First call requires that we initialize everything to zero for safety
      CALL LEF_ZERO( wspeed0,myThid )
      CALL LEF_ZERO( wspeed1,myThid )
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
# ifdef ALLOW_FE
      CALL LEF_ZERO( feinput0,myThid )
      CALL LEF_ZERO( feinput1,myThid )
# endif

       _EXCH_XY_R4(wspeed0, myThid )
       _EXCH_XY_R4(wspeed1, myThid )
       _EXCH_XY_R4(atmosp0, myThid )
       _EXCH_XY_R4(atmosp1, myThid )
       _EXCH_XY_R4(silica0, myThid )
       _EXCH_XY_R4(silica1, myThid )
       _EXCH_XY_R4(ice0, myThid )
       _EXCH_XY_R4(ice1, myThid )
#ifdef READ_PAR
       _EXCH_XY_R4(par0, myThid )
       _EXCH_XY_R4(par1, myThid )
#endif
# ifdef ALLOW_FE
       _EXCH_XY_R4(feinput0, myThid )
       _EXCH_XY_R4(feinput1, myThid )
# endif

#ifdef READ_PAR
#ifdef USE_QSW
        WRITE(msgBuf,'(2A)')
     &       ' DIC_INI_FORCING: You can not use READ_PAR ',
     &       ' and USE_QSW'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R DIC_INI_FORCING'
#endif
#endif

c set reasonable values to those that need at least something
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
             WIND(i,j,bi,bj) = 5.d0*maskC(i,j,1,bi,bj)
             ATMOSP(i,j,bi,bj)   =1.d0*maskC(i,j,1,bi,bj)
             SILICA(i,j,bi,bj)   =7.6838e-3*maskC(i,j,1,bi,bj)
             FIce(i,j,bi,bj) =0.d0
#ifdef READ_PAR
             PAR(i,j,bi,bj) =100.d0*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_FE
             InputFe(i,j,bi,bj)  = 1e-11*maskC(i,j,1,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO


#endif
      RETURN
      END
