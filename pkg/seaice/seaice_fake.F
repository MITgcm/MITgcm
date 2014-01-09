C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_fake.F,v 1.3 2014/01/09 19:36:27 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

C     StartOfInterface
      SUBROUTINE SEAICE_FAKE( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE seaice_fake (for adjoint purpose only)        |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
# include "EXF_PARAM.h"
#endif
C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid
C     EndOfInterface(global-font-lock-mode 1)

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL fac, tempFrz
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(2A)') 'SEAICE_FAKE:',
     &  ' forward code is not meant to be used (adj only)'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R SEAICE_FAKE'

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
c shielding effect
            fac=MIN(1. _d 0, MAX(0. _d 0 , 1. _d 0 - area(i,j,bi,bj)))
            fu(i,j,bi,bj) = fu(i,j,bi,bj) * fac
            fv(i,j,bi,bj) = fv(i,j,bi,bj) * fac
            qnet(i,j,bi,bj) = qnet(i,j,bi,bj) * fac
            qsw(i,j,bi,bj) = qsw(i,j,bi,bj) * fac
#if (defined ALLOW_EXF) && (defined ALLOW_ATM_TEMP)
c the fresh water flux at the top of the ice
            EmPmR(i,j,bi,bj)  = maskC(i,j,1,bi,bj)*(
     &         fac * EVAP(i,j,bi,bj)
     &             - PRECIP(i,j,bi,bj)
#ifdef ALLOW_RUNOFF
     &             - RUNOFF(i,j,bi,bj)
#endif /* ALLOW_RUNOFF */
     &         )*rhoConstFresh
#endif
c relaxation to freezing point
            fac=MIN(1. _d 0, MAX(0. _d 0 , area(i,j,bi,bj)))
            tempFrz = SEAICE_tempFrz0 +
     &                SEAICE_dTempFrz_dS *salt(i,j,1,bi,bj)
            theta(i,j,1,bi,bj)=theta(i,j,1,bi,bj) + fac *
     &        ( tempFrz-theta(i,j,1,bi,bj) ) *
     &        SEAICE_mcPheePiston/drF(1)*SEAICE_deltaTtherm
         ENDDO
        ENDDO
       ENDDO
      ENDDO

       CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
       _EXCH_XY_RS( qnet,  myThid )
       _EXCH_XY_RS( qsw,  myThid )
       _EXCH_XY_RS( empmr,  myThid )

      RETURN
      END
