#include "CPP_OPTIONS.h"
#include "ICEFRONT_OPTIONS.h"

      subroutine SGRUNOFF_READ(
     I            myTime, myIter, myThid)
c     |==================================================================|
c     | SUBROUTINE sgrunoff_read                                         |
c     |==================================================================|
c     | read 2-D subglacial runoff data from files,                      |
c     |      and fill the lowest layer of 3-D addmass variable           |
c     |==================================================================|

      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ICEFRONT.h"
#include "DYNVARS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif

c     == routine arguments ==

      _RL     myTime
      integer myIter
      integer myThid

#if (defined(ALLOW_ICEFRONT) && defined(ALLOW_SUBGLACIAL_RUNOFF))
C     !LOCAL VARIABLES:
C     === Local arrays ===
C     msgBuf       :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf,fname
      _RL SGrunoff (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rdt
      integer Iprd,Icyc,iRec

       IF ( myIter .EQ. nIter0 ) THEN
        WRITE(msgBuf,'(1X,A,I10,1P1E20.12)')
     &       'SGRUNOFF_READ: loading initial data:',
     &       myIter, myTime
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid)

        IF ( SGrunoffFile .NE. ' '  ) THEN
         _BARRIER
          CALL READ_REC_XY_RS( SGrunoffFile, SGrunoff,
     &                      1, myIter, myThid )
         _EXCH_XY_RS( SGrunoff, myThid )

         CALL SGRUNOFF_FILL_ADDMASS(addmass,SGrunoff,myThid)

        ENDIF

       ELSEIF(SGRUNOFF_period.GT.0.0) THEN
        rdt = 1. _d 0 / deltaTclock
        Iprd = NINT(SGRUNOFF_Period*rdt)
        Icyc = NINT(SGRUNOFF_Cycle*rdt)
        IF (MOD(myIter+Iprd-1,Iprd) .EQ. 0) THEN
         iRec=1+INT(MOD(myIter-1,Icyc)/Iprd)
         _BARRIER
         CALL READ_REC_XY_RS( SGrunoffFile, SGrunoff,
     &                      iRec, myIter, myThid )
         _EXCH_XY_RS( SGrunoff, myThid )
         CALL SGRUNOFF_FILL_ADDMASS(addmass,SGrunoff,myThid)
        ENDIF

C      endif myIter .EQ. nIter0
       ENDIF

      RETURN
      END

CBOP
C     !ROUTINE: SGRUNOFF_FILL_ADDMASS
C     !INTERFACE:
      SUBROUTINE SGRUNOFF_FILL_ADDMASS(
     O     addmass,
     I     SGrunoff, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SGRUNOFF_FILL_ADDMASS
C     | o Fill addmass with SGrunoff
C     *==========================================================*
C     \ev 

C     !USES: 
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid
      _RL SGrunoff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL addMass (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     bi,bj,I,J,K :: loop counters
      INTEGER bi,bj,I,J,K
CEOP
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
C ??? if there is another source of addmass ???
           K = kLowC(I,J,bi,bj)
           addmass(I,J,K,bi,bj) = SGrunoff(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_ICEFRONT and ALLOW_SUBGLACIAL_RUNOFF */
      RETURN
      END

