
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ICE_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE ICE_INIT                                      |
C     | o Set model ice parameters                          |
C     |==========================================================|
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
cswdice -- add ---
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#endif
cswdice -- end add --

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_RBCS
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
      INTEGER prec
      CHARACTER*(MAX_LEN_FNAM) fn

      _BARRIER

cswdice -- add ----
#ifdef ALLOW_THERM_SEAICE
cQQ   allowFreezing=.FALSE.
c     set up ice arrays to zero if starting ice
       if (startIceModel.eq.1) then
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO J=1,sNy
           DO I=1,sNx
            iceMask(i,j,bi,bj)=0.d0
            iceHeight(i,j,bi,bj)=0.d0
            snow(i,j,bi,bj)=0.d0
            Tsrf(i,j,bi,bj)=theta(i,j,1,bi,bj)
            Tice1(i,j,bi,bj)=0.d0
            Tice2(i,j,bi,bj)=0.d0
            Qice1(i,j,bi,bj)=0.d0
            Qice2(i,j,bi,bj)=0.d0
            sage(i,j,bi,bj)=0.d0
          ENDDO
         ENDDO
        ENDDO
        ENDDO
       else
C--     Read ice pickup fields
        prec=precFloat64
        WRITE(fn,'(A,I10.10)') 'pickup_ic.',nIter0
        CALL MDSREADFIELD(fn,prec,'RL',1,iceMask,   1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,iceHeight, 2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,snowHeight,3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Tsrf,      4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Tice1,     5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Tice2,     6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Qice1,     7,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Qice2,     8,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,sage,      9,myThid)
      endif

        _EXCH_XY_R8(iceMask, myThid)
        _EXCH_XY_R8(iceHeight, myThid)
        _EXCH_XY_R8(snowHeight, myThid)
        _EXCH_XY_R8(Tsrf, myThid)
        _EXCH_XY_R8(Tice1, myThid)
        _EXCH_XY_R8(Tice2, myThid)
        _EXCH_XY_R8(Qice1, myThid)
        _EXCH_XY_R8(Qice2, myThid)

#endif
 
cswdcou -- add ---
#ifdef COUPLE_MODEL
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO J=1,sNy
           DO I=1,sNx
            dFdT(i,j,bi,bj)=0.d0
           ENDDO
          ENDDO
        ENDDO
        ENDDO
        _EXCH_XY_R8(dFdT, myThid)
#endif
cswdcou -- end add ---


cswdice -------

      RETURN
      END
