C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ini_vars.F,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE THSICE_INI_VARS( myThid )
C     *==========================================================*
C     | SUBROUTINE THSICE_INI_VARS
C     | o initialize THermo_SeaICE variables
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "THSICE_PARAMS.h"
#include "THSICE.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_RBCS
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_THSICE
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
      INTEGER prec
      CHARACTER*(MAX_LEN_FNAM) fn


c     set up ice arrays to zero if starting ice
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO J=1-Oly,sNy+Oly
           DO I=1-Olx,sNx+Olx
            iceMask(i,j,bi,bj)=0. _d 0
            iceHeight(i,j,bi,bj)=0. _d 0
            snowHeight(i,j,bi,bj)=0. _d 0
            Tsrf(i,j,bi,bj)=theta(i,j,1,bi,bj)
            Tice1(i,j,bi,bj)=0. _d 0
            Tice2(i,j,bi,bj)=0. _d 0
            Qice1(i,j,bi,bj)=0. _d 0
            Qice2(i,j,bi,bj)=0. _d 0
            sage(i,j,bi,bj)=0. _d 0
#ifdef COUPLE_MODEL
            dFdT(i,j,bi,bj)=0. _d 0
#endif
            snow(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      IF ( startIceModel.NE.1 .AND. nIter0.NE.0 ) THEN
C--     Read ice pickup fields
       _BARRIER
        prec=precFloat64
        CALL THSICE_READ_CHECKPOINT( prec, nIter0, myThid )

        _EXCH_XY_R8(iceMask, myThid)
        _EXCH_XY_R8(iceHeight, myThid)
        _EXCH_XY_R8(snowHeight, myThid)
        _EXCH_XY_R8(Tsrf, myThid)
        _EXCH_XY_R8(Tice1, myThid)
        _EXCH_XY_R8(Tice2, myThid)
        _EXCH_XY_R8(Qice1, myThid)
        _EXCH_XY_R8(Qice2, myThid)
#ifdef COUPLE_MODEL
        _EXCH_XY_R8(dFdT, myThid)
#endif
      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
