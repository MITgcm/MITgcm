C $Header: /u/gcmpack/MITgcm/diags/src/Attic/timeaver_1fld_xyz.F,v 1.7 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_1FLD_XYZ( fld, fldtave, deltaT, 
     &                              bi, bj, K, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_1FLD_XYZ                             |
C     | o Sum over time a single array                           |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fld  - Input Field
C     fldtave - time averaged Field
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL deltaT
      INTEGER bi, bj, K, myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)=
     &       fldtave(i,j,k,bi,bj)+fld(i,j,k,bi,bj)*deltaT
           ENDDO
          ENDDO
C        ENDDO
C       ENDDO
C      ENDDO
 
      RETURN
      END
