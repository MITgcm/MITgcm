C $Header: /u/gcmpack/MITgcm/verification/solid-body.cs-32x32x1/code/ini_psurf.F,v 1.1 2001/07/31 18:30:55 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_PSURF( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_PSURF                                     |
C     | o Set model initial free-surface height/pressure.        |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | surface displacement (r unit) field.                     |
C     |  1. Inline code                                          |
C     |  2. Two-dimensional data from a file.                    |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_PSURF 
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER  I,  J
      _RL omegaprime,fac

C--   Initialise surface position anomaly to zero
      omegaprime=38.60328935834681d0/rSphere
      fac=-(rSphere**2)*omegaprime*(Omega+omegaprime)/(4.d0*(Omega**2))
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaN(I,J,bi,bj) = 0. +fac*(fCori(i,j,bi,bj)**2)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Read an initial state
      IF (pSurfInitFile .NE. ' ') THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RL( pSurfInitFile, ' ', etaN, 0, myThid )
       _END_MASTER(myThid)
      ENDIF
C
      _EXCH_XY_R8(etaN, myThid)

#ifdef INCLUDE_CD_CODE
C--   By default, initialize etaNm1 with etaN : 
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaNm1(I,J,bi,bj) = etaN(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     _EXCH_XY_R8(etaNm1, myThid)
#endif

      RETURN
      END
