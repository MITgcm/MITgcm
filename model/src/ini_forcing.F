C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.21 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_FORCING( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_FORCING                                   |
C     | o Set model initial forcing fields.                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FORCING
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER  I,  J

      _BARRIER

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fu              (i,j,bi,bj) = 0. _d 0
          fv              (i,j,bi,bj) = 0. _d 0
          Qnet            (i,j,bi,bj) = 0. _d 0
          EmPmR           (i,j,bi,bj) = 0. _d 0
          SST             (i,j,bi,bj) = 0. _d 0
          SSS             (i,j,bi,bj) = 0. _d 0
          Qsw             (i,j,bi,bj) = 0. _d 0
          surfaceTendencyU(i,j,bi,bj) = 0. _d 0
          surfaceTendencyV(i,j,bi,bj) = 0. _d 0
          surfaceTendencyT(i,j,bi,bj) = 0. _d 0
          surfaceTendencyS(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _BEGIN_MASTER(myThid)
      IF ( zonalWindFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( zonalWindFile, ' ', fu, 0, myThid )
      ENDIF
      IF ( meridWindFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( meridWindFile, ' ', fv, 0, myThid )
      ENDIF
      IF ( surfQFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( surfQFile, ' ', Qnet, 0, myThid )
      ENDIF
      IF ( EmPmRfile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( EmPmRfile, ' ', EmPmR, 0, myThid )
      ENDIF
      IF ( thetaClimFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( thetaClimFile, ' ', SST, 0, myThid )
      ENDIF
      IF ( saltClimFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( saltClimFile, ' ', SSS, 0, myThid )
      ENDIF
#ifdef SHORTWAVE_HEATING
      IF ( surfQswFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( surfQswFile, ' ', Qsw, 0, myThid )
      ENDIF
#endif
      _END_MASTER(myThid)
C
      _EXCH_XY_R4(fu   , myThid )
      _EXCH_XY_R4(fv   , myThid )
      _EXCH_XY_R4(Qnet , myThid )
      _EXCH_XY_R4(EmPmR, myThid )
      _EXCH_XY_R4(SST  , myThid )
      _EXCH_XY_R4(SSS  , myThid )
      _EXCH_XY_R4(Qsw  , myThid )

C     CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
C     CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

      RETURN
      END
