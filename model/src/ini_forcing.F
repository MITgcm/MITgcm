C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.23 2002/07/31 16:38:31 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_FORCING
C     !INTERFACE:
      SUBROUTINE INI_FORCING( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_FORCING                                    
C     | o Set model initial forcing fields.                       
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FORCING
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER  I,  J
CEOP

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
#ifdef SHORTWAVE_HEATING          
          Qsw             (i,j,bi,bj) = 0. _d 0
#else
          Qsw             (1,1,1,1)   = 0. _d 0
#endif
#ifdef ATMOSPHERIC_LOADING
          pload           (i,j,bi,bj) = 0. _d 0
#else
          pload           (1,1,1,1)   = 0. _d 0
#endif
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
#ifdef ATMOSPHERIC_LOADING
      IF ( pLoadFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( pLoadFile, ' ', pload, 0, myThid )
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
#ifdef SHORTWAVE_HEATING
      _EXCH_XY_R4(Qsw  , myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_R4(pload  , myThid )
C     CALL PLOT_FIELD_XYRS( pload, 'S/R INI_FORCING pload',1,myThid)
#endif

C     CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
C     CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

      RETURN
      END
