C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_init_fixed.F,v 1.3 2008/04/08 03:01:55 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_INIT_FIXED( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_INIT_FIXED                             |
C     | o Initialization of sea ice model.                       |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE.h"
CML#include "SEAICE_GRID.h"
#include "SEAICE_DIAGS.h"
#include "SEAICE_PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface
 
C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER k, bi, bj
cif(
cif   Helper variable for determining the fraction of sw radiation
cif   penetrating the model's shallowest layer
      INTEGER dummyIter
      _RL dummyTime
      _RL swfracba(2)
      _RL FACTORM
      INTEGER IMAX
cif)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL SEAICE_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

cif(
#ifdef SHORTWAVE_HEATING
      IMAX      = 2
      FACTORM   = -1.0
      dummyTime = 1.0
      dummyIter = 0
      swfracba(1) = abs(rF(1)) 
      swfracba(2) = abs(rF(2))
      CALL SWFRAC(
     I       IMAX,FACTORM,
     U       swfracba,
     I       dummyTime,dummyIter,myThid)
      SWFRACB = swfracba(2)
#endif
cif)

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET(FUtave   ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(FVtave   ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(EmPmRtave,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(QNETtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(QSWtave  ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(UICEtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(VICEtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(HEFFtave ,1,bi,bj,myThid)
        CALL TIMEAVE_RESET(AREAtave ,1,bi,bj,myThid)
        DO k=1,Nr
         SEAICE_TimeAve(k,bi,bj)=ZERO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

      RETURN
      END
