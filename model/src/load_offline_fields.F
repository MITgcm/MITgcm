C $Header: /u/gcmpack/MITgcm/model/src/Attic/load_offline_fields.F,v 1.3 1998/04/26 23:41:54 cnh Exp $

#include "CPP_EEOPTIONS.h"
 
CStartOfInterface
      SUBROUTINE LOAD_OFFLINE_FIELDS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE LOAD_OFFLINE_FIELDS                           |
C     | o Control reading of fields for offline experiment.      |
C     |==========================================================|
C     | Offline experiment flow field and other field loading    |
C     | routine. This routine is called every time we want to    |
C     | load a a set of offlinhe fields. The routine decides     |
C     | which fields to load and then reads them in.             |
C     | Notes                                                    |
C     | =====                                                    |
C     | Two-dimensional and three-dimensional I/O are handled in |
C     | the following way under MITgcmUV. A master thread        |
C     | performs I/O using system calls. This threads reads data |
C     | into a temporary buffer. At present the buffer is loaded |
C     | with the entire model domain. This is probably OK for now|
C     | Each thread then copies data from the buffer to the      |
C     | region of the proper array it is responsible for.        |
C     \==========================================================/
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation's time
C     myIter - Simulation's iteration number
      INTEGER myThid
      REAL    myTime
      INTEGER myIter
CEndOfInterface
 
C     === Local variables ===

      RETURN
      END
