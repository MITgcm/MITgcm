C $Header: /u/gcmpack/MITgcm/model/src/Attic/do_gterm_blocking_exchanges.F,v 1.2 1998/04/24 02:05:40 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE DO_GTERM_BLOCKING_EXCHANGES(myThid)
C     /==========================================================\
C     | SUBROUTINE DO_THE_MODEL_IO                               |
C     | o Controlling routine for IO in model main time-stepping |
C     |   loop.                                                  |
C     |==========================================================|
C     | Many systems do not have thread safe IO so it is easier  |
C     | to lump everything together and do dumping of fields     |
C     | and updating of forcing terms in a single place.         |
C     | The approach to IO used here is that writes are only     |
C     | performed by thread 1 and that a process only writes out |
C     | its data ( since it doen't know about anyone elses data!)|
C     | Reading on the other hand is assumed to be from a file   |
C     | containing all the data for all the processes. Only the  |
C     | portion of data of interest to this process is actually  |
C     | loaded. To work well this assumes the existence of some  |
C     | reliable tool to join datasets together at the end of a  |
C     | run.                                                     |
C     | Notes                                                    |
C     | =====                                                    |
C     | We allow thread 2-nThreads to continue whilst thread 1   |
C     | does IO. The assumption is that the other threads won't  |
C     | do anything to update their interior regions before      |
C     | thread 1 has finished writing them out.                  |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

      _EXCH_XYZ_R8( Gu , myThid )
      _EXCH_XYZ_R8( Gv , myThid )
      _EXCH_XYZ_R8( Gt , myThid )
CcnhDebugStarts
Cdbg  _EXCH_XYZ_R8( phSave , myThid )
CcnhDebugEnds

      RETURN
      END
 
