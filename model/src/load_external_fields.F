C $Header: /u/gcmpack/MITgcm/model/src/Attic/load_external_fields.F,v 1.1 1998/06/15 05:13:56 cnh Exp $

#include "CPP_EEOPTIONS.h"
 
CStartOfInterface
      SUBROUTINE LOAD_EXTERNAL_FIELDS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE LOAD_EXTERNAL_FIELDS                          |
C     | o Control reading of fields from external source.        |
C     |==========================================================|
C     | External source field loading routine.                   |
C     | This routine is called every time we want to             |
C     | load a a set of external fields. The routine decides     |
C     | which fields to load and then reads them in.             |
C     | This routine needs to be customised for particular       |
C     | experiments.                                             |
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

C     Simple example of loading a climatology at the
C     first time step that is the used in a relaxation term
C     in the surface layer.
      IF ( myTime .EQ. startTime ) THEN
       CALL LOAD_THETA_CLIMATOLOGY( myThid )
       CALL LOAD_SALT_CLIMATOLOGY( myThid )
      ENDIF

      RETURN
      END

CStartOfInterface
      SUBROUTINE LOAD_SALT_CLIMATOLOGY( myThid )
C     /==========================================================\
C     | S/R LOAD_SALT_CLIMATOLOGY                                |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

C     === Local variables ===
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) msgBuf

      IF ( thetaClimFile .EQ. ' ' ) THEN
C--    Check that relaxation term is off
       IF ( doSaltClimRelax ) THEN
        WRITE(msgBuf,'(A)')  'S/R LOAD_SALT_CLIMATOLOGY'
        CALL PRINT_ERROR( msgBuf, 1 )
        WRITE(msgBuf,'(A)')  'No salt climatology for relaxation specified.'
        CALL PRINT_ERROR( msgBuf, 1 )
        STOP 'ABNORMAL END: S/R LOAD_SALT_CLIMATOLOGY'
       ENDIF
      ELSE
C--    Load value from file
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RS( saltClimFile, ' ', saltClim, 0, myThid )
       _END_MASTER(   myThid )
      ENDIF

      _EXCH_XYZ_R4( saltClim, myThid )

C
      CALL PLOT_FIELD_XYZRS( saltClim, 'Salt Climatology' , Nz, 1, myThid )
C

      RETURN
      END

CStartOfInterface
      SUBROUTINE LOAD_THETA_CLIMATOLOGY( myThid )
C     /==========================================================\
C     | S/R LOAD_THETA_CLIMATOLOGY                               |
C     \==========================================================?

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

C     === Local variables ===
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) msgBuf

      IF ( thetaClimFile .EQ. ' ' ) THEN
C--    Check that relaxation term is off
       IF ( doThetaClimRelax ) THEN
        WRITE(msgBuf,'(A)')  'S/R LOAD_THETA_CLIMATOLOGY'
        CALL PRINT_ERROR( msgBuf, 1 )
        WRITE(msgBuf,'(A)')  'No theta climatology for relaxation specified.'
        CALL PRINT_ERROR( msgBuf, 1 )
        STOP 'ABNORMAL END: S/R LOAD_THETA_CLIMATOLOGY'
       ENDIF
      ELSE
C--    Load value from file
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RS( thetaClimFile, ' ', thetaClim, 0, myThid )
       _END_MASTER(   myThid )
      ENDIF

      _EXCH_XYZ_R4( thetaClim, myThid )

C
      CALL PLOT_FIELD_XYZRS( thetaClim, 'Theta Climatology' , Nz, 1, myThid )
C

      RETURN
      END
