C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/aim_external_fields_load.F,v 1.1 2003/12/06 00:14:12 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE AIM_FIELDS_LOAD( 
     I            myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE AIM_FIELDS_LOAD
C     | o Control reading of AIM fields from external source.
C     *==========================================================*
C     | don't need any input fields for this experiment 
C     | since SST is computed from an analytic function. 
C     *==========================================================*
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter
CEndOfInterface
 
#ifdef ALLOW_AIM
C-    empty S/R

#endif /* ALLOW_AIM */

      RETURN 
      END
