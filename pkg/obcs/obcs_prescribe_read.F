C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_prescribe_read.F,v 1.29 2011/04/17 03:20:51 jmc Exp $
C $Name:  $

# include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_PRESCRIBE_READ
C     !INTERFACE:
      SUBROUTINE OBCS_PRESCRIBE_READ (
     I                          myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *============================================================*
C     | SUBROUTINE OBCS_PRESCRIBE_READ
C     *============================================================*
C     | read open boundary conditions from file                           
C     | N.B.: * uses exf and cal routines for file/record handling        
C     |       * uses ctrl routines for control variable handling          
C     *============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "OBCS.h"
c#ifdef ALLOW_EXF
c# include "EXF_PARAM.h"
c#endif
c#ifdef ALLOW_PTRACERS
c# include "PTRACERS_SIZE.h"
c# include "OBCS_PTRACERS.h"
c#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OBCS_PRESCRIBE

C     !LOCAL VARIABLES:
CEOP

# ifdef ALLOW_EXF
      IF ( useEXF ) THEN
        CALL OBCS_EXF_LOAD( myTime, myIter, myThid )
      ENDIF
# endif /* ALLOW_EXF */

      IF ( .NOT. useEXF ) THEN
cph#ifndef ALLOW_AUTODIFF_TAMC
       CALL OBCS_FIELDS_LOAD( myTime, myIter, myThid )
cph#else
cph       STOP 'PH HAS DISABLED THIS RUNTIME OPTION FOR ALLOW_EXF'
cph#endif
      ENDIF

# ifdef ALLOW_OBCSN_CONTROL
      CALL CTRL_GETOBCSN ( myTime, myIter, mythid )
# endif

# ifdef ALLOW_OBCSS_CONTROL
      CALL CTRL_GETOBCSS ( myTime, myIter, mythid )
# endif

# ifdef ALLOW_OBCSW_CONTROL
      CALL CTRL_GETOBCSW ( myTime, myIter, myThid )
# endif

# ifdef ALLOW_OBCSE_CONTROL
      CALL CTRL_GETOBCSE ( myTime, myIter, myThid )
# endif

#endif /* ALLOW_OBCS_PRESCRIBE */

      RETURN
      END
