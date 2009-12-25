C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_init.F,v 1.2 2009/12/25 19:43:43 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_INIT

C !INTERFACE:
      SUBROUTINE CPL_INIT

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_INIT
C     | o Routine controlling coupler layer initialisation.
C     | - Atmospheric version -
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "ATMIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
c     INTEGER myThid
CEOP

C !LOCAL VARIABLES:
C     == Local variables ==

C--   Initialise connection with coupler
#ifdef COMPONENT_MODULE
      CALL MITCOMPONENT_INIT( atmCompName, MPI_COMM_MODEL )
#endif

      RETURN
      END
