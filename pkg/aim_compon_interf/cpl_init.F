C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_init.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_INIT      
C     *==========================================================*
C     | SUBROUTINE CPL_INIT                                      |
C     | o Routine controlling coupler layer initialisation.      |
C     | - Atmospheric version -                                  |
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "ATMIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C--   Initialise connection with coupler
#ifdef COMPONENT_MODULE
      CALL MITCOMPONENT_INIT( atmCompName, MPI_COMM_MODEL )
#endif


      RETURN
      END
