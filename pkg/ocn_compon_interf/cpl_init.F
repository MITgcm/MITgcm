C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_init.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_INIT      
C     *==========================================================*
C     | SUBROUTINE CPL_INIT                                      |
C     | o Routine controlling coupler layer initialisation.      |
C     | - Oceanic version -                                      |
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "OCNIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==


C--   Initialise connection with coupler
#ifdef COMPONENT_MODULE
      CALL MITCOMPONENT_INIT( ocnCompName, MPI_COMM_MODEL )
#endif


      RETURN
      END
