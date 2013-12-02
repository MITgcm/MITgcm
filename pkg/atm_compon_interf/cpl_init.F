C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_init.F,v 1.3 2013/12/02 21:41:59 jmc Exp $
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
C     myThid :: Thread number for this instance of the routine
c     INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C !LOCAL VARIABLES:
C     msgUnit  :: log-file I/O unit
      INTEGER msgUnit

C--   Initialise connection with coupler
      CALL MITCOMPONENT_INIT(
     I                  atmCompName,
     O                  MPI_COMM_MODEL, msgUnit )
#endif

      RETURN
      END
