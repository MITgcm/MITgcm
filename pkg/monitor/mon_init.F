C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_init.F,v 1.12 2012/12/21 01:00:08 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_INIT

C     !INTERFACE:
      SUBROUTINE MON_INIT( myThid )

C     !DESCRIPTION:
C     Set default monitor internal setup.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
c     CHARACTER*(MAX_LEN_MBUF) msgbuf

C     Since monitor does not have a "data" file of its own, all its
C     output flags are set in "model/src/ini_parms.F".  Please see the
C     IO section of that file for the monitor flags.

C     Set monitor I/O to standard output by default
C     and prefix monitor "variables" with mon by default.
      CALL MON_SET_IOUNIT( standardMessageUnit, myThid )
      CALL MON_SET_PREF  ( mon_string_none    , myThid )

      _BEGIN_MASTER(myThid)
C     Internal parameter for continuing when monitor_solution would
C     otherwise decide to stop the code.
c     mon_overrideStop=.FALSE.
      monSolutionMaxRange = 1. _d +4
      IF ( fluidIsWater ) monSolutionMaxRange = 1. _d +3
      mon_output_AM = fluidIsAir .AND. useCoriolis
     &                           .AND. selectCoriMap.GE.2
      _END_MASTER(myThid)

      _BARRIER

      RETURN
      END
