C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_monitor.F,v 1.6 2006/01/13 17:19:32 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXF_MONITOR

C     !INTERFACE:
      SUBROUTINE EXF_MONITOR(
     I     myIter, myTime, myThid )

C     !DESCRIPTION:
C     Print some statistics about input forcing fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"
#include "GRID.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif
#include "exf_fields.h"
#include "exf_clim_fields.h"

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_MONITOR

C       Print some statistics about input forcing fields
        CALL MON_SET_PREF('exf',myThid)
        CALL MON_PRINTSTATS_RL(1,ustress,'_ustress',
     &       maskH,maskH,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,vstress,'_vstress',
     &       maskH,maskH,rAs,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,hflux,'_hflux',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,sflux,'_sflux',
     &       maskH,maskH,rA ,drF,myThid)
#ifdef ALLOW_ATM_WIND
        CALL MON_PRINTSTATS_RL(1,uwind,'_uwind',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,vwind,'_vwind',
     &       maskH,maskH,rA ,drF,myThid)
#endif /* ALLOW_ATM_WIND */
#ifdef ALLOW_ATM_TEMP
        CALL MON_PRINTSTATS_RL(1,atemp,'_atemp',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,aqh,'_aqh',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,lwflux,'_lwflux',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,precip,'_precip',
     &       maskH,maskH,rA ,drF,myThid)
#endif /* ALLOW_ATM_TEMP */
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
        CALL MON_PRINTSTATS_RL(1,swflux,'_swflux',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
        CALL MON_PRINTSTATS_RL(1,evap,'_evap',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
        CALL MON_PRINTSTATS_RL(1,swdown,'_swdown',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,lwdown,'_lwdown',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ATMOSPHERIC_LOADING
        CALL MON_PRINTSTATS_RL(1,apressure,'_apressure',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#if defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE)
        CALL MON_PRINTSTATS_RL(1,runoff,'_runoff',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
        CALL MON_PRINTSTATS_RL(1,climsst,'_climsst',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
        CALL MON_PRINTSTATS_RL(1,climsss,'_climsss',
     &       maskH,maskH,rA ,drF,myThid)
#endif

#endif /* ALLOW_MONITOR */

      RETURN
      END
