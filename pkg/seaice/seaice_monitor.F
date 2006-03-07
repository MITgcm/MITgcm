C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_monitor.F,v 1.3 2006/03/07 05:04:38 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_MONITOR

C     !INTERFACE:
      SUBROUTINE SEAICE_MONITOR(
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
#include "SEAICE.h"

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_MONITOR
      INTEGER i,j,bi,bj
      _RL TMP_DIAG (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

C       Print some statistics about input forcing fields
        CALL MON_SET_PREF('seaice',myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=UICE(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
#ifdef SEAICE_CGRID
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_uice',
     &       maskW,maskW,rAw,drF,myThid)
#else
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_uice',
     &       UVM,UVM,rAz,drF,myThid)
#endif
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=VICE(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
#ifdef SEAICE_CGRID
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_vice',
     &       maskS,maskS,rAs,drF,myThid)
#else
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_vice',
     &       UVM,UVM,rAz,drF,myThid)
#endif
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=AREA(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_area',
     &       maskH,maskH,rA ,drF,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=HEFF(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_heff',
     &       maskH,maskH,rA ,drF,myThid)
#endif /* ALLOW_MONITOR */

      RETURN
      END
