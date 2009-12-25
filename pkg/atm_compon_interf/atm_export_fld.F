C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_export_fld.F,v 1.2 2009/12/25 19:44:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_EXPORT_FLD
C     !INTERFACE:
      SUBROUTINE ATM_EXPORT_FLD(
     I               atmFldName,
     U               atmFld, countTime,
     I               myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_EXPORT_FLD
C     | o Routine for exporting 1 atmos. field to coupling layer:
C     |   - compute the time-average (if needed)
C     |   - send the field to the coupler
C     |   - reset time and field to zero
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     | Note: requires arg. array "atmFld" to be shared
C     |       (in common block) to work in multi-threaded.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     atmFldName :: atmos. Field identificator name
C     atmFld     :: array containing the atmospheric Field to export
C     countTime  :: fractional integrated time
C     myThid     :: my Thread Id number
      CHARACTER*(*) atmFldName
      _RL     atmFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     countTime(nSx,nSy)
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     recipAvT  :: Temp. for holding reciprocal of averaging period.
C     i,j,bi,bj :: Loop counters
      _RL recipAvT
      INTEGER i,j,bi,bj

C     Convert time integrated heatflux to mean value ready for export.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( ABS(countTime(bi,bj) -1. _d 0).GT. 1. _d -12
     &     .AND. countTime(bi,bj) .NE. 0. ) THEN
         recipAvT = 1. _d 0/countTime(bi,bj)
         DO j=1,sNy
          DO i=1,sNx
           atmFld(i,j,bi,bj) = atmFld(i,j,bi,bj)*recipAvT
          ENDDO
         ENDDO
         WRITE(errorMessageUnit,'(3A,2I4,1PE15.8)')
     &     'ATM_EXPORT_FLD: ',atmFldName,' : 1-CountTime=',
     &                             bi,bj,1.-countTime(bi,bj)
        ENDIF
       ENDDO
      ENDDO

C     Send atmos. field to coupling layer.
      _BARRIER
      _BEGIN_MASTER( myThid )
      CALL COMPSEND_R8TILES(
     I              atmFldName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     I              atmFld )
      _END_MASTER( myThid )
      _BARRIER

C     Reset atmos. field & integrated time before accumulating again.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        countTime(bi,bj) = 0.
        DO j=1,sNy
         DO i=1,sNx
          atmFld(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* COMPONENT_MODULE */

      RETURN
      END
