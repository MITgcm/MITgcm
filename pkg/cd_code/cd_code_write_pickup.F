C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_write_pickup.F,v 1.4 2007/10/19 03:21:39 jmc Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CD_CODE_WRITE_PICKUP

C     !INTERFACE:
      SUBROUTINE CD_CODE_WRITE_PICKUP(
     I                         permPickup, suff,
     I                         myTime, myIter, myThid )

C     !DESCRIPTION:
C     Read the pickup.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CD_CODE_VARS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     permPickup :: Is or is not a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_CD_CODE

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

#ifdef ALLOW_MNC
      IF (useMNC .AND. pickup_write_mnc) THEN

        IF ( permPickup ) THEN
          WRITE(fn,'(A)') 'pickup_cd'
        ELSE
          WRITE(fn,'(A,A)') 'pickup_cd.',suff
        ENDIF

C       Read variables from the pickup file
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        IF ( permPickup ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'uVelD',uVelD, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'vVelD',vVelD, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Unm1',Unm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Vnm1',Vnm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'etaNm1', etaNm1, myThid)
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
      ENDIF
#endif /*  ALLOW_MNC  */

      IF (pickup_write_mdsio) THEN

        prec = precFloat64
        WRITE(fn,'(A,A)') 'pickup_cd.',suff

        CALL WRITE_REC_3D_RL( fn, prec, Nr, uVelD, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, Nr, vVelD, 2, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, Nr, uNM1,  3, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, Nr, vNM1,  4, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, 1,etaNm1,4*Nr+1,myIter,myThid)
      ENDIF

#endif /*  ALLOW_CD_CODE  */

      RETURN
      END
