C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_read_pickup.F,v 1.3 2007/08/17 19:38:18 jmc Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CD_CODE_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE CD_CODE_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION:
C     Read the checkpoint.

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
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_CD_CODE

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec

C     Suffix for pickup files
      IF (pickupSuff.EQ.' ') THEN
         WRITE(suff,'(I10.10)') myIter
      ELSE
         WRITE(suff,'(A10)') pickupSuff
      ENDIF
      prec = precFloat64

      _BARRIER

#ifdef ALLOW_MNC
      IF (useMNC .AND. pickup_read_mnc) THEN
C       Read variables from the pickup file
        WRITE(fn,'(A)') 'pickup_cd'
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'uVelD',uVelD, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'vVelD',vVelD, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Unm1',Unm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Vnm1',Vnm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'etaNm1', etaNm1, myThid)
      ENDIF
#endif /*  ALLOW_MNC  */

      WRITE(fn,'(A,A10)') 'pickup_cd.',suff

      IF (pickup_read_mdsio) THEN
        CALL READ_REC_3D_RL( fn, prec, Nr, uVelD, 1, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, Nr, vVelD, 2, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, Nr, uNM1,  3, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, Nr, vNM1,  4, myIter, myThid )
        IF ( usePickupBeforeC54 ) THEN
          CALL READ_REC_3D_RL(fn,prec, 1, etaNm1,6*Nr+1,myIter,myThid)
        ELSE
          CALL READ_REC_3D_RL(fn,prec, 1, etaNm1,4*Nr+1,myIter,myThid)
        ENDIF
      ENDIF

C-    thread synchronisation (barrier) is part of the EXCH S/R calls
      CALL EXCH_UV_DGRID_3D_RL( uVelD,vVelD, .TRUE., Nr, myThid )
      CALL EXCH_UV_XYZ_RL( uNM1,vNM1, .TRUE., myThid )
      CALL EXCH_XY_RL( etaNm1, myThid )

#endif /*  ALLOW_CD_CODE  */

      RETURN
      END
