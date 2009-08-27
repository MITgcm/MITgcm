C $Header: /u/gcmpack/MITgcm/pkg/land/land_write_pickup.F,v 1.6 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE LAND_WRITE_PICKUP( isperm, suff,
     I                              myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_WRITE_PICKUP
C     | o Writes current state of land package to a pickup file
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "LAND_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     isperm  :: flag for permanent or rolling checkpoint
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      LOGICAL isperm
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_LAND

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
c     INTEGER prec, iChar, lChar, k
      INTEGER prec, lChar, k
      CHARACTER*(MAX_LEN_FNAM) fn
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      lChar = ILNBLNK(suff)

      IF ( land_pickup_write_mdsio ) THEN

C--   Write fields as consecutive records
        WRITE(fn,'(A,A)') 'pickup_land.',suff(1:lChar)
        prec = precFloat64

        CALL WRITE_REC_3D_RL( fn, prec, land_nLev,
     &                        land_enthalp,  1, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, land_nLev,
     &                        land_groundW,  2, myIter, myThid )
        k=2*land_nLev
        CALL WRITE_REC_3D_RL( fn, prec, 1,
     &                        land_skinT,  k+1, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, 1,
     &                        land_hSnow,  k+2, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, prec, 1,
     &                        land_snowAge,k+3, myIter, myThid )

      ENDIF

#ifdef ALLOW_MNC
      IF ( land_pickup_write_mnc ) THEN

        DO k = 1,MAX_LEN_FNAM
          fn(k:k) = ' '
        ENDDO
        IF ( isperm ) THEN
          WRITE(fn,'(A)') 'pickup_land'
        ELSE
          WRITE(fn,'(A,A)') 'pickup_land.',suff(1:lChar)
        ENDIF
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        IF ( isperm ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)

        CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)

        CALL MNC_CW_RL_W('D',fn,0,0,
     &       'land_enthalp', land_enthalp, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,
     &       'land_groundW', land_groundW, myThid)

        CALL MNC_CW_RL_W('D',fn,0,0,
     &       'land_skinT', land_skinT, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,
     &       'land_hSnow', land_hSnow, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,
     &       'land_snAge', land_snowAge, myThid)

      ENDIF
#endif /*  ALLOW_MNC  */

#endif /* ALLOW_LAND */

      RETURN
      END
