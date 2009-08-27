C $Header: /u/gcmpack/MITgcm/pkg/land/land_read_pickup.F,v 1.6 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE LAND_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_READ_PICKUP
C     | o Reads current state of land model from a pickup file
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

C     == Routine Arguments ==
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_LAND

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     k       :: loop index
      INTEGER prec, k
      CHARACTER*(MAX_LEN_FNAM) fn
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.land_pickup_read_mnc ) THEN

      WRITE(fn,'(A,I10.10)') 'pickup_land.',myIter
      prec = precFloat64

      IF ( land_oldPickup ) THEN
C-    Read fields as consecutive records
        CALL READ_REC_3D_RL( fn, prec, land_nLev,
     &                       land_groundT, 1, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, land_nLev,
     &                       land_groundW, 2, myIter, myThid )

      ELSE

C-    Read fields as consecutive records
        CALL READ_REC_3D_RL( fn, prec, land_nLev,
     &                       land_enthalp, 1, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, land_nLev,
     &                       land_groundW, 2, myIter, myThid )
        k=2*land_nLev
        CALL READ_REC_3D_RL( fn, prec, 1,
     &                       land_skinT,  k+1, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, 1,
     &                       land_hSnow,  k+2, myIter, myThid )
        CALL READ_REC_3D_RL( fn, prec, 1,
     &                       land_snowAge,k+3, myIter, myThid )

      ENDIF

      ENDIF

#ifdef ALLOW_MNC
      IF ( land_pickup_read_mnc ) THEN

        DO k = 1,MAX_LEN_FNAM
          fn(k:k) = ' '
        ENDDO
        WRITE(fn,'(A)') 'pickup_land'
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)

        CALL MNC_CW_RL_R('D',fn,0,0,
     &       'land_enthalp', land_enthalp, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,
     &       'land_groundW', land_groundW, myThid)

        CALL MNC_CW_RL_R('D',fn,0,0,
     &       'land_skinT', land_skinT, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,
     &       'land_hSnow', land_hSnow, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,
     &       'land_snAge', land_snowAge, myThid)

      ENDIF
#endif

C- jmc: exchange is not really necessary for land model
C-      and presently exchange S/R cannot work for 3d array where 3rd dim <> Nr

#endif /* ALLOW_LAND */

      RETURN
      END
