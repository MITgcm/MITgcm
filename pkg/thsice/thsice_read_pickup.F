#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE THSICE_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_READ_PICKUP
C     | o Read thsice pickup file
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     == Local variables ==
      INTEGER prec
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn

      IF ( .NOT. thSIce_pickup_write_mnc ) THEN

        prec = precFloat64
        IF ( pickupSuff.EQ.' ' ) THEN
          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(fn,'(A,I10.10)') 'pickup_ic.', myIter
          ELSE
            CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
            WRITE(fn,'(A,A)') 'pickup_ic.', suff
          ENDIF
        ELSE
          WRITE(fn,'(A,A10)')    'pickup_ic.', pickupSuff
        ENDIF
        CALL READ_REC_3D_RL( fn, prec, 1, iceMask,  1, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, iceHeight,2, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1,snowHeight,3, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, Tsrf,     4, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, Tice1,    5, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, Tice2,    6, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, Qice1,    7, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, Qice2,    8, myIter,myThid )
        CALL READ_REC_3D_RL( fn, prec, 1, snowAge,  9, myIter,myThid )

        IF ( stepFwd_oceMxL .AND. startIceModel.EQ.0 )  THEN
          CALL READ_REC_3D_RL( fn,prec,1, tOceMxL, 10, myIter,myThid )
          CALL READ_REC_3D_RL( fn,prec,1, sOceMxL, 11, myIter,myThid )
        ENDIF

      ENDIF

#ifdef ALLOW_MNC
      IF ( thSIce_pickup_write_mnc ) THEN
        WRITE(fn,'(A)') 'pickup_ic'
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'iceMask',   iceMask,   myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'iceHeight', iceHeight, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'snowHeight',snowHeight,myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Tsrf',      Tsrf,      myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Tice1',     Tice1,     myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Tice2',     Tice1,     myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Qice1',     Qice1,     myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Qice2',     Qice2,     myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'snowAge',   snowAge,   myThid)
        IF ( stepFwd_oceMxL ) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'tOceMxL',tOceMxL,myThid)
          CALL MNC_CW_RL_R('D',fn,0,0,'sOceMxL',sOceMxL,myThid)
        ENDIF
      ENDIF
#endif  /* ALLOW_MNC */

#endif  /* ALLOW_THSICE */
      RETURN
      END
