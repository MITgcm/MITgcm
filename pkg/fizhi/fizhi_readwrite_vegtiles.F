C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_readwrite_vegtiles.F,v 1.4 2004/08/18 15:55:21 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
CBOP
C     !ROUTINE: FIZHI_WRITE_VEGTILES 
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_VEGTILES(fn,pickupflg,myTime,myIter,myThid)

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "EEPARAMS.h"
      EXTERNAL ILNBLNK
      INTEGER ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER pickupflg
      _RL myTime
      INTEGER myIter
      INTEGER myThid

CEOP
C     !LOCAL VARIABLES:
      CHARACTER*1 prec
      CHARACTER*80 bnam
      integer ilst
      integer i

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO i = 1,80
        bnam(i:i) = ' '
      ENDDO
      ilst = ILNBLNK(fn)
      if(pickupflg.eq.0) then
       prec = 'D'
       WRITE(bnam,'(a,a)') 'pickup_vegtiles.', fn(1:ilst)
      else
       prec = 'D'
       WRITE(bnam,'(a,a)') 'state_vegtiles.', fn(1:ilst)
      endif

      
#ifdef ALLOW_MNC
      IF (useMNC) THEN

C       Write fizhi veg-space variables using the MNC package
        CALL MNC_CW_SET_UDIM(bnam, 1, myThid)
        CALL MNC_CW_I_W('I',bnam,0,0,'iter', myIter, myThid)

C       fizhi_coms.h
        CALL MNC_CW_RL_W(prec,bnam,0,0,'ctmt', ctmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'xxmt', xxmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'yymt', yymt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'zetamt', zetamt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'xlmt', xlmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'khmt', khmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'tke', tke, myThid)

C       fizhi_land_coms.h
        CALL MNC_CW_RL_W(prec,bnam,0,0,'tcanopy', tcanopy, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'tdeep', tdeep, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'ecanopy', ecanopy, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'swetshal', swetshal, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'swetroot', swetroot, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'swetdeep', swetdeep, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'snodep', snodep, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'capac', capac, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'chlt', chlt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'chlon', chlon, myThid)
        CALL MNC_CW_RL_W('I',bnam,0,0,'igrd', igrd, myThid)

C       fizhi_earth_coms.h
        CALL MNC_CW_RL_W('I',bnam,0,0,'ityp', ityp, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'chfr', chfr, myThid)

      ENDIF
#endif /*  ALLOW_MNC  */

      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: FIZHI_READ_VEGTILES
C     !INTERFACE:
      SUBROUTINE FIZHI_READ_VEGTILES(fn,prec,myTime,myIter,myThid)

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "EEPARAMS.h"
      EXTERNAL ILNBLNK
      INTEGER ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*1 prec
      _RL myTime
      INTEGER myIter
      INTEGER myThid

CEOP
C     !LOCAL VARIABLES:
      CHARACTER*80 bnam
      integer ilst
      integer i

      DO i = 1,80
        bnam(i:i) = ' '
      ENDDO
      ilst = ILNBLNK(fn)
      WRITE(bnam,'(a,a)') 'pickup_vegtiles.', fn(1:ilst)
      
#ifdef ALLOW_MNC
      IF (useMNC) THEN

C       Write fizhi veg-space variables using the MNC package
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(bnam, 1, myThid)

C       fizhi_coms.h
        CALL MNC_CW_RL_R(prec,bnam,0,0,'ctmt', ctmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'xxmt', xxmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'yymt', yymt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'zetamt', zetamt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'xlmt', xlmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'khmt', khmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'tke', tke, myThid)

C       fizhi_land_coms.h
        CALL MNC_CW_RL_R(prec,bnam,0,0,'tcanopy', tcanopy, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'tdeep', tdeep, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'ecanopy', ecanopy, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'swetshal', swetshal, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'swetroot', swetroot, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'swetdeep', swetdeep, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'snodep', snodep, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'capac', capac, myThid)

      ENDIF
#endif /*  ALLOW_MNC  */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
