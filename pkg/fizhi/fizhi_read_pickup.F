C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_read_pickup.F,v 1.8 2004/10/07 00:06:09 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

CBOP
C     !ROUTINE: FIZHI_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE FIZHI_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIZHI_READ_PICKUP
C     | o Reads current fizhi state from a pickup file
C     *==========================================================*
C     \ev

C     !USES:
CEOP
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_io_comms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     == Routine Arguments ==
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     k       :: loop index
      INTEGER i, prec
      CHARACTER*(MAX_LEN_FNAM) fn
      _RL temp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)

      DO i = 1,MAX_LEN_FNAM
        fn(i:i) = ' '
      ENDDO
      WRITE(fn,'(A,I10.10)') 'pickup_fizhi.',myIter
      prec = precFloat64

C#ifdef ALLOW_MDSIO
      IF ( fizhi_mdsio_read_pickup ) THEN

C       Read fields as consecutive records
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,uphy,1,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,vphy,2,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,thphy,3,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,sphy,4,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldtot_lw,5,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldras_lw,6,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldlsp_lw,7,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldtot_sw,8,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldras_sw,9,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldlsp_sw,10,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,qliqavelw,11,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,qliqavesw,12,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,fccavelw,13,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,fccavesw,14,myIter,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,temp,15,myIter,myThid)
       iras = nint(temp(1,1,1,1,1))
       nlwcld = nint(temp(2,1,1,1,1))
       nlwlz = nint(temp(3,1,1,1,1))
       nswcld = nint(temp(4,1,1,1,1))
       nswlz = nint(temp(5,1,1,1,1))
       imstturbsw = nint(temp(6,1,1,1,1))
       imstturblw = nint(temp(7,1,1,1,1))

      ENDIF

#ifdef ALLOW_MNC
      IF ( useMNC .and. fizhi_mnc_read_pickup ) THEN

        _BEGIN_MASTER(myThid)

        WRITE(fn,'(A,I10.10)') 'pickup_fizhi.',myIter

        CALL MNC_CW_RL_R('D',fn,0,0,'uphy',uphy,myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'vphy',vphy,myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'thphy',thphy,myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'sphy',sphy,myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'sphy',sphy,myThid)


        _END_MASTER()

      ENDIF
#endif

      RETURN
      END
