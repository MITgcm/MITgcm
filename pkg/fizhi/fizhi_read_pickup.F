C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_read_pickup.F,v 1.14 2004/12/14 16:54:08 edhill Exp $
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
      integer j,bi,bj

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
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldtot_lw,5,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldras_lw,6,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldlsp_lw,7,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,lwlz,8,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldtot_sw,9,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldras_sw,10,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,cldlsp_sw,11,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,swlz,12,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,qliqavelw,13,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,qliqavesw,14,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,fccavelw,15,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,fccavesw,16,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',Nrphys,temp,17,myThid)

       do bj = 1,myByLo(myThid), myByHi(myThid)
       do bi = 1,myBxLo(myThid), myBxHi(myThid)
       do j = 1-OLy,sNy+OLy
       do i = 1-OLx,sNx+OLx
        raincon(i,j,bi,bj) = temp(i,j,1,bi,bj)
        rainlsp(i,j,bi,bj) = temp(i,j,2,bi,bj)
        snowfall(i,j,bi,bj) = temp(i,j,3,bi,bj)
       enddo
       enddo

       iras(bi,bj)       = nint(temp(1,1,4,bi,bj))
       nlwcld(bi,bj)     = nint(temp(2,1,4,bi,bj))
       nlwlz(bi,bj)      = nint(temp(3,1,4,bi,bj))
       nswcld(bi,bj)     = nint(temp(4,1,4,bi,bj))
       nswlz(bi,bj)      = nint(temp(5,1,4,bi,bj))
       imstturbsw(bi,bj) = nint(temp(6,1,4,bi,bj))
       imstturblw(bi,bj) = nint(temp(7,1,4,bi,bj))

       enddo
       enddo

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


        _END_MASTER(myThid)

      ENDIF
#endif

      RETURN
      END
