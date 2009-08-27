C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_read_pickup.F,v 1.15 2009/08/27 18:00:01 jmc Exp $
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

      IF ( fizhi_mdsio_read_pickup ) THEN

C     Read fields as consecutive records
       CALL READ_REC_3D_RL( fn,prec,Nrphys,uphy,      1,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,vphy,      2,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,thphy,     3,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,sphy,      4,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,cldtot_lw, 5,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,cldras_lw, 6,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,cldlsp_lw, 7,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,lwlz,      8,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,cldtot_sw, 9,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,cldras_sw,10,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,cldlsp_sw,11,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,swlz,     12,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,qliqavelw,13,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,qliqavesw,14,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,fccavelw, 15,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,fccavesw, 16,myIter,myThid )
       CALL READ_REC_3D_RL( fn,prec,Nrphys,temp,     17,myIter,myThid )

       do bj = 1,myByLo(myThid), myByHi(myThid)
       do bi = 1,myBxLo(myThid), myBxHi(myThid)
       do j = 1-OLy,sNy+OLy
       do i = 1-OLx,sNx+OLx
        raincon(i,j,bi,bj) = temp(i,j,1,bi,bj)
        rainlsp(i,j,bi,bj) = temp(i,j,2,bi,bj)
        snowfall(i,j,bi,bj) = temp(i,j,3,bi,bj)
       enddo
       enddo

       iras(bi,bj)       = NINT(temp(1,1,4,bi,bj))
       nlwcld(bi,bj)     = NINT(temp(2,1,4,bi,bj))
       nlwlz(bi,bj)      = NINT(temp(3,1,4,bi,bj))
       nswcld(bi,bj)     = NINT(temp(4,1,4,bi,bj))
       nswlz(bi,bj)      = NINT(temp(5,1,4,bi,bj))
       imstturbsw(bi,bj) = NINT(temp(6,1,4,bi,bj))
       imstturblw(bi,bj) = NINT(temp(7,1,4,bi,bj))

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
