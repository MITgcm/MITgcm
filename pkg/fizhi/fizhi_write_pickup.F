C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_write_pickup.F,v 1.10 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
CBOP
C     !ROUTINE: FIZHI_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_PICKUP( suff, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIZHI_WRITE_PICKUP
C     | o Writes current state of fizhi package to a pickup file
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
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
      INTEGER prec, iChara, lChar
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf
      _RL temp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      integer i,j,k,bi,bj

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      do bj = 1,myByLo(myThid), myByHi(myThid)
      do bi = 1,myBxLo(myThid), myBxHi(myThid)
      do k = 1,Nrphys
      do j = 1-OLy,sNy+OLy
      do i = 1-OLx,sNx+OLx
       temp(i,j,k,bi,bj) = 0.
      enddo
      enddo
      enddo
      enddo
      enddo

      lChar = 0
      DO iChara = 1,len(suff)
       IF ( suff(iChara:iChara) .NE. ' ') lChar=iChara
      ENDDO
      WRITE(fn,'(A,A)') 'pickup_fizhi.',suff(1:lChar)
      prec = precFloat64
      lgf = globalFiles

C--   Write fields as consecutive records
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,uphy,      1,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,vphy,      2,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,thphy,     3,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,sphy,      4,myIter,myThid)

      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,cldtot_lw, 5,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,cldras_lw, 6,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,cldlsp_lw, 7,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,lwlz,      8,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,cldtot_sw, 9,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,cldras_sw,10,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,cldlsp_sw,11,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,swlz,     12,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,qliqavelw,13,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,qliqavesw,14,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,fccavelw, 15,myIter,myThid)
      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,fccavesw, 16,myIter,myThid)

      do bj = myByLo(myThid), myByHi(myThid)
      do bi = myBxLo(myThid), myBxHi(myThid)
      do j = 1,sNy
      do i = 1,sNx
       temp(i,j,1,bi,bj) = raincon(i,j,bi,bj)
       temp(i,j,2,bi,bj) = rainlsp(i,j,bi,bj)
       temp(i,j,3,bi,bj) = snowfall(i,j,bi,bj)
      enddo
      enddo

      temp(1,1,4,bi,bj) = float(iras(bi,bj))
      temp(2,1,4,bi,bj) = float(nlwcld(bi,bj))
      temp(3,1,4,bi,bj) = float(nlwlz(bi,bj))
      temp(4,1,4,bi,bj) = float(nswcld(bi,bj))
      temp(5,1,4,bi,bj) = float(nswlz(bi,bj))
      temp(6,1,4,bi,bj) = float(imstturbsw(bi,bj))
      temp(7,1,4,bi,bj) = float(imstturblw(bi,bj))

      enddo
      enddo

      CALL WRITE_REC_3D_RL(fn,prec,Nrphys,temp,17,myIter,myThid)

      RETURN
      END
