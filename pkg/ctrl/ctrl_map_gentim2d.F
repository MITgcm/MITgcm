#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_MAP_GENTIM2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENTIM2D(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  CTRL_MAP_GENTIM2D
C     *=============================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_MYFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer bi,bj
      integer i,j
      integer iarr

      _RL xx_gentim2d_loc(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL LOCsumTile(nSx,nSy), LOCsumGlob, genprecipGloH

c     == external ==
      integer  ilnblnk
      external ilnblnk
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_GENTIM2D_CONTROL
C--   generic - user-defined control vars
      do iarr = 1, maxCtrlTim2D

       do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do j = 1-Oly,sNy+Oly
          do i = 1-Olx,sNx+Olx
           xx_gentim2d_loc(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo
C
       if (xx_gentim2d_weight(iarr).NE.' ')
     &  CALL CTRL_GET_GEN (
     I      xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     I      xx_gentim2d_startdate(1,iarr),
     I      xx_gentim2d_period(iarr),
     I      maskC,
     O      xx_gentim2d_loc,
     I      xx_gentim2d0(1-Olx,1-Oly,1,1,iarr),
     I      xx_gentim2d1(1-Olx,1-Oly,1,1,iarr),
     I      xx_gentim2d_dummy(iarr),
     I      zeroRL, zeroRL,
     I      wgentim2d(1-Olx,1-Oly,1,1,iarr),
     I      mytime, myiter, mythid )
C
       do bj=myByLo(myThid),myByHi(myThid)
        do bi=myBxLo(myThid),myBxHi(myThid)
         do j = 1,sNy
          do i = 1,sNx
           if (xx_gentim2d_cumsum(iarr)) then
            xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d(i,j,bi,bj,iarr)
     &           +xx_gentim2d_loc(i,j,bi,bj)
           else
            xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d_loc(i,j,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo
C
       if (xx_gentim2d_glosum(iarr)) then

        LOCsumGlob=0. _d 0
        do bj=myByLo(myThid),myByHi(myThid)
         do bi=myBxLo(myThid),myBxHi(myThid)
          LOCsumTile(bi,bj)=0. _d 0
          do j = 1,sNy
           do i = 1,sNx
            LOCsumTile(bi,bj)=LOCsumTile(bi,bj)+
     &           maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
     &           *xx_gentim2d(i,j,bi,bj,iarr)
           enddo
          enddo
         enddo
        enddo

        CALL GLOBAL_SUM_TILE_RL( LOCsumTile, LOCsumGlob, myThid )

        do bj = myByLo(myThid), myByHi(myThid)
         do bi = myBxLo(myThid), myBxHi(myThid)
          do j = 1-Oly,sNy+Oly
           do i = 1-Olx,sNx+Olx
            xx_gentim2d(i,j,bi,bj,iarr) =
     &           LOCsumGlob/globalArea*maskC(i,j,1,bi,bj)
           enddo
          enddo
         enddo
        enddo

        if (xx_gentim2d_file(iarr).EQ.'xx_gen_precip') then

         genprecipGloH=LOCsumGlob/globalArea
     &     *rhoConstFresh*recip_rhoConst*deltaTClock
         WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &     ' iter=', myiter, ' ; genprecipGloH= ', genprecipGloH
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )

        endif !if (xx_gentim2d_file(iarr).EQ.'xx_gen_precip') then

       endif !if (xx_gentim2d_glosum(iarr)) then

      enddo !DO iarr = 1, maxCtrlTim2D

#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
