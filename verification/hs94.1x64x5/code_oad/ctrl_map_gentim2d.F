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
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_GENARR.h"
#include "CTRL_DUMMY.h"
#include "OPTIMCYCLE.h"
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
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il
      integer iarr

      logical equal
      logical doglobalread
      logical ladinit
      character*(MAX_LEN_FNAM) fnamebase

      _RL fac
      _RL xx_gentim2d_loc(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL LOCsumTile(nSx,nSy), LOCsumGlob

c     == external ==
      integer  ilnblnk
      external ilnblnk
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_GENTIM2D_CONTROL
C--   An example of connecting specific fields
C--   to generic time-varying 2D control arrays
cph--->>>
cph--->>> COMPILE FAILURE IS DELIBERATE
cph--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cph--->>>
C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlTim2D

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
           xx_gentim2d_loc(I,J,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C
       fnamebase = xx_gentim2d_file(iarr)
       CALL CTRL_GET_GEN (
     I      xx_gentim2d_file(iarr),
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
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
            do j = 1,sNy
              do i =  1,sNx
              if (xx_gentim2d_cumsum(iarr)) then
                xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d(i,j,bi,bj,iarr)
     &            +xx_gentim2d_loc(i,j,bi,bj)
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
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          LOCsumTile(bi,bj)=0. _d 0
            do j = 1,sNy
              do i =  1,sNx
                LOCsumTile(bi,bj)=LOCsumTile(bi,bj)+
     &            maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
     &            *xx_gentim2d(i,j,bi,bj,iarr)
              enddo
            enddo
        enddo
      enddo

      CALL GLOBAL_SUM_TILE_RL( LOCsumTile, LOCsumGlob, myThid )

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
             xx_gentim2d(I,J,bi,bj,iarr) =
     &            LOCsumGlob/globalArea*maskC(i,j,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

         WRITE(msgBuf,'(A,I6,A,I6,A,1PE21.14)') ' xx_gentim2d ',
     &    iarr,' : iter=', myiter, ' ; global sum = ', LOCsumGlob
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )

       endif

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
           if (iarr.EQ.1) then
             theta(I,J,1,bi,bj) = theta(I,J,1,bi,bj)
     &                          + xx_gentim2d(I,J,bi,bj,iarr)
           endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDDO

#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
