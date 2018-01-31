#include "CTRL_OPTIONS.h"
#include "STREAMICE_OPTIONS.h"

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
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_MYFIELDS.h"
#endif
#ifdef ALLOW_STREAMICE
#include "STREAMICE.h"
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
      character*( 80)   fnamegeneric

      _RL fac
      _RL xx_gentim2d_loc(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL genweight(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

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

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
           bdot_streamice(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      
      DO iarr = 1, maxCtrlTim2D
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
           xx_gentim2d_loc(I,J,bi,bj) = 0. _d 0
!           bdot_streamice(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C
!       CALL CTRL_GET_GEN (
!     I      xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
!     I      xx_gentim2d_startdate(1,iarr),
!     I      xx_gentim2d_period(iarr),
!     I      maskC,
!     O      xx_gentim2d_loc,
!     I      xx_gentim2d0(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d1(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d_dummy(iarr),
!     I      zeroRL, zeroRL,
!     I      mytime, myiter, mythid )

!       CALL CTRL_GET_GEN (
!     I      xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
!     I      xx_gentim2d_startdate(1,iarr),
!     I      xx_gentim2d_period(iarr),
!     I      maskC,
!     O      xx_gentim2d_loc,
!     I      xx_gentim2d0(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d1(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d_dummy(iarr),
!     I      zeroRL, zeroRL, genweight,
!     I      mytime, myiter, mythid )

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



       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         do j = 1,sNy
          do i =  1,sNx
           xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d_loc(i,j,bi,bj)
          enddo
         enddo
        enddo
       enddo


C


       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1,sNy
          DO I = 1,sNx
               if ( iarr .eq. 1 ) then


                bdot_streamice(i,j,bi,bj) =
     &           bdot_streamice(i,j,bi,bj)+
     &            xx_gentim2d(i,j,bi,bj,iarr)

               endif
          enddo
         enddo
        enddo
       enddo
c--
!      CALL EXCH_XY_RL 
!     & (streamice_v_shear_pert, myThid)
!      CALL EXCH_XY_RL 
!     & (streamice_u_shear_pert, myThid)
!      CALL EXCH_XY_RL 
!     & (streamice_v_normal_pert, myThid)
!      CALL EXCH_XY_RL 
!     & (streamice_u_normal_pert, myThid)

       _EXCH_XY_RL(bdot_streamice, mythid )
c--
      ENDDO ! iarr


#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
