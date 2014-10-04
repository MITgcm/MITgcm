C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_genread.F,v 1.3 2014/10/04 20:28:20 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_genread
C     !INTERFACE:
      subroutine cost_genread(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           nnzbar,
     I                           preproc,
     I                           myThid,
     I                           dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE cost_genread
C     ==================================================================
C     reads and pre-processes bar file records
C     ==================================================================
C     SUBROUTINE cost_genread
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER nnzbar
      _RL     dummy
      character*(16) preproc

#ifdef ALLOW_ECCO

c     == local variables ==

      _RL     fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL     fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      integer i,j,k,bi,bj
      integer iyear, imon
CEOP

      if ( preproc .EQ. '          ') then

        if ( nnzbar .EQ. 1 ) then
           call active_read_xy( active_var_file, active_var,
     &                      irec, doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      dummy )
        else
           call active_read_xyz( active_var_file, active_var,
     &                       irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       dummy )
        endif

      elseif ( preproc .EQ. 'climmon ') then

c--   Loop over month
         nyears=int((nmonsrec-irec)/12)+1

         call ecco_zero(fld1,nnzbar,zeroRL,myThid)

         do iyear=1,nyears
           imon=irec+(iyear-1)*12

           if ( nnzbar .EQ. 1 ) then
           call active_read_xy( active_var_file, fld2,
     &                      imon, doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      dummy )
           else
           call active_read_xyz( active_var_file, fld2,
     &                       imon, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       dummy )
           endif

           do bj = mybylo(mythid),mybyhi(mythid)
             do bi = mybxlo(mythid),mybxhi(mythid)
               do k = 1,nnzbar
                 do j = 1,sNy
                   do i = 1,sNx
                     fld1(i,j,k,bi,bj) =fld1(i,j,k,bi,bj)
     $                   +fld2(i,j,k,bi,bj)/float(nyears)
                   enddo
                 enddo
               enddo
             enddo
           enddo

         enddo

         do bj = mybylo(mythid),mybyhi(mythid)
           do bi = mybxlo(mythid),mybxhi(mythid)
             do k = 1,nnzbar
               do j = 1,sNy
                 do i = 1,sNx
                   active_var(i,j,k,bi,bj)=fld1(i,j,k,bi,bj)
                 enddo
               enddo
             enddo
           enddo
         enddo

      endif

#endif /* ALLOW_ECCO */

      RETURN
      END

