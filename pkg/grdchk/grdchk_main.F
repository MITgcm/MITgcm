C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_main.F,v 1.3 2001/09/28 16:53:46 heimbach Exp $

#include "CPP_OPTIONS.h"

CBOI
C
C !TITLE: GRADIENT CHECK
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: gradient check package
c \bv
c Compare the gradients calculated by the adjoint model with
c finite difference approximations.
c
C     !CALLING SEQUENCE:
c
c the_model_main
c |
c |-- ctrl_unpack
c |-- adthe_main_loop          - unperturbed cost function and
c |-- ctrl_pack                  adjoint gradient are computed here
c |
c |-- grdchk_main
c     |
c     |-- grdchk_init
c     |-- do icomp=...        - loop over control vector elements
c         |
c         |-- grdchk_loc      - determine location of icomp on grid
c         |
c         |-- grdchk_getxx    - get control vector component from file
c         |                     perturb it and write back to file
c         |-- grdchk_getadxx  - get gradient component calculated 
c         |                     via adjoint
c         |-- the_main_loop   - forward run and cost evaluation
c         |                     with perturbed control vector element
c         |-- calculate ratio of adj. vs. finite difference gradient
c         |
c         |-- grdchk_setxx    - Reset control vector element
c         |
c         |-- grdchk_print    - print results
c \ev
CEOI

CBOP
C     !ROUTINE: grdchk_main
C     !INTERFACE:
      subroutine grdchk_main( mythid )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE grdchk_main
c     ==================================================================
c     o Compare the gradients calculated by the adjoint model with
c       finite difference approximations.
c     started: Christian Eckert eckert@mit.edu 24-Feb-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c     ==================================================================
c     SUBROUTINE grdchk_main
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "grdchk.h"
#include "cost.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid 

#ifdef ALLOW_GRADIENT_CHECK
C     !LOCAL VARIABLES:
c     == local variables ==
      integer myiter
      _RL     mytime 
      integer bi, itlo, ithi
      integer bj, jtlo, jthi
      integer i,  imin, imax
      integer j,  jmin, jmax
      integer k

      integer jprocs
      integer proc_grdchk
      integer icomp
      integer ichknum
      integer icvrec
      integer jtile
      integer itile
      integer layer
      integer itilepos
      integer jtilepos
      integer itest
      integer ierr
      integer ierr_grdchk
      _RL     gfd
      _RL     fcref
      _RL     fcpert
      _RL     ratio
      _RL     xxmemo_ref
      _RL     xxmemo_pert
      _RL     adxxmemo

cph(
      _RL tmpplot1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpplot2(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
cph)

c     == end of interface ==
CEOP

c--   Set the loop ranges.
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   initialise variables
      call grdchk_init( mythid )

c--   Compute the adjoint models' gradients.
c--   Compute the unperturbed cost function.
cph   Gradient via adjoint has already been computed,
cph   and so has unperturbed cost function,
cph   assuming all xx_ fields are initialised to zero.

      fcref = fc
      ierr_grdchk = 0

cph(
      do bj = jtlo, jthi
         do bi = itlo, ithi
            do k = 1, nr
               do j = jmin, jmax
                  do i = imin, imax
                     tmpplot1(i,j,k,bi,bj) = 0. _d 0
                     tmpplot2(i,j,k,bi,bj) = 0. _d 0
                  end do
               end do
            end do
         end do
      end do
cph)

c--   Compute the finite difference approximations.
c--   Cycle through all processes doing NINT(nend-nbeg+1)/nstep
c--   gradient checks.
      do jprocs = 1,numberOfProcs
         proc_grdchk = jprocs - 1

         if ( myProcId .eq. proc_grdchk ) then

            do icomp = nbeg, nend, nstep

               ichknum = (icomp - nbeg)/nstep + 1

               if (ichknum .le. maxgrdchecks ) then

c--         Determine the location of icomp on the grid.
                  call grdchk_loc( icomp, ichknum, 
     &                 icvrec, itile, jtile, layer,
     &                 itilepos, jtilepos, itest, ierr,
     &                 mythid )
              
                  if ( ierr .eq. 0 ) then

c--   get control vector component from file
c--   perturb it and write back to file
                     call grdchk_getxx( icvrec,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, xxmemo_pert, mythid )
                     _BARRIER
                     
c--   get gradient component calculated via adjoint
                     call grdchk_getadxx( icvrec,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    adxxmemo, mythid )
                     _BARRIER

c--   forward run with perturbed control vector
                     mytime = starttime
                     myiter = niter0
                     call the_main_loop( mytime, myiter, mythid )
                     fcpert = fc
                  
                     if ( grdchk_eps .eq. 0. ) then
                        gfd = (fcpert-fcref)
                     else
                        gfd = (fcpert-fcref)/grdchk_eps
                     endif
                     
                     if ( gfd .eq. 0. ) then
                        ratio = abs( adxxmemo - gfd )
                     else
                        ratio = 1. - adxxmemo/gfd
                     endif
                  
c--   Reset control vector.
                     call grdchk_setxx( icvrec,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, mythid )
                     _BARRIER

cph(
                     tmpplot1(itilepos,jtilepos,layer,itile,jtile) =
     &                    gfd
                     tmpplot2(itilepos,jtilepos,layer,itile,jtile) =
     &                    ratio
cph)

                     
                     fcpmem    ( ichknum ) = fcpert
                     xxmemref  ( ichknum ) = xxmemo_ref
                     xxmempert ( ichknum ) = xxmemo_pert
                     gfdmem    ( ichknum ) = gfd
                     adxxmem   ( ichknum ) = adxxmemo
                     ratiomem  ( ichknum ) = ratio

                     irecmem   ( ichknum ) = icvrec
                     bimem     ( ichknum ) = itile
                     bjmem     ( ichknum ) = jtile
                     ilocmem   ( ichknum ) = itilepos
                     jlocmem   ( ichknum ) = jtilepos
                     klocmem   ( ichknum ) = layer
                     icompmem  ( ichknum ) = icomp
                     ichkmem   ( ichknum ) = ichknum
                     itestmem  ( ichknum ) = itest
                     ierrmem   ( ichknum ) = ierr
                  
cph(
                     print *, 'ph-grd 3 -------------------------------'
                     print *, 'ph-grd 3 ',
     &                    ichknum,itilepos,jtilepos,layer,
     &                    fcref, fcpert
                     print *, 'ph-grd 3 ',
     &                    ichknum,ichkmem(ichknum),
     &                    icompmem(ichknum),itestmem(ichknum),
     &                    adxxmemo, gfd, ratio
cph)
                  else
c
                  endif
               else
                  ierr_grdchk = -1
               endif
            
            enddo
         endif

c--   Everyone has to wait for the component to be reset.
         _BARRIER

      enddo

cph(
      CALL WRITE_REC_XYZ_RL( 'grd_findiff' , tmpplot1, 1, 0, myThid)
      CALL WRITE_REC_XYZ_RL( 'grd_ratio' , tmpplot2, 1, 0, myThid)
cph)

c--   Print the results of the gradient check.
      call grdchk_print( ichknum, ierr_grdchk, mythid )

#endif /* ALLOW_GRADIENT_CHECK */

      end
