C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_main.F,v 1.6 2002/09/16 18:11:58 heimbach Exp $

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
c     continued&finished: heimbach@mit.edu: 13-Jun-2001
c     changed: mlosch@ocean.mit.edu: 09-May-2002
c              - added centered difference vs. 1-sided difference option
c              - improved output format for readability
c              - added control variable hFacC
c     
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
      _RL     fcpertplus, fcpertminus
      _RL     ratio_ad
      _RL     ratio_ftl
      _RL     xxmemo_ref
      _RL     xxmemo_pert
      _RL     adxxmemo
      _RL     ftlxxmemo
      _RL     localEps
      _RL     grdchk_epsfac

#ifdef ALLOW_TANGENTLINEAR_RUN
      _RL     g_fc
      common /g_cost_r/ g_fc
#endif

      _RL tmpplot1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpplot2(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpplot3(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

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
c--   Gradient via adjoint has already been computed,
c--   and so has unperturbed cost function,
c--   assuming all xx_ fields are initialised to zero.

      fcref = fc
      ierr_grdchk = 0

      do bj = jtlo, jthi
         do bi = itlo, ithi
            do k = 1, nr
               do j = jmin, jmax
                  do i = imin, imax
                     tmpplot1(i,j,k,bi,bj) = 0. _d 0
                     tmpplot2(i,j,k,bi,bj) = 0. _d 0
                     tmpplot3(i,j,k,bi,bj) = 0. _d 0
                  end do
               end do
            end do
         end do
      end do

      if ( useCentralDiff ) then
         grdchk_epsfac = 2. _d 0
      else
         grdchk_epsfac = 1. _d 0
      end if

c--   Compute the finite difference approximations.
c--   Cycle through all processes doing NINT(nend-nbeg+1)/nstep
c--   gradient checks.
      do jprocs = 1,numberOfProcs
         proc_grdchk = jprocs - 1

         if ( myProcId .eq. proc_grdchk ) then

            do icomp = nbeg, nend, nstep

               ichknum = (icomp - nbeg)/nstep + 1

               if (ichknum .le. maxgrdchecks ) then

c--   Determine the location of icomp on the grid.
                  call grdchk_loc( icomp, ichknum, 
     &                 icvrec, itile, jtile, layer,
     &                 itilepos, jtilepos, itest, ierr,
     &                 mythid )
              
                  if ( ierr .eq. 0 ) then

c******************************************************
c--   (A): get gradient component calculated via adjoint
c******************************************************
                     call grdchk_getadxx( icvrec,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    adxxmemo, mythid )
                     _BARRIER

#ifdef ALLOW_TANGENTLINEAR_RUN
c******************************************************
c--   (B): Get gradient component g_fc from tangent linear run:
c******************************************************
c--
c--   1. perturb control vector component: xx(i)=1.

                     localEps = 1.
                     call grdchk_getxx( icvrec, TANGENT_SIMULATION,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, xxmemo_pert, localEps,
     &                    mythid )
                     _BARRIER

c--
c--   2. perform tangent linear run
                     mytime = starttime
                     myiter = niter0
                     g_fc = 0.
                     call g_the_main_loop( mytime, myiter, mythid )
                     ftlxxmemo = g_fc
c--
c--   3. reset control vector
                     call grdchk_setxx( icvrec, TANGENT_SIMULATION,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, mythid )
                     _BARRIER
#endif

c******************************************************
c--   (C): Get gradient via finite difference perturbation
c******************************************************

c--   get control vector component from file
c--   perturb it and write back to file:
c--   positive perturbation
                     localEps = abs(grdchk_eps)
                     call grdchk_getxx( icvrec, FORWARD_SIMULATION,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, xxmemo_pert, localEps,
     &                    mythid )
                     _BARRIER
                     
c--   forward run with perturbed control vector
                     mytime = starttime
                     myiter = niter0
                     call the_main_loop( mytime, myiter, mythid )
                     fcpertplus = fc
                  
c--   Reset control vector.
                     call grdchk_setxx( icvrec, FORWARD_SIMULATION,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, mythid )
                     _BARRIER

                     fcpertminus = fcref

                     if ( useCentralDiff ) then

c--   get control vector component from file
c--   perturb it and write back to file:
c--   repeat the proceedure for a negative perturbation
                        localEps = - abs(grdchk_eps)
                        call grdchk_getxx( icvrec, FORWARD_SIMULATION,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, xxmemo_pert, localEps,
     &                    mythid )
                        _BARRIER
                     
c--   forward run with perturbed control vector
                        mytime = starttime
                        myiter = niter0
                        call the_main_loop( mytime, myiter, mythid )
                        fcpertminus = fc
                  
c--   Reset control vector.
                        call grdchk_setxx( icvrec, FORWARD_SIMULATION,
     &                    itile, jtile, layer,
     &                    itilepos, jtilepos,
     &                    xxmemo_ref, mythid )
                        _BARRIER

                     end if
c--
c******************************************************
c--   (D): calculate relative differences between gradients
c******************************************************

                     if ( grdchk_eps .eq. 0. ) then
                        gfd = (fcpertplus-fcpertminus)
                     else
                        gfd = (fcpertplus-fcpertminus)
     &                       /(grdchk_epsfac*grdchk_eps)
                     endif

                     if ( adxxmemo .eq. 0. ) then
                        ratio_ad = abs( adxxmemo - gfd )
                     else
                        ratio_ad = 1. - gfd/adxxmemo
                     endif
                  
                     if ( ftlxxmemo .eq. 0. ) then
                        ratio_ftl = abs( ftlxxmemo - gfd )
                     else
                        ratio_ftl = 1. - gfd/ftlxxmemo
                     endif
                  
                     tmpplot1(itilepos,jtilepos,layer,itile,jtile) =
     &                    gfd
                     tmpplot2(itilepos,jtilepos,layer,itile,jtile) =
     &                    ratio_ad
                     tmpplot3(itilepos,jtilepos,layer,itile,jtile) =
     &                    ratio_ftl

                     
                     fcrmem      ( ichknum ) = fcref
                     fcppmem     ( ichknum ) = fcpertplus
                     fcpmmem     ( ichknum ) = fcpertminus
                     xxmemref    ( ichknum ) = xxmemo_ref
                     xxmempert   ( ichknum ) = xxmemo_pert
                     gfdmem      ( ichknum ) = gfd
                     adxxmem     ( ichknum ) = adxxmemo
                     ftlxxmem    ( ichknum ) = ftlxxmemo
                     ratioadmem  ( ichknum ) = ratio_ad
                     ratioftlmem ( ichknum ) = ratio_ftl

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
                     print '(a,4I5,3(1x,E15.9))', 'ph-grd 3 ',
     &                    ichknum,itilepos,jtilepos,layer,
     &                    fcref, fcpertplus, fcpertminus
                     print '(a,4I5,3(1x,E15.9))', 'ph-grd 3 ',
     &                    ichknum,ichkmem(ichknum),
     &                    icompmem(ichknum),itestmem(ichknum),
     &                    adxxmemo, gfd, ratio_ad
                     print '(a,4I5,3(1x,E15.9))', 'ph-grd 3 ',
     &                    ichknum,ichkmem(ichknum),
     &                    icompmem(ichknum),itestmem(ichknum),
     &                    ftlxxmemo, gfd, ratio_ftl
cph)
                  else
c
                     print *, 'ph-grd 3 -------------------------------'
                     print *, 'ph-grd 3 : ierr = ', ierr, 
     &                                 ', icomp = ', icomp
                  endif
               else
                  ierr_grdchk = -1
               endif
            
            enddo
         endif

c--   Everyone has to wait for the component to be reset.
         _BARRIER

      enddo

      CALL WRITE_REC_XYZ_RL( 'grd_findiff'   , tmpplot1, 1, 0, myThid)
      CALL WRITE_REC_XYZ_RL( 'grd_ratio_ad'  , tmpplot2, 1, 0, myThid)
      CALL WRITE_REC_XYZ_RL( 'grd_ratio_ftl' , tmpplot3, 1, 0, myThid)

c--   Print the results of the gradient check.
      call grdchk_print( ichknum, ierr_grdchk, mythid )

#endif /* ALLOW_GRADIENT_CHECK */

      end
