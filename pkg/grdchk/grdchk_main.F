C
C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_main.F,v 1.23 2007/08/04 18:05:23 heimbach Exp $
C $Name:  $

#include "AD_CONFIG.h"
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
c              heimbach@mit.edu 24-Feb-2003
c              - added tangent linear gradient checks
c              - fixes for multiproc. gradient checks
c              - added more control variables
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
#include "ctrl.h"
#ifdef ALLOW_TANGENTLINEAR_RUN
#include "g_cost.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid 

#ifdef ALLOW_GRDCHK
C     !LOCAL VARIABLES:
c     == local variables ==
      integer myiter
      _RL     mytime 
      integer bi, itlo, ithi
      integer bj, jtlo, jthi
      integer i,  imin, imax
      integer j,  jmin, jmax
      integer k

      integer icomp
      integer ichknum
      integer icvrec
      integer jtile
      integer itile
      integer layer
      integer obcspos
      integer itilepos
      integer jtilepos
      integer icglo
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

      _RL tmpplot1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpplot2(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpplot3(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      CHARACTER*(MAX_LEN_MBUF) msgBuf

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

      print *, 'ph-check entering grdchk_main '

c--   initialise variables
      call grdchk_init( mythid )

c--   Compute the adjoint models' gradients.
c--   Compute the unperturbed cost function.
cph   Gradient via adjoint has already been computed,
cph   and so has unperturbed cost function,
cph   assuming all xx_ fields are initialised to zero.

      ierr_grdchk = 0
#ifdef ALLOW_ADMTLM
      fcref = objf_state_final(idep,jdep,1,1,1)
#else
      fcref = fc
#endif

      print *, 'ph-check fcref = ', fcref

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

      WRITE(standardmessageunit,'(A)')
     &    'grad-res -------------------------------'
      WRITE(standardmessageunit,'(2a)')
     &    ' grad-res  proc    #    i    j    k   bi   bj iobc',
     &    '               fc ref           fc + eps           fc - eps'
#ifdef ALLOW_TANGENTLINEAR_RUN
      WRITE(standardmessageunit,'(2a)')
     &    ' grad-res  proc    #    i    j    k   bi   bj iobc',
     &    '             tlm grad            fd grad         1 - fd/tlm'
#else
      WRITE(standardmessageunit,'(2a)')
     &    ' grad-res  proc    #    i    j    k   bi   bj iobc',
     &    '             adj grad            fd grad         1 - fd/adj'
#endif

c--   Compute the finite difference approximations.
c--   Cycle through all processes doing NINT(nend-nbeg+1)/nstep
c--   gradient checks.

      if ( nbeg .EQ. 0 ) 
     &     call grdchk_get_position( mythid )

      do icomp = nbeg, nend, nstep

         ichknum = (icomp - nbeg)/nstep + 1

cph(
cph-print         print *, 'ph-grd _main: nbeg, icomp, ichknum ', 
cph-print     &        nbeg, icomp, ichknum
cph)
         if (ichknum .le. maxgrdchecks ) then

c--         Determine the location of icomp on the grid.
            if ( myProcId .EQ. grdchkwhichproc ) then
               call grdchk_loc( icomp, ichknum, 
     &              icvrec, itile, jtile, layer, obcspos,
     &              itilepos, jtilepos, icglo, itest, ierr,
     &              mythid )
cph(
cph-print               print *, 'ph-grd ----- back from loc -----',
cph-print     &             icvrec, itilepos, jtilepos, layer, obcspos
cph)
            endif
            _BARRIER
              
c******************************************************
c--   (A): get gradient component calculated via adjoint
c******************************************************

c--   get gradient component calculated via adjoint
            if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
               call grdchk_getadxx( icvrec,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              adxxmemo, mythid )
            endif
            _BARRIER

#ifdef ALLOW_TANGENTLINEAR_RUN
c******************************************************
c--   (B): Get gradient component g_fc from tangent linear run:
c******************************************************
c--
c--   1. perturb control vector component: xx(i)=1.
            ftlxxmemo = 0.

            if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
               localEps = 1. _d 0
               call grdchk_getxx( icvrec, TANGENT_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, xxmemo_pert, localEps,
     &              mythid )
            endif
            _BARRIER

c--
c--   2. perform tangent linear run
            mytime = starttime
            myiter = niter0
#ifdef ALLOW_ADMTLM
            do k=1,4*Nr+1
             do j=1,sny
              do i=1,snx
               g_objf_state_final(i,j,1,1,k) = 0.
              enddo
             enddo
            enddo
#else
            g_fc = 0.
#endif

            call g_the_main_loop( mytime, myiter, mythid )
            _BARRIER
#ifdef ALLOW_ADMTLM
            ftlxxmemo = g_objf_state_final(idep,jdep,1,1,1)
#else
            ftlxxmemo = g_fc
#endif

c--
c--   3. reset control vector
            if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
               call grdchk_setxx( icvrec, TANGENT_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, mythid )
            endif
            _BARRIER

#endif /* ALLOW_TANGENTLINEAR_RUN */


c******************************************************
c--   (C): Get gradient via finite difference perturbation
c******************************************************

c--   get control vector component from file
c--   perturb it and write back to file
c--   positive perturbation
            localEps = abs(grdchk_eps)
            if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
               call grdchk_getxx( icvrec, FORWARD_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, xxmemo_pert, localEps,
     &              mythid )
            endif
            _BARRIER
                     
c--   forward run with perturbed control vector
            mytime = starttime
            myiter = niter0
            call the_main_loop( mytime, myiter, mythid )
#ifdef ALLOW_ADMTLM
            fcpertplus = objf_state_final(idep,jdep,1,1,1)
#else
            fcpertplus = fc
#endif
            print *, 'ph-check fcpertplus = ', fcpertplus
            _BARRIER
                  
c--   Reset control vector.
            if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
               call grdchk_setxx( icvrec, FORWARD_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, mythid )
            endif
            _BARRIER

            fcpertminus = fcref
            print *, 'ph-check fcpertminus = ', fcpertminus

            if ( useCentralDiff ) then

c--   get control vector component from file
c--   perturb it and write back to file
c--   repeat the proceedure for a negative perturbation
               if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
                  localEps = - abs(grdchk_eps)
                  call grdchk_getxx( icvrec, FORWARD_SIMULATION,
     &                 itile, jtile, layer,
     &                 itilepos, jtilepos,
     &                 xxmemo_ref, xxmemo_pert, localEps,
     &                 mythid )
               endif
               _BARRIER
                     
c--   forward run with perturbed control vector
               mytime = starttime
               myiter = niter0
               call the_main_loop( mytime, myiter, mythid )
               _BARRIER
#ifdef ALLOW_ADMTLM
               fcpertminus = objf_state_final(idep,jdep,1,1,1)
#else
               fcpertminus = fc
#endif
                  
c--   Reset control vector.
               if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then
                  call grdchk_setxx( icvrec, FORWARD_SIMULATION,
     &                 itile, jtile, layer,
     &                 itilepos, jtilepos,
     &                 xxmemo_ref, mythid )
               endif
               _BARRIER

c-- end of if useCentralDiff ...
            end if

c******************************************************
c--   (D): calculate relative differences between gradients
c******************************************************

            if ( myProcId .EQ. grdchkwhichproc .AND.
     &           ierr .EQ. 0 ) then

               if ( grdchk_eps .eq. 0. ) then
                  gfd = (fcpertplus-fcpertminus)
               else
                  gfd = (fcpertplus-fcpertminus)
     &                 /(grdchk_epsfac*grdchk_eps)
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
                  
               tmpplot1(itilepos,jtilepos,layer,itile,jtile)
     &              = gfd
               tmpplot2(itilepos,jtilepos,layer,itile,jtile)
     &              = ratio_ad
               tmpplot3(itilepos,jtilepos,layer,itile,jtile)
     &              = ratio_ftl

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
               iobcsmem  ( ichknum ) = obcspos
               icompmem  ( ichknum ) = icomp
               ichkmem   ( ichknum ) = ichknum
               itestmem  ( ichknum ) = itest
               ierrmem   ( ichknum ) = ierr
               icglomem  ( ichknum ) = icglo

               WRITE(standardmessageunit,'(A)')
     &             'grad-res -------------------------------'
               WRITE(standardmessageunit,'(a,8I5,2x,3(1x,E18.12))')
     &              ' grad-res ',myprocid,ichknum,itilepos,jtilepos,
     &              layer,itile,jtile,obcspos,
     &              fcref, fcpertplus, fcpertminus
#ifdef ALLOW_TANGENTLINEAR_RUN
               WRITE(standardmessageunit,'(a,8I5,2x,3(1x,E18.12))')
     &              ' grad-res ',myprocid,ichknum,ichkmem(ichknum),
     &              icompmem(ichknum),itestmem(ichknum),
     &              bimem(ichknum),bjmem(ichknum),iobcsmem(ichknum),
     &              ftlxxmemo, gfd, ratio_ftl
               WRITE(msgBuf,'(A34,2(1PE24.14,X))') 
     &              'precision_grdchk_result TLM ', fcref, ftlxxmemo
               CALL PRINT_MESSAGE
     &              (msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#else
               WRITE(standardmessageunit,'(a,8I5,2x,3(1x,E18.12))')
     &              ' grad-res ',myprocid,ichknum,ichkmem(ichknum),
     &              icompmem(ichknum),itestmem(ichknum),
     &              bimem(ichknum),bjmem(ichknum),obcspos,
     &              adxxmemo, gfd, ratio_ad
               WRITE(msgBuf,'(A34,2(1PE24.14,X))') 
     &              'precision_grdchk_result ADM ', fcref, adxxmemo
               CALL PRINT_MESSAGE
     &              (msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#endif

            endif

            print *, 'ph-grd  ierr ---------------------------'
            print *, 'ph-grd  ierr = ', ierr, ', icomp = ', icomp,
     &           ', ichknum = ', ichknum

            _BARRIER

c-- else of if ( ichknum ...
         else
            ierr_grdchk = -1

c-- end of if ( ichknum ... 
         endif

c-- end of do icomp = ... 
      enddo

      if ( myProcId .EQ. grdchkwhichproc ) then
         CALL WRITE_REC_XYZ_RL( 
     &        'grd_findiff'   , tmpplot1, 1, 0, myThid)
         CALL WRITE_REC_XYZ_RL( 
     &        'grd_ratio_ad'  , tmpplot2, 1, 0, myThid)
         CALL WRITE_REC_XYZ_RL( 
     &        'grd_ratio_ftl' , tmpplot3, 1, 0, myThid)
      endif

c--   Everyone has to wait for the component to be reset.
      _BARRIER

c--   Print the results of the gradient check.
      call grdchk_print( ichknum, ierr_grdchk, mythid )

#endif /* ALLOW_GRDCHK */

      end
