C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_main.F,v 1.43 2014/04/04 21:39:04 jmc Exp $
C $Name:  $

#include "GRDCHK_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOI
C
C !TITLE: GRADIENT CHECK
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: gradient check package
C \bv
C Compare the gradients calculated by the adjoint model with
C finite difference approximations.
C
C     !CALLING SEQUENCE:
C
C the_model_main
C |
C |-- ctrl_unpack
C |-- adthe_main_loop          - unperturbed cost function and
C |-- ctrl_pack                  adjoint gradient are computed here
C |
C |-- grdchk_main
C     |
C     |-- grdchk_init
C     |-- do icomp=...        - loop over control vector elements
C         |
C         |-- grdchk_loc      - determine location of icomp on grid
C         |
C         |-- grdchk_getadxx  - get gradient component calculated
C         |                     via adjoint
C         |-- grdchk_getxx    - get control vector component from file
C         |                     perturb it and write back to file
C         |-- the_main_loop   - forward run and cost evaluation
C         |                     with perturbed control vector element
C         |-- calculate ratio of adj. vs. finite difference gradient
C         |
C         |-- grdchk_setxx    - Reset control vector element
C     |
C     |-- grdchk_print    - print results
C \ev
CEOI

CBOP
C     !ROUTINE: GRDCHK_MAIN
C     !INTERFACE:
      SUBROUTINE GRDCHK_MAIN( myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE GRDCHK_MAIN
C     ==================================================================
C     o Compare the gradients calculated by the adjoint model with
C       finite difference approximations.
C     started: Christian Eckert eckert@mit.edu 24-Feb-2000
C     continued&finished: heimbach@mit.edu: 13-Jun-2001
C     changed: mlosch@ocean.mit.edu: 09-May-2002
C              - added centered difference vs. 1-sided difference option
C              - improved output format for readability
C              - added control variable hFacC
C              heimbach@mit.edu 24-Feb-2003
C              - added tangent linear gradient checks
C              - fixes for multiproc. gradient checks
C              - added more control variables
C     ==================================================================
C     SUBROUTINE GRDCHK_MAIN
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "grdchk.h"
#include "cost.h"
#include "ctrl.h"
#include "g_cost.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C     !LOCAL VARIABLES:
C     == local variables ==
      INTEGER myIter
      _RL     myTime
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER iMin, iMax, jMin, jMax
      PARAMETER( iMin = 1 , iMax = sNx , jMin = 1 , jMax = sNy )
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER icomp
      INTEGER ichknum
      INTEGER icvrec
      INTEGER jtile
      INTEGER itile
      INTEGER layer
      INTEGER obcspos
      INTEGER itilepos
      INTEGER jtilepos
      INTEGER icglo
      INTEGER itest
      INTEGER ierr
      INTEGER ierr_grdchk
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
      _RL tmpplot1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpplot2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpplot3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C     == end of interface ==
CEOP

      ioUnit = standardMessageUnit
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// Gradient-check starts (grdchk_main)'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_TANGENTLINEAR_RUN
C--   prevent writing output multiple times
C     note: already called in AD run so that only needed for TLM run
      CALL TURNOFF_MODEL_IO( 0, myThid )
#endif

C--   initialise variables
      CALL GRDCHK_INIT( myThid )

C--   Compute the adjoint model gradients.
C--   Compute the unperturbed cost function.
Cph   Gradient via adjoint has already been computed,
Cph   and so has unperturbed cost function,
Cph   assuming all xx_ fields are initialised to zero.

      ierr      = 0
      ierr_grdchk = 0
      adxxmemo  = 0.
      ftlxxmemo = 0.
#if (defined  (ALLOW_ADMTLM))
      fcref = objf_state_final(idep,jdep,1,1,1)
#elif (defined (ALLOW_OPENAD))
      fcref = fc%v
#else
      fcref = fc
#endif
      WRITE(msgBuf,'(A,1PE22.14)')
     &         'grdchk reference fc: fcref       =', fcref
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, Nr
            DO j = jMin, jMax
              DO i = iMin, iMax
                tmpplot1(i,j,k,bi,bj) = 0. _d 0
                tmpplot2(i,j,k,bi,bj) = 0. _d 0
                tmpplot3(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF ( useCentralDiff ) THEN
         grdchk_epsfac = 2. _d 0
      ELSE
         grdchk_epsfac = 1. _d 0
      ENDIF

      WRITE(standardmessageunit,'(A)')
     &    'grad-res -------------------------------'
      WRITE(standardmessageunit,'(2a)')
     &    ' grad-res  proc    #    i    j    k   bi   bj iobc',
     &    '       fc ref            fc + eps           fc - eps'
#ifdef ALLOW_TANGENTLINEAR_RUN
      WRITE(standardmessageunit,'(2a)')
     &    ' grad-res  proc    #    i    j    k   bi   bj iobc',
     &    '      tlm grad            fd grad          1 - fd/tlm'
#else
      WRITE(standardmessageunit,'(2a)')
     &    ' grad-res  proc    #    i    j    k   bi   bj iobc',
     &    '      adj grad            fd grad          1 - fd/adj'
#endif

C--   Compute the finite difference approximations.
C--   Cycle through all processes doing NINT(nend-nbeg+1)/nstep
C--   gradient checks.

      IF ( nbeg .EQ. 0 ) CALL GRDCHK_GET_POSITION( myThid )

      DO icomp = nbeg, nend, nstep

        adxxmemo  = 0.
        ichknum = (icomp - nbeg)/nstep + 1

        IF ( ichknum .LE. maxgrdchecks ) THEN
          WRITE(msgBuf,'(A,I4,A)')
     &      '====== Starts gradient-check number', ichknum,
     &                                         ' (=ichknum) ======='
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C--       Determine the location of icomp on the grid.
          IF ( myProcId .EQ. grdchkwhichproc ) THEN
            CALL grdchk_loc( icomp, ichknum,
     &              icvrec, itile, jtile, layer, obcspos,
     &              itilepos, jtilepos, icglo, itest, ierr,
     &              myThid )
          ELSE
            icvrec   = 0
            itile    = 0
            jtile    = 0
            layer    = 0
            obcspos  = 0
            itilepos = 0
            jtilepos = 0
            icglo    = 0
            itest    = 0
          ENDIF

C make sure that all procs have correct file records, so that useSingleCpuIO works
          CALL GLOBAL_SUM_INT( icvrec , myThid )
          CALL GLOBAL_SUM_INT( layer , myThid )
          CALL GLOBAL_SUM_INT( ierr , myThid )

          WRITE(msgBuf,'(A,3I5,A,2I4,A,I3,A,I4)')
     &     'grdchk pos: i,j,k=', itilepos, jtilepos, layer,
     &              ' ; bi,bj=', itile, jtile, ' ; iobc=', obcspos,
     &              ' ; rec=', icvrec
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C******************************************************
C--   (A): get gradient component calculated via adjoint
C******************************************************

C--   get gradient component calculated via adjoint
          CALL GRDCHK_GETADXX( icvrec,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              adxxmemo, ierr, myThid )
C--   Add a global-sum call so that all proc will get the adjoint gradient
          _GLOBAL_SUM_RL( adxxmemo, myThid )

#ifdef ALLOW_TANGENTLINEAR_RUN
C******************************************************
C--   (B): Get gradient component g_fc from tangent linear run:
C******************************************************

C--   1. perturb control vector component: xx(i)=1.

          localEps = 1. _d 0
          CALL GRDCHK_GETXX( icvrec, TANGENT_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, xxmemo_pert, localEps,
     &              ierr, myThid )

C--   2. perform tangent linear run
          myTime = startTime
          myIter = nIter0
#ifdef ALLOW_ADMTLM
          DO k=1,4*Nr+1
            DO j=1,sNy
              DO i=1,sNx
                g_objf_state_final(i,j,1,1,k) = 0.
              ENDDO
            ENDDO
          ENDDO
#else
          g_fc = 0.
#endif

          CALL G_THE_MAIN_LOOP( myTime, myIter, myThid )
#ifdef ALLOW_ADMTLM
          ftlxxmemo = g_objf_state_final(idep,jdep,1,1,1)
#else
          ftlxxmemo = g_fc
#endif

C--   3. reset control vector
          CALL GRDCHK_SETXX( icvrec, TANGENT_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, ierr, myThid )

#endif /* ALLOW_TANGENTLINEAR_RUN */

C******************************************************
C--   (C): Get gradient via finite difference perturbation
C******************************************************

C--   (C-1) positive perturbation:
          localEps = ABS(grdchk_eps)

C--   get control vector component from file
C--   perturb it and write back to file
          CALL GRDCHK_GETXX( icvrec, FORWARD_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, xxmemo_pert, localEps,
     &              ierr, myThid )

C--   forward run with perturbed control vector
          myTime = startTime
          myIter = nIter0
#ifdef ALLOW_OPENAD
          CALL OpenAD_THE_MAIN_LOOP( myTime, myIter, myThid )
#else
          CALL THE_MAIN_LOOP( myTime, myIter, myThid )
#endif

#if (defined (ALLOW_ADMTLM))
          fcpertplus = objf_state_final(idep,jdep,1,1,1)
#elif (defined (ALLOW_OPENAD))
          fcpertplus = fc%v
#else
          fcpertplus = fc
#endif
          WRITE(msgBuf,'(A,1PE22.14)')
     &         'grdchk perturb(+)fc: fcpertplus  =', fcpertplus
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C--   Reset control vector.
          CALL GRDCHK_SETXX( icvrec, FORWARD_SIMULATION,
     &              itile, jtile, layer,
     &              itilepos, jtilepos,
     &              xxmemo_ref, ierr, myThid )

          fcpertminus = fcref
          IF ( useCentralDiff ) THEN
C--   (C-2) repeat the proceedure for a negative perturbation:
            localEps = - ABS(grdchk_eps)

C--   get control vector component from file
C--   perturb it and write back to file
            CALL GRDCHK_GETXX( icvrec, FORWARD_SIMULATION,
     &                 itile, jtile, layer,
     &                 itilepos, jtilepos,
     &                 xxmemo_ref, xxmemo_pert, localEps,
     &                 ierr, myThid )

C--   forward run with perturbed control vector
            myTime = startTime
            myIter = nIter0
#ifdef ALLOW_OPENAD
            CALL OpenAD_THE_MAIN_LOOP( myTime, myIter, myThid )
#else
            CALL THE_MAIN_LOOP( myTime, myIter, myThid )
#endif

#if (defined (ALLOW_ADMTLM))
            fcpertminus = objf_state_final(idep,jdep,1,1,1)
#elif (defined (ALLOW_OPENAD))
            fcpertminus = fc%v
#else
            fcpertminus = fc
#endif
            WRITE(msgBuf,'(A,1PE22.14)')
     &         'grdchk perturb(-)fc: fcpertminus =', fcpertminus
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C--   Reset control vector.
            CALL GRDCHK_SETXX( icvrec, FORWARD_SIMULATION,
     &                 itile, jtile, layer,
     &                 itilepos, jtilepos,
     &                 xxmemo_ref, ierr, myThid )

C-- end of if useCentralDiff ...
          ENDIF

C******************************************************
C--   (D): calculate relative differences between gradients
C******************************************************

          IF ( grdchk_eps .EQ. 0. ) THEN
            gfd = (fcpertplus-fcpertminus)
          ELSE
            gfd = (fcpertplus-fcpertminus) /(grdchk_epsfac*grdchk_eps)
          ENDIF

          IF ( adxxmemo .EQ. 0. ) THEN
            ratio_ad = ABS( adxxmemo - gfd )
          ELSE
            ratio_ad = 1. - gfd/adxxmemo
          ENDIF

          IF ( ftlxxmemo .EQ. 0. ) THEN
            ratio_ftl = ABS( ftlxxmemo - gfd )
          ELSE
            ratio_ftl = 1. - gfd/ftlxxmemo
          ENDIF

          IF ( myProcId .EQ. grdchkwhichproc .AND. ierr .EQ. 0 ) THEN
            tmpplot1(itilepos,jtilepos,layer,itile,jtile) = gfd
            tmpplot2(itilepos,jtilepos,layer,itile,jtile) = ratio_ad
            tmpplot3(itilepos,jtilepos,layer,itile,jtile) = ratio_ftl
          ENDIF

          IF ( ierr .EQ. 0 ) THEN
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
          ENDIF

          IF ( myProcId .EQ. grdchkwhichproc .AND. ierr .EQ. 0 ) THEN
            WRITE(standardmessageunit,'(A)')
     &          'grad-res -------------------------------'
            WRITE(standardmessageunit,'(A,8I5,1x,1P3E19.11)')
     &           ' grad-res ',myprocid,ichknum,itilepos,jtilepos,
     &             layer,itile,jtile,obcspos,
     &             fcref, fcpertplus, fcpertminus
#ifdef ALLOW_TANGENTLINEAR_RUN
            WRITE(standardmessageunit,'(A,8I5,1x,1P3E19.11)')
     &           ' grad-res ',myprocid,ichknum,ichkmem(ichknum),
     &             icompmem(ichknum),itestmem(ichknum),
     &             bimem(ichknum),bjmem(ichknum),iobcsmem(ichknum),
     &             ftlxxmemo, gfd, ratio_ftl
#else
            WRITE(standardmessageunit,'(A,8I5,1x,1P3E19.11)')
     &           ' grad-res ',myprocid,ichknum,ichkmem(ichknum),
     &             icompmem(ichknum),itestmem(ichknum),
     &             bimem(ichknum),bjmem(ichknum),iobcsmem(ichknum),
     &             adxxmemo, gfd, ratio_ad
#endif
          ENDIF
#ifdef ALLOW_TANGENTLINEAR_RUN
          WRITE(msgBuf,'(A30,1PE22.14)')
     &              ' TLM  ref_cost_function      =', fcref
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A30,1PE22.14)')
     &              ' TLM  tangent-lin_grad       =', ftlxxmemo
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A30,1PE22.14)')
     &              ' TLM  finite-diff_grad       =', gfd
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#else
          WRITE(msgBuf,'(A30,1PE22.14)')
     &              ' ADM  ref_cost_function      =', fcref
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A30,1PE22.14)')
     &              ' ADM  adjoint_gradient       =', adxxmemo
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A30,1PE22.14)')
     &              ' ADM  finite-diff_grad       =', gfd
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif

          WRITE(msgBuf,'(A,I4,A,I3,A)')
     &      '====== End of gradient-check number', ichknum,
     &                      ' (ierr=', ierr, ') ======='
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C-- else of if ichknum < ...
        ELSE
          ierr_grdchk = -1

C-- end of if ichknum < ...
        ENDIF

C-- end of do icomp = ...
      ENDDO

      IF (myProcId .EQ. grdchkwhichproc .AND. .NOT.useSingleCpuIO) THEN
        CALL WRITE_REC_XYZ_RL(
     &       'grd_findiff'   , tmpplot1, 1, 0, myThid)
        CALL WRITE_REC_XYZ_RL(
     &       'grd_ratio_ad'  , tmpplot2, 1, 0, myThid)
        CALL WRITE_REC_XYZ_RL(
     &       'grd_ratio_ftl' , tmpplot3, 1, 0, myThid)
      ENDIF

C--   Print the results of the gradient check.
      CALL GRDCHK_PRINT( ichknum, ierr_grdchk, myThid )

#endif /* ALLOW_GRDCHK */

      RETURN
      END
