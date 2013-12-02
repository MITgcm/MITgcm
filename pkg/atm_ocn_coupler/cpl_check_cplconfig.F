C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_check_cplconfig.F,v 1.1 2013/12/02 23:25:38 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_CHECK_CPLCONFIG

C !INTERFACE:
      SUBROUTINE CPL_CHECK_CPLCONFIG(
     I                     msgUnit,
     U                     errFlag, errMsg )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_CHECK_CPLCONFIG
C     | o Routine to check Coupler config
C     *==========================================================*
C     | This routine also set the number of coupler time-step
C     |  to run
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "ATMSIZE.h"
#include "OCNSIZE.h"
#include "ATMVARS.h"
#include "OCNVARS.h"
#include "CPL_PARAMS.h"
#include "CPL_MAP2GRIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit  :: log-file I/O unit
C     errFlag  :: logical flag to report an error
C     errMsg   :: error message to print
      INTEGER msgUnit
      LOGICAL errFlag
      CHARACTER*(*) errMsg

C !LOCAL VARIABLES:
      INTEGER i, j, n
      INTEGER ijo, ija
      LOGICAL tmpVar(Nx_atm,Ny_atm)
CEOP

C--   Define length of the run (number of coupler time-step)
      WRITE(msgUnit,'(2A,I9)') ' CPL_CHECK_CPLCONFIG: ',
     &    'Nb of time-steps to run (ATM) =', nSteps_atm
      WRITE(msgUnit,'(2A,I9)') ' CPL_CHECK_CPLCONFIG: ',
     &    'Nb of time-steps to run (OCN) =', nSteps_ocn
      IF ( nSteps_atm .EQ. nSteps_ocn ) THEN
        nCouplingSteps = nSteps_ocn
        WRITE(msgUnit,'(2A,I9)') ' CPL_CHECK_CPLCONFIG: ',
     &    'Nb of time-steps to run (CPL) =', nCouplingSteps
      ELSE
        errFlag = .TRUE.
        nCouplingSteps = 0
        WRITE(msgUnit,'(2A)') ' *** ERROR *** CPL_CHECK_CPLCONFIG: ',
     &    ' wrong Nb of time-steps to run'
      ENDIF

C--   Check for consistent RunOff mapping regarding Land/Sea mask:
      DO j=1,Ny_atm
       DO i=1,Nx_atm
        tmpVar(i,j) = landMask_atm(i,j) .EQ. 1. _d 0
       ENDDO
      ENDDO
      DO n=1,nROmap
C     a) check that RunOff drain a land point
        ija = ijROatm(n) - 1
        i = 1 + MOD(ija,Nx_atm)
        j = 1 + ija/Nx_atm
        IF ( landMask_atm(i,j).EQ.0. ) THEN
          errFlag = .TRUE.
          WRITE(msgUnit,'(A,2I8,A)')
     &    ' *** ERROR *** RunOff Mapp: i,j=', i, j, ' not Land point'
        ENDIF
        tmpVar(i,j) = .FALSE.
C     b) and supply an ocean point
        ijo = ijROocn(n) - 1
        i = 1 + MOD(ijo,Nx_ocn)
        j = 1 + ijo/Nx_ocn
        IF ( OcMxlD_ocn(i,j).EQ.0. ) THEN
          errFlag = .TRUE.
          WRITE(msgUnit,'(A,2I8,A)')
     &    ' *** ERROR *** RunOff Mapp: i,j=', i, j, ' not Oce point'
        ENDIF
      ENDDO
C     c) check that all land-points are drained,
C  note: would need to know ATM grid-cell area to check that
C        RunOff Mapping conserve water.
      DO j=1,Ny_atm
       DO i=1,Nx_atm
        IF ( tmpVar(i,j) ) THEN
         WRITE(msgUnit,'(A,2I8,A)')
     &    ' ** WARNING ** Land point: i,j=', i, j, ' not drained'
        ENDIF
       ENDDO
      ENDDO

      IF ( errFlag ) THEN
        errMsg = 'in CPL_CHECK_CPLCONFIG'
      ELSE
        WRITE(msgUnit,'(2A)') ' CPL_CHECK_CPLCONFIG: ','done'
      ENDIF

      RETURN
      END
