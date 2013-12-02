C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_read_params.F,v 1.4 2013/12/02 22:03:08 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_READ_PARAMS

C !INTERFACE:
      SUBROUTINE CPL_READ_PARAMS( msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_READ_PARAMS
C     | o Read coupler and mapping parameters
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "CPL_PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit          :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
CEOP

      WRITE(msgUnit,'(2A)') 'CPL_READ_PARAMS: ',
     &                      'reading parameter file "data"'
        OPEN(88, FILE='data', STATUS='OLD')
c       READ(88,*) nCouplingSteps
        READ(88,*)
        READ(88,*) runOffMapSize
        READ(88,'(A)') runOffMapFile
        CLOSE(88)

        WRITE(msgUnit,'(2A,I8)') 'CPL_READ_PARAMS: ',
     &                'runOffMapSize=', runOffMapSize
        WRITE(msgUnit,'(3A)') 'CPL_READ_PARAMS: ',
     &                'runOffMapFile=', runOffMapFile

      RETURN
      END
