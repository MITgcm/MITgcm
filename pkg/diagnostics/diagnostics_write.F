C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write.F,v 1.8 2004/12/13 21:55:48 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

      SUBROUTINE DIAGNOSTICS_WRITE ( myIter, myThid ) 
C***********************************************************************
C  Purpose
C  -------
C    Output sequence for the (multiple) diagnostics output files
C
C  Arguments  Description
C  ----------------------
C     myIter ..... Current Iteration Number
C     myThid ..... my thread Id number
C***********************************************************************
       IMPLICIT NONE
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      INTEGER myIter, myThid

c Local variables
c ===============
      INTEGER   n

      IF ( myIter.ne.nIter0 ) THEN

C***********************************************************************
C***   Check to see IF its time for Diagnostic Output                ***
C***********************************************************************

        DO n = 1,nlists
          IF ( MOD(myIter,freq(n)).EQ.0 ) THEN
            CALL DIAGNOSTICS_OUT(n,myIter,myThid)
          ENDIF
        ENDDO

C-      wait for everyone before setting arrays to zero: 
        _BARRIER
        DO n = 1,nlists
          IF ( MOD(myIter,freq(n)).EQ.0 ) CALL CLRINDX(n,myThid)
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      RETURN 
      END
