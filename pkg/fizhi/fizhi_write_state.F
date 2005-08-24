C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_write_state.F,v 1.9 2005/08/24 23:23:34 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
CBOP
C     !ROUTINE: FIZHI_WRITE_STATE 
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_STATE( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIZHI_WRITE_STATE
C     | o Writes current state of fizhi package to an output file
C     | o     Called from do_the_model_io
C     *==========================================================*
C     \ev

C     !USES:
CEOP
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     suff    :: suffix for pickup file
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
      CHARACTER*(MAX_LEN_FNAM) pref
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(80) fn
      logical glf
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

      IF (
     &     DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write fields as consecutive records
       glf = .FALSE.
       WRITE(suff,'(I10.10)') myIter
       pref = 'fizhi_U.'
       WRITE( fn, '(A,A)' ) pref(1:8),suff(1:10)
       call mdswritefield(fn,64,glf,'RL',Nrphys,uphy,1,myIter,myThid)
       pref = 'fizhi_V.'
       WRITE( fn, '(A,A)' ) pref(1:8),suff(1:10)
       call mdswritefield(fn,64,glf,'RL',Nrphys,vphy,1,myIter,myThid)
       pref = 'fizhi_T.'
       WRITE( fn, '(A,A)' ) pref(1:8),suff(1:10)
       call mdswritefield(fn,64,glf,'RL',Nrphys,thphy,1,myIter,myThid)
       pref = 'fizhi_S.'
       WRITE( fn, '(A,A)' ) pref(1:8),suff(1:10)
       call mdswritefield(fn,64,glf,'RL',Nrphys,sphy,1,myIter,myThid)

      ENDIF

      RETURN
      END
