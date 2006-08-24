C $Header: /u/gcmpack/MITgcm/verification/rotating_tank/code/Attic/read_pickup.F,v 1.1 2006/08/24 19:00:20 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: READ_PICKUP
C     !INTERFACE:
      SUBROUTINE READ_PICKUP ( myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE READ_PICKUP                                    
C     | o Controlling routine for IO to write restart file.       
C     *==========================================================*
C     | Read model checkpoint files for use in restart.           
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
      INTEGER myThid
      INTEGER myIter

C     !LOCAL VARIABLES:
C     == Local variables ==
C     oldPrec :: Temp. for hold I/O precision information
C     prec    
C     fn      :: Temp. for building file name.
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
CEOP

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER

        prec = precFloat64

C--     Read model fields
        WRITE(fn,'(A,I10.10)') 'pickup.',myIter
        CALL MDSREADFIELD(fn,prec,'RL',Nr,uVel,   1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gU,     2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gUnm1,  3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,vVel,   4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gV,     5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gVnm1,  6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,wVel,  7,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,theta,  8,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gT,     9,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gTnm1,  10,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,salt,  11,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gS,    12,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gSnm1, 13,myThid)
        CALL MDSREADFIELD(fn,prec,'RL', 1,etaN,13*Nr+1,myThid)
#ifdef NONLIN_FRSURF
        IF ( nonlinFreeSurf.GE.0)
     &  CALL MDSREADFIELD(fn,prec,'RL',1,etaH,13*Nr+2,myThid)
#endif

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         CALL MDSREADFIELD(fn,prec,'RL',Nr,phi_nh,1,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',Nr,gW,   2,myThid)
c        CALL MDSREADFIELD(fn,prec,'RL',Nr,gWnm1,3,myThid)
        ENDIF
#endif

       _BARRIER

C--    Fill in edge regions
      CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
      CALL EXCH_UV_XYZ_RL(gU,gV,.TRUE.,myThid)
      CALL EXCH_UV_XYZ_RL(gUnm1,gVnm1,.TRUE.,myThid)
c     _EXCH_XYZ_R8(uVel  , myThid )
c     _EXCH_XYZ_R8(gu    , myThid )
c     _EXCH_XYZ_R8(guNM1 , myThid )
c     _EXCH_XYZ_R8(vVel  , myThid )
c     _EXCH_XYZ_R8(gv    , myThid )
c     _EXCH_XYZ_R8(gvNM1 , myThid )
      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(gt    , myThid )
      _EXCH_XYZ_R8(gtNM1 , myThid )
      _EXCH_XYZ_R8(salt  , myThid )
      _EXCH_XYZ_R8(gs    , myThid )
      _EXCH_XYZ_R8(gsNM1 , myThid )
      _EXCH_XY_R8 (etaN, myThid )
      _EXCH_XY_R8( etaH, myThid )

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         _EXCH_XYZ_R8(phi_nh, myThid )
         _EXCH_XYZ_R8(gW    , myThid )
c        _EXCH_XYZ_R8(gWNM1 , myThid )
        ENDIF
#endif

      RETURN
      END
