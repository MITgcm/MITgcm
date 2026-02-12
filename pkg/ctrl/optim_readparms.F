#include "CTRL_OPTIONS.h"

      subroutine optim_readparms(
     I                            myThid )

c     ==================================================================
c     SUBROUTINE optim_readparms
c     ==================================================================
c
c     o Initialise the optimization part of the ECCO release.
c
c     started: Christian Eckert eckert@mit.edu 03-Mar-2000
c
c     changed:
c
c     ==================================================================
c     SUBROUTINE optim_readparms
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "OPTIMCYCLE.h"

c     == routine arguments ==
      INTEGER myThid

c     == local variables ==
      INTEGER iUnit
c     this is a dummy parameter that is never used; it is introduced here
c     so that we can have compatible namelists between mitgmcuv_ad and
c     optim.x compiled from optim_m1qn3
      _RL dfminFrac
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     These variables were formerly defined in optim.h, but they are
C     only used in the offline optimization, but never within the
C     MITgcm.  packages. We define them here locally to allow to keep
C     the namelist intact.
      INTEGER nvars
      INTEGER numiter
      INTEGER nfunc
      INTEGER iprint
      INTEGER nupdate
      _RL epsf
      _RL epsx
      _RL fmin
      _RL epsg
      _RL eps
      LOGICAL nondimcontrol

c     == end of interface ==

c--   Optimization parameters.
      NAMELIST /optim/
     &                 optimcycle, nvars,
     &                 nondimcontrol,
     &                 numiter, nfunc, fmin, dfminFrac, iprint,
     &                 epsf, epsx, epsg,
     &                 nupdate, eps

      IF ( .NOT.useCTRL ) THEN
C-    pkg CTRL is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.optim is found
         CALL PACKAGES_UNUSED_MSG( 'useCTRL',
     I                             'OPTIM_READPARMS', 'optim' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

c--     Set default values.
        optimcycle    =   0
        nvars         =  10
        nondimcontrol =  .false.
        numiter       =   0
        nfunc         =   1
        fmin          =   0.0 _d 0
        dfminFrac     =   0.0 _d 0
        iprint        =  10
        epsx          =   1.e-6
        epsg          =   1.e-6
        eps           =  -1.e-6
        nupdate       =   0

        WRITE(msgBuf,'(A)') 'OPTIM_READPARMS: opening data.optim'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.optim', 'OPTIM_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = optim)

        WRITE(msgBuf,'(A)')
     &       'OPTIM_READPARMS: finished reading data.optim'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
        CLOSE(iUnit)
#else
        CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
