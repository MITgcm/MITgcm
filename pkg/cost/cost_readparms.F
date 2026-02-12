#include "COST_OPTIONS.h"

      SUBROUTINE COST_READPARMS( myThid )

c     ==================================================================
c     SUBROUTINE cost_readparms
c     ==================================================================
c
c     o This routine initialises the package cost.
c
c     started: Ralf Giering 18-Jan-2001
c
c     ==================================================================
c     SUBROUTINE cost_readparms
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "cost.h"

c     == routine arguments ==
      INTEGER myThid

c     == local variables ==
      CHARACTER*(max_len_mbuf) msgbuf
      INTEGER iUnit

c     == end of interface ==

c--   Read the namelist input.
      NAMELIST /cost_nml/
     &         mult_atl, mult_test, mult_tracer,
     &         multTheta, multSalt, multUvel, multVvel, multEtan,
#ifdef ALLOW_COST_DEPTH
     &         mult_depth,
#endif
#ifdef ALLOW_COST_HFLUXM
     &         mult_temp_tut, mult_hflux_tut,
#endif
     &         lastinterval, cost_mask_file

      _BEGIN_MASTER( myThid )

c--     Set default values.
        mult_atl              =      0. _d 0
        mult_test             =      0. _d 0
        mult_tracer           =      0. _d 0
        multTheta             =      0. _d 0
        multSalt              =      0. _d 0
        multUvel              =      0. _d 0
        multVvel              =      0. _d 0
        multEtan              =      0. _d 0
#ifdef ALLOW_COST_DEPTH
        mult_depth            =      0. _d 0
#endif
#ifdef ALLOW_COST_HFLUXM
        mult_temp_tut         =      0. _d 0
        mult_hflux_tut        =      0. _d 0
#endif
        cost_mask_file        =      ' '

c--     30*24*3600 = 2592000 = 1 model month
        lastinterval = 2592000.

c--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'COST_READPARMS: opening data.cost'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.cost', 'COST_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(  iUnit, nml = cost_nml )

        WRITE(msgBuf,'(A)') 'COST_READPARMS: finished reading data.cost'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
        CLOSE(iUnit)
#else
        CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

        IF ( MOD(lastinterval,deltaTClock) .GT. 0. )
     &       lastinterval = MAX(
     &       INT(lastinterval/deltaTClock-1)*deltaTClock,deltaTClock)

#if (defined ALLOW_COST_HFLUXM) || (defined ALLOW_COST_TEMP)
c-- get the weights for the cost functions
      CALL COST_WEIGHTS( myThid )
cgf : to compile previous line user is expected to provide cost_weights.F
#endif

C--   print cost parameters to stdout for better debugging
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') '// cost configuration  >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_RL( lastinterval, INDEX_NONE,'lastinterval =',
     &'   /* cost interval over which to average ( s ). */')
C
       CALL WRITE_0D_C( cost_mask_file, -1, INDEX_NONE,
     &  'cost_mask_file =', ' /* file name of cost mask file */')

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// cost configuration  >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
