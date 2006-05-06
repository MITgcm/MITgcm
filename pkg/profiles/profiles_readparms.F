C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_readparms.F,v 1.2 2006/05/06 14:33:53 heimbach Exp $

#include "PROFILES_OPTIONS.h"

      subroutine profiles_readparms( mythid )

c     ==================================================================
c     SUBROUTINE profiles_readparms
c     ==================================================================
c
c     o This routine initialises the package cost.
c     started: Ralf Giering 18-Jan-2001
c
c     ==================================================================
c     SUBROUTINE profiles_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

cph#include "ecco_cost.h"
#include "profiles.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, num_file, num_var

c     == end of interface ==

c--   Read the namelist input.
      namelist /profiles_nml/ 
     &                   profilesfiles,
     &                   mult_profiles

      _BEGIN_MASTER( myThid )

c--     Set default values.
        do num_file=1,NFILESPROFMAX
          profilesfiles(num_file)      =    ' '
        enddo

        do num_file=1,NFILESPROFMAX
         do num_var=1,4
          mult_profiles(num_file,num_var) = 0. _d 0
         enddo
        enddo

c--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'PROFILES_READPARMS: opening data.profiles'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.profiles', 'PROFILES_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(  iUnit, nml = profiles_nml )

        WRITE(msgBuf,'(2A)') 'PROFILES_READPARMS: ',
     &       'finished reading data.profiles'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

        CLOSE( iUnit )

      _END_MASTER( mythid )

      _BARRIER

      end
