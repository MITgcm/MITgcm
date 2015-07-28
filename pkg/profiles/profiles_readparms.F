C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_readparms.F,v 1.18 2015/07/28 01:00:39 gforget Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"

      subroutine profiles_readparms( myThid )

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

#include "profiles.h"

c     == routine arguments ==

      integer myThid

c     == local variables ==
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, num_file, num_var, IL
      LOGICAL exst
      character*(128) fname

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == end of interface ==

c--   Read the namelist input.
      namelist /profiles_nml/
     &                   profilesDir,
     &                   profilesfiles,
     &                   mult_profiles,
     &                   prof_facmod,
     &                   prof_names,
     &                   prof_namesmod,
     &                   prof_itracer,
     &                   profilesDoNcOutput,
     &                   profilesDoGenGrid

      IF ( .NOT.usePROFILES ) THEN
C-    pkg PROFILES is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.profiles is found
         CALL PACKAGES_UNUSED_MSG( 'usePROFILES', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

c--     Set default values.
      profilesDir=' '

      do num_file=1,NFILESPROFMAX
          profilesfiles(num_file)      =    ' '
      enddo

      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        mult_profiles(num_file,num_var) = 1. _d 0
        prof_facmod(num_file,num_var)  = 1. _d 0
       enddo
      enddo

      do num_file=1,NFILESPROFMAX
       prof_names(num_file,1)='prof_T'
       prof_names(num_file,2)='prof_S'
       prof_names(num_file,3)='prof_U'
       prof_names(num_file,4)='prof_V'
       prof_names(num_file,5)='prof_ptr'
       prof_names(num_file,6)='prof_ssh'
       prof_itracer(num_file,1)=1
       prof_itracer(num_file,2)=1
       prof_itracer(num_file,3)=1
       prof_itracer(num_file,4)=1
       prof_itracer(num_file,5)=1
       prof_itracer(num_file,6)=1
       prof_namesmod(num_file,1)='theta'
       prof_namesmod(num_file,2)='salt'
       prof_namesmod(num_file,3)='uVel'
       prof_namesmod(num_file,4)='vVel'
       prof_namesmod(num_file,5)='pTracer'
       prof_namesmod(num_file,6)='etaN'
      enddo

      profilesDoNcOutput=.false.
#ifdef ALLOW_PROFILES_GENERICGRID
      profilesDoGenGrid=.true.
#else
      profilesDoGenGrid=.false.
#endif

      if (NVARMAX.GT.6) then
        WRITE(msgBuf,'(A)')
     & 'PROFILES_READPARMS: you need to define variables 7 to NVARMAX'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R PROFILES_READPARMS'
      endif

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

        do num_file=1,NFILESPROFMAX

          do num_var=1,NVARMAX
            IL  = ILNBLNK( prof_names(num_file,num_var) )
            WRITE(prof_namesmask(num_file,num_var),'(2A)')
     &        prof_names(num_file,num_var)(1:IL),'mask'
            WRITE(prof_namesweight(num_file,num_var),'(2A)')
     &        prof_names(num_file,num_var)(1:IL),'weight'
          enddo

          if ( profilesfiles(num_file) .NE. ' ' ) then
            IL  = ILNBLNK( profilesfiles(num_file) )
            fname = profilesfiles(num_file)(1:IL)//'.nc'
            inquire( file=fname, exist=exst )
            if (.NOT.exst) then
c warn user as we override profilesfiles
             WRITE(msgBuf,'(3A)')
     &       '** WARNING ** PROFILES_READPARMS: missing file: ',
     &       profilesfiles(num_file)(1:IL),' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
c switch off this file (and potential cost function term)
              profilesfiles(num_file) = ' '
            endif
          endif
        enddo

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
