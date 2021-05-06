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

#include "PROFILES_SIZE.h"
#include "profiles.h"

c     == routine arguments ==

      integer myThid

c     == local variables ==
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, num_file, num_var, IL
      LOGICAL exst, specifiedNames
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
     &                   mult_profiles_mean,
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
C number of independent samples
     &                   profiles_mean_indsamples,
#endif
     &                   prof_facmod,
     &                   prof_names,
     &                   prof_namesmod,
#ifdef ALLOW_PROFILES_CLIMMASK
     &                   prof_namesclim,
#endif
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
        prof_names(num_file,num_var)='empty'
        prof_itracer(num_file,num_var)=1
        prof_namesmod(num_file,num_var)='empty'
#ifdef ALLOW_PROFILES_CLIMMASK
        prof_namesclim(num_file,num_var)='empty'
#endif
       enddo
      enddo
      do num_var=1,NVARMAX
         mult_profiles_mean(num_var) = 1. _d 0
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
         profiles_mean_indsamples(num_var) = 1
#endif
      enddo

      profilesDoNcOutput=.false.

      IF ( (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
        profilesDoGenGrid=.true.
      ELSE
        profilesDoGenGrid=.false.
      ENDIF

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

#ifdef SINGLE_DISK_IO
        CLOSE(iUnit)
#else
        CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

        do num_file=1,NFILESPROFMAX

          specifiedNames=.FALSE.
          do num_var=1,NVARMAX
            if (prof_names(num_file,num_var).NE.'empty')
     &         specifiedNames=.TRUE.
          enddo

          if (.NOT.specifiedNames) then
            prof_names(num_file,1)='prof_T'
            prof_names(num_file,2)='prof_S'
C----- local version addition: start
            prof_names(num_file,3)='prof_U'
            prof_names(num_file,4)='prof_V'
            prof_names(num_file,5)='prof_ptr'
            prof_names(num_file,6)='prof_ssh'
            prof_names(num_file,7)='prof_DIC'
            prof_names(num_file,8)='prof_ALK'
            prof_names(num_file,9)='prof_O2'
            prof_names(num_file,10)='prof_NO3'
            prof_names(num_file,11)='prof_PO4'
            prof_names(num_file,12)='prof_FE'
            prof_names(num_file,13)='prof_PCO'
            prof_names(num_file,14)='prof_PH'
            prof_names(num_file,15)='prof_CHL'
            prof_names(num_file,16)='prof_POC'

            prof_itracer(num_file,1)=1
            prof_itracer(num_file,2)=1
            prof_itracer(num_file,3)=1
            prof_itracer(num_file,4)=1
            prof_itracer(num_file,5)=1
            prof_itracer(num_file,6)=1
            prof_itracer(num_file,7)=1
            prof_itracer(num_file,8)=2
            prof_itracer(num_file,9)=3
            prof_itracer(num_file,10)=4
            prof_itracer(num_file,11)=5
            prof_itracer(num_file,12)=6
            prof_itracer(num_file,13)=1
            prof_itracer(num_file,14)=1
            prof_itracer(num_file,15)=1
            prof_itracer(num_file,16)=1
C----- local version addition: end
            prof_namesmod(num_file,1)='theta'
            prof_namesmod(num_file,2)='salt'
C----- local version addition: start
            prof_namesmod(num_file,3)='uVel'
            prof_namesmod(num_file,4)='vVel'
            prof_namesmod(num_file,5)='pTracer'
            prof_namesmod(num_file,6)='etaN'
            prof_namesmod(num_file,7)='pTracer'
            prof_namesmod(num_file,8)='pTracer'
            prof_namesmod(num_file,9)='pTracer'
            prof_namesmod(num_file,10)='pTracer'
            prof_namesmod(num_file,11)='pTracer'
            prof_namesmod(num_file,12)='pTracer'
            prof_namesmod(num_file,13)='PCO'
            prof_namesmod(num_file,14)='PH'
            prof_namesmod(num_file,15)='CHL'
            prof_namesmod(num_file,16)='POC'
C----- local version addition: end
#ifdef ALLOW_PROFILES_CLIMMASK
            prof_namesclim(num_file,1)='prof_Tclim'
            prof_namesclim(num_file,2)='prof_Sclim'
#endif
          endif

          do num_var=1,NVARMAX
            if (((prof_names(num_file,num_var).NE.'empty').AND.
     &           (prof_namesmod(num_file,num_var).EQ.'empty')).OR.
     &          ((prof_names(num_file,num_var).EQ.'empty').AND.
     &           (prof_namesmod(num_file,num_var).NE.'empty'))) then
             print*,'prof_names=',prof_names(num_file,num_var),' ',
     &         prof_namesmod(num_file,num_var),' ',num_file,num_var
             WRITE(errorMessageUnit,'(2A)')
     &         'ERROR in PROFILES_READPARMS: inconsistent ',
     &         'prof_names and prof_namesmod'
             CALL ALL_PROC_DIE( myThid )
             STOP 'ABNORMAL END: S/R PROFILES_READPARMS'
            endif
          enddo

          do num_var=1,NVARMAX
            IL  = ILNBLNK( prof_names(num_file,num_var) )
            WRITE(prof_namesmask(num_file,num_var),'(2A)')
     &        prof_names(num_file,num_var)(1:IL),'mask'
            WRITE(prof_namesweight(num_file,num_var),'(2A)')
     &        prof_names(num_file,num_var)(1:IL),'weight'
#ifdef ALLOW_PROFILES_CLIMMASK
            WRITE(prof_namesclim(num_file,num_var),'(2A)')
     &        prof_names(num_file,num_var)(1:IL),'clim'
#endif
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
