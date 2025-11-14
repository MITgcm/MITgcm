#include "PROFILES_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_READPARMS

C !INTERFACE:
      SUBROUTINE PROFILES_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialise the package cost.
C     Started: Ralf Giering 18-Jan-2001

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "PROFILES_SIZE.h"
#include "profiles.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error message buffer
C     iUnit  :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, num_file, num_var, IL
      LOGICAL exst, specifiedNames
      CHARACTER*(128) fname

C Read the namelist input.
      NAMELIST /profiles_nml/
     &                   profilesDir,
     &                   profilesfiles,
     &                   mult_profiles,
     &                   mult_profiles_mean,
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
C Number of independent samples
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
     &                   profilesDoGenGrid,
     &                   prof_make_nc

      IF ( .NOT.usePROFILES ) THEN
        _BEGIN_MASTER(myThid)
C Track pkg activation status:
C Print a (weak) warning if data.profiles is found
        CALL PACKAGES_UNUSED_MSG( 'usePROFILES', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

C Set default values.
      profilesDir=' '

      DO num_file = 1, NFILESPROFMAX
        profilesfiles(num_file) = ' '
      ENDDO

      DO num_file = 1, NFILESPROFMAX
        DO num_var = 1, NVARMAX
          mult_profiles(num_file,num_var) = 1. _d 0
          prof_facmod(num_file,num_var)  = 1. _d 0
          prof_names(num_file,num_var) = 'empty'
          prof_itracer(num_file,num_var) = 1
          prof_namesmod(num_file,num_var) = 'empty'
#ifdef ALLOW_PROFILES_CLIMMASK
          prof_namesclim(num_file,num_var) = 'empty'
#endif
        ENDDO
      ENDDO

      DO num_var = 1, NVARMAX
        mult_profiles_mean(num_var) = 1. _d 0
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
        profiles_mean_indsamples(num_var) = 1
#endif
      ENDDO

      profilesDoNcOutput = .false.
      prof_make_nc = .false.

      IF ( (.NOT.usingSphericalPolarGrid.OR.rotateGrid) ) THEN
        profilesDoGenGrid = .true.
      ELSE
        profilesDoGenGrid = .false.
      ENDIF

C Next, read cost data file.
      WRITE(msgBuf,'(A)') 'PROFILES_READPARMS: opening data.profiles'
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, 1 )

      CALL OPEN_COPY_DATA_FILE(
     I     'data.profiles', 'PROFILES_READPARMS',
     O     iUnit,
     I     myThid )

      READ( iUnit, nml = profiles_nml )

      WRITE(msgBuf,'(2A)') 'PROFILES_READPARMS: ',
     & 'finished reading data.profiles'
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, 1 )

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit, STATUS = 'DELETE')
#endif /* SINGLE_DISK_IO */

      DO num_file = 1, NFILESPROFMAX
        specifiedNames = .FALSE.

        DO num_var = 1, NVARMAX
          IF (prof_names(num_file,num_var).NE.'empty')
     &      specifiedNames = .TRUE.
        ENDDO

        IF (.NOT.specifiedNames) THEN
          prof_names(num_file,1) = 'prof_T'
          prof_names(num_file,2) = 'prof_S'
#ifdef ALLOW_BLING
          prof_names(num_file,3) = 'prof_U'
          prof_names(num_file,4) = 'prof_V'
          prof_names(num_file,5) = 'prof_ptr'
          prof_names(num_file,6) = 'prof_ssh'
          prof_names(num_file,7) = 'prof_DIC'
          prof_names(num_file,8) = 'prof_ALK'
          prof_names(num_file,9) = 'prof_O2'
          prof_names(num_file,10) = 'prof_NO3'
          prof_names(num_file,11) = 'prof_PO4'
          prof_names(num_file,12) = 'prof_FE'
          prof_names(num_file,13) = 'prof_PCO'
          prof_names(num_file,14) = 'prof_PH'
          prof_names(num_file,15) = 'prof_CHL'
          prof_names(num_file,16) = 'prof_POC'

          prof_itracer(num_file,1) = 1
          prof_itracer(num_file,2) = 1
          prof_itracer(num_file,3) = 1
          prof_itracer(num_file,4) = 1
          prof_itracer(num_file,5) = 1
          prof_itracer(num_file,6) = 1
          prof_itracer(num_file,7) = 1
          prof_itracer(num_file,8) = 2
          prof_itracer(num_file,9) = 3
          prof_itracer(num_file,10) = 4
          prof_itracer(num_file,11) = 5
          prof_itracer(num_file,12) = 6
          prof_itracer(num_file,13) = 1
          prof_itracer(num_file,14) = 1
          prof_itracer(num_file,15) = 1
          prof_itracer(num_file,16) = 1
#endif /* ALLOW_BLING */

          prof_namesmod(num_file,1) = 'theta'
          prof_namesmod(num_file,2) = 'salt'
#ifdef ALLOW_BLING
          prof_namesmod(num_file,3) = 'uVel'
          prof_namesmod(num_file,4) = 'vVel'
          prof_namesmod(num_file,5) = 'pTracer'
          prof_namesmod(num_file,6) = 'etaN'
          prof_namesmod(num_file,7) = 'pTracer'
          prof_namesmod(num_file,8) = 'pTracer'
          prof_namesmod(num_file,9) = 'pTracer'
          prof_namesmod(num_file,10) = 'pTracer'
          prof_namesmod(num_file,11) = 'pTracer'
          prof_namesmod(num_file,12) = 'pTracer'
          prof_namesmod(num_file,13) = 'PCO'
          prof_namesmod(num_file,14) = 'PH'
          prof_namesmod(num_file,15) = 'CHL'
          prof_namesmod(num_file,16) = 'POC'
#endif /* ALLOW_BLING */
#ifdef ALLOW_PROFILES_CLIMMASK

          prof_namesclim(num_file,1) = 'prof_Tclim'
          prof_namesclim(num_file,2) = 'prof_Sclim'
#endif

        ENDIF !IF (.NOT. specifiedNames)

        DO num_var = 1, NVARMAX
          IF ( ((prof_names(num_file,num_var).NE.'empty') .AND.
     &          (prof_namesmod(num_file,num_var).EQ.'empty')) .OR.
     &         ((prof_names(num_file,num_var).EQ.'empty') .AND.
     &          (prof_namesmod(num_file,num_var).NE.'empty')) ) THEN
            PRINT *, 'prof_names = ',prof_names(num_file,num_var),' ',
     &       prof_namesmod(num_file,num_var),' ',num_file,num_var

            WRITE(errorMessageUnit,'(2A)')
     &       'ERROR in PROFILES_READPARMS: inconsistent ',
     &       'prof_names and prof_namesmod'
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R PROFILES_READPARMS'

          ENDIF

          IL  = ILNBLNK( prof_names(num_file,num_var) )
          WRITE(prof_namesmask(num_file,num_var),'(2A)')
     &     prof_names(num_file,num_var)(1:IL),'mask'
          WRITE(prof_namesweight(num_file,num_var),'(2A)')
     &     prof_names(num_file,num_var)(1:IL),'weight'
#ifdef ALLOW_PROFILES_CLIMMASK
          WRITE(prof_namesclim(num_file,num_var),'(2A)')
     &     prof_names(num_file,num_var)(1:IL),'clim'
#endif
        ENDDO !DO num_var

        IF (profilesfiles(num_file).NE.' ') THEN
          IL  = ILNBLNK( profilesfiles(num_file) )
          fname = profilesfiles(num_file)(1:IL)//'.nc'
          INQUIRE( FILE = fname, EXIST = exst )
          IF (.NOT.exst) THEN
C Warn user as we override profilesfiles
            WRITE(msgBuf,'(3A)')
     &       '** WARNING ** PROFILES_READPARMS: missing file: ',
     &       profilesfiles(num_file)(1:IL),' gets switched off'
            CALL PRINT_MESSAGE( msgBuf,
     &           errorMessageUnit, SQUEEZE_RIGHT, myThid )

C Switch off this file (and potential cost function term)
              profilesfiles(num_file) = ' '

          ENDIF
        ENDIF

      ENDDO !DO num_file

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
