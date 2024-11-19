#include "PROFILES_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_INIT_FIXED

C !INTERFACE:
      SUBROUTINE PROFILES_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialization for netcdf profiles data

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
#include "cal.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid: my thread ID number
      INTEGER myThid
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL MDS_RECLEN

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER kLev, kC, kCMax, kProf, prof_num
      INTEGER chunk, chunkProf, c, recLen
      INTEGER i,j,bi,bj,iG,jG,num_file,num_var,ProfNo_tile
      INTEGER stopProfiles
      INTEGER fid, dimId, varId1, varId1a, varId1b
      INTEGER varId2,varId3
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax

      INTEGER tmpdate(4),tmpdiff(4),profIsInRunTime
      _RL tmp_lon, tmp_lon2(1000), tmp_lat2(1000)
      _RL lon_cur, lat_cur
      _RL lon_1, lon_2, lat_1, lat_2
      _RL lon_tmp1, lon_tmp2
      _RL lat_fac, lon_fac
      INTEGER prof_i, prof_j
      INTEGER vec_start(2), vec_count(2), profno_div1000, kk
      CHARACTER*(MAX_LEN_FNAM) profilesfile, fnamedatanc
      CHARACTER*(MAX_LEN_FNAM) fnameequinc
      CHARACTER*(MAX_LEN_FNAM) adfnameequinc, tlfnameequinc
      INTEGER IL, JL, KL, err
      LOGICAL  exst

      INTEGER varId_intp1, varId_intp2, varId_intp11, varId_intp22
      INTEGER varId_intp3, varId_intp4, varId_intp5, q, iINTERP
      _RL tmp_i(1000,NUM_INTERP_POINTS)
      _RL tmp_j(1000,NUM_INTERP_POINTS)
      _RL tmp_weights(1000,NUM_INTERP_POINTS),tmp_sum_weights
      _RL tmp_xC11(1000),tmp_yC11(1000)
      _RL tmp_xCNINJ(1000),tmp_yCNINJ(1000)
      INTEGER stopGenericGrid
      Real*8 xy_buffer_r8(0:sNx+1,0:sNy+1)
      INTEGER vec_start2(2), vec_count2(2)
      INTEGER hh, ProfNo_hh
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      INTEGER varId4
      _RL tmp_avgbin(1000)
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// insitu profiles model sampling >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )

      stopProfiles = 0
      stopGenericGrid = 0

      IF ( (.NOT.profilesDoGenGrid) .AND.
     &     (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &   'profilesDoGenGrid=.true. is required'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &   'unless usingSphericalGrid=.TRUE. and rotateGrid=.FALSE.'
        CALL PRINT_ERROR( msgBuf , myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') 'general packages parameters :'

      JL = ILNBLNK( profilesDir )
      IF (JL.NE.0) THEN
        WRITE(msgBuf,'(2A)') '  profilesDir ', profilesDir(1:JL)
      ELSE
        WRITE(msgBuf,'(2A)') '  profilesDir ','./'
      ENDIF

      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,L5)') '  profilesDoGenGrid  ',
     & profilesDoGenGrid
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,L5)') '  profilesDoNcOutput ',
     & profilesDoNcOutput
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )

      _BEGIN_MASTER( myThid )

      DO bj = 1, nSy
        DO bi = 1, nSx
          profiles_curfile_buff(bi,bj) = 0
          yymmddMin=modelstartdate(1)
          yymmddMax=modelenddate(1)
          hhmmssMin=modelstartdate(2)
          hhmmssMax=modelenddate(2)

          DO kLev = 1, NLEVELMAX
            DO c = 1, 1000
              DO num_var = 1, NVARMAX
                profiles_data_buff(kLev,c,num_var,bi,bj) = 0. _d 0
                profiles_weight_buff(kLev,c,num_var,bi,bj) = 0. _d 0

              ENDDO
            ENDDO
          ENDDO

          DO num_file = 1, NFILESPROFMAX
            ProfNo_hh=0
            profilesfile=' '
            IL = ILNBLNK( profilesfiles(num_file) )
            IF (IL.NE.0) THEN
               WRITE(profilesfile,'(A)') profilesfiles(num_file)(1:IL)
               WRITE(msgBuf,'(A)') ' '
               CALL PRINT_MESSAGE( msgBuf,
     &              standardMessageUnit, SQUEEZE_RIGHT, myThid )
               WRITE(msgBuf,'(A,I3,2A)') 'profiles file #', num_file,
     &          ' is ', profilesfile(1:IL)
               CALL PRINT_MESSAGE( msgBuf,
     &              standardMessageUnit, SQUEEZE_RIGHT, myThid )
            ENDIF

            IL = ILNBLNK( profilesfile )
            IF (IL.NE.0) THEN
C===========================================================
C Open data files and read information
C===========================================================
              WRITE(fnamedatanc,'(2A)') profilesfile(1:IL),'.nc'
              err = NF_OPEN( fnamedatanc, NF_NOWRITE,
     &               fiddata(num_file,bi,bj) )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_OPEN fiddata',err,bi,bj,myThid )

C1) Read the number of profiles and available dimensions:
              fid = fiddata(num_file,bi,bj)

              err = NF_INQ_DIMID( fid, 'iPROF', dimId )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_DIMID iPROF',err,bi,bj,myThid )
              err = NF_INQ_DIMLEN( fid,
     &              dimId, ProfNo(num_file,bi,bj) )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_DIMLEN ProfNo',
     &             err,bi,bj,myThid )

              err = NF_INQ_DIMID( fid, 'iDEPTH', dimId )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_DIMID iDEPTH',
     &             err,bi,bj,myThid )
              IF (err.NE.NF_NOERR) THEN
                err = NF_INQ_DIMID( fid, 'Z', dimId )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_DIMID Z',err,bi,bj,myThid )

              ENDIF

              err = NF_INQ_DIMLEN( fid, dimId,
     &              ProfDepthNo(num_file,bi,bj) )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_DIMLEN ProfDepthNo',
     &             err,bi,bj,myThid )

              err = NF_INQ_DIMID( fid, 'iINTERP', dimId )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_DIMID iINTERP',
     &             err,bi,bj,myThid )
              IF (err.EQ.NF_NOERR) THEN
                err = NF_INQ_DIMLEN( fid, dimId, iINTERP )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_DIMLEN iINTERP',
     &               err,bi,bj,myThid )

              ELSE
                IF (debugLevel .GE. debLevA) THEN
                  WRITE(msgBuf,'(3A,I3)')
     &             'S/R PROFILES_INIT_FIXED: ',
     &             'no iINTERP dim in data file using iINTERP ',
     &             '= NUM_INTERP_POINTS =', NUM_INTERP_POINTS
                  CALL PRINT_MESSAGE( msgBuf,
     &                 standardMessageUnit, SQUEEZE_RIGHT, myThid )

                ENDIF

                iINTERP = NUM_INTERP_POINTS

              ENDIF

              WRITE(msgBuf,'(2(A,I4))')
     &           '  current tile is bi,bj                      =',
     &           bi,',',bj
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT, myThid )
              WRITE(msgBuf,'(A,I9)')
     &           '  # of depth levels in file                  =',
     &           ProfDepthNo(num_file,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT, myThid )
              WRITE(msgBuf,'(A,I9)')
     &           '  # of profiles in file                      =',
     &           ProfNo(num_file,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT, myThid )

C2) Read dates and positions:
              err = NF_INQ_VARID( fid,'prof_depth', varId1a )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_VARID prof_depth',
     &             err,bi,bj,myThid )
              IF (err.NE.NF_NOERR) THEN
C LEGACY: Try old variable name: depth
                err = NF_INQ_VARID( fid,'depth', varId1a )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID depth',
     &               err,bi,bj,myThid )

              ENDIF

              IF (err.NE.NF_NOERR) THEN
C If neither is found, then stop
                IL = ILNBLNK( profilesfile )
                WRITE(msgBuf,'(4A)')
     &           'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &           '.nc is not in the pkg/profiles format',
     &           ' (no prof_depth etc.)'
                CALL PRINT_ERROR( msgBuf, myThid )

                stopProfiles = 1

              ENDIF

              DO kProf = 1, ProfDepthNo(num_file,bi,bj)
                err = NF_GET_VAR1_DOUBLE( fid, varId1a, kProf,
     &                prof_depth(num_file,kProf,bi,bj) )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_VAR1_DOUBLE prof_depth',
     &               err,bi,bj,myThid )

              ENDDO

C Get time, lon, lat varIds
              err = NF_INQ_VARID( fid,'prof_YYYYMMDD', varId1a )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_VARID prof_YYYYMMDD',
     &             err,bi,bj,myThid )
              err = err + NF_INQ_VARID( fid,'prof_HHMMSS', varId1b )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_VARID prof_HHMMSS',
     &             err,bi,bj,myThid )
              err = err + NF_INQ_VARID( fid,'prof_lon', varId2 )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_VARID prof_lon',
     &             err,bi,bj,myThid )
              err = err + NF_INQ_VARID( fid,'prof_lat', varId3 )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_VARID prof_lat',
     &             err,bi,bj,myThid )
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
              err = NF_INQ_VARID( fid,'prof_bin_id_a', varId4 )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_INQ_VARID prof_bin_id_a',
     &             err,bi,bj,myThid )
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

              IF (err.NE.NF_NOERR) THEN
                IL = ILNBLNK( profilesfile )
                WRITE(msgBuf,'(4A)')
     &           'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &           '.nc not in pkg/profiles format',
     &           ' (no prof_YYYYMMDD etc.)'
                CALL PRINT_ERROR( msgBuf, myThid )

                stopProfiles = 1

              ENDIF

              IF (profilesDoGenGrid) THEN
C3) Read interpolation information (grid points, coeffs, etc.)
                err = NF_INQ_VARID( fid,'prof_interp_XC11',
     &                varId_intp1 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_XC11',
     &               err,bi,bj,myThid )
                err = err + NF_INQ_VARID( fid,'prof_interp_YC11',
     &                varId_intp2 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_YC11',
     &               err,bi,bj,myThid )
                err = err + NF_INQ_VARID( fid,'prof_interp_XCNINJ',
     &                varId_intp11 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_XCNINJ',
     &               err,bi,bj,myThid )
                err = err + NF_INQ_VARID( fid,'prof_interp_YCNINJ',
     &                varId_intp22 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_YCNINJ',
     &               err,bi,bj,myThid )
                err = err + NF_INQ_VARID( fid,'prof_interp_weights',
     &                varId_intp3 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_weights',
     &               err,bi,bj,myThid )
                err = err + NF_INQ_VARID( fid,'prof_interp_i',
     &                varId_intp4 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_i',
     &               err,bi,bj,myThid )
                err = err + NF_INQ_VARID( fid,'prof_interp_j',
     &                varId_intp5 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_interp_j',
     &               err,bi,bj,myThid )

                IF (err.NE.NF_NOERR) THEN
                  IL = ILNBLNK( profilesfile )
                  WRITE(msgBuf,'(4A)')
     &             'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &             '.nc missing interpolation information',
     &             ' (profilesDoGenGrid)'
                  CALL PRINT_ERROR( msgBuf, myThid )

                  stopGenericGrid = 2

                ENDIF

              ENDIF !IF (profilesDoGenGrid)

C4) Default values
              DO prof_num = 1, NOBSGLOB
                prof_time(num_file,prof_num,bi,bj) = -999. _d 0
                prof_lon(num_file,prof_num,bi,bj) = -999. _d 0
                prof_lat(num_file,prof_num,bi,bj) = -999. _d 0
                prof_ind_glob(num_file,prof_num,bi,bj) = 0
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                prof_ind_avgbin(num_file,prof_num,bi,bj) = -999
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

                DO q = 1, NUM_INTERP_POINTS
                  prof_interp_i(num_file,prof_num,q,bi,bj) = 1
                  prof_interp_j(num_file,prof_num,q,bi,bj) = 1
                  prof_interp_weights(num_file,prof_num,q,bi,bj) =
     &             0. _d 0

                ENDDO

                prof_interp_xC11(num_file,prof_num,bi,bj) = -999. _d 0
                prof_interp_yC11(num_file,prof_num,bi,bj) = -999. _d 0
                prof_interp_xCNINJ(num_file,prof_num,bi,bj) =
     &           -999. _d 0
                prof_interp_yCNINJ(num_file,prof_num,bi,bj) =
     &           -999. _d 0

              ENDDO !DO prof_num

C5) Main loop: look for profiles in this bi,bj tile
              ProfNo_tile = 0
              profNo_div1000 = MAX(0,INT(ProfNo(num_file,bi,bj)/1000))

              DO chunkProf = 1, profno_div1000+1
C5.1) Read a chunk
                chunk = 1000*(chunkProf-1)

                IF (MIN(ProfNo(num_file,bi,bj), 1000*chunkProf).GE.
     &              1+chunk) THEN
                  vec_start(1) = 1
                  vec_start(2) = 1+chunk
                  vec_count(1) = 1
                  vec_count(2) = MIN(1000,
     &                               ProfNo(num_file,bi,bj)-chunk)

                  IF ( (vec_count(2).LE.0) .OR.
     &                 (vec_count(2).GT.1000) .OR.
     &                 (vec_start(2).LE.0) .OR.
     &                 (vec_count(2)+vec_start(2)-1.GT.
     &                  ProfNo(num_file,bi,bj)) ) THEN
                   IL  = ILNBLNK( profilesfile )
                   WRITE(msgBuf,'(3A)')
     &              'PROFILES_INIT_FIXED: file ',profilesfile(1:IL),
     &              '.nc was not read properly (case 1).'
                   CALL PRINT_ERROR( msgBuf, myThid )

                   stopProfiles = 1

                  ENDIF

                  err = NF_GET_VARA_DOUBLE( fid, varId1a,
     &                  vec_start(2),vec_count(2), tmpyymmdd )
                  CALL PROFILES_NF_ERROR(
     &                 'INIT_FIXED: NF_GET_VARA_DOUBLE tmpyymmdd',
     &                 err,bi,bj,myThid )
                  err = err + NF_GET_VARA_DOUBLE( fid, varId1b,
     &                        vec_start(2), vec_count(2), tmphhmmss )
                  CALL PROFILES_NF_ERROR(
     &                 'INIT_FIXED: NF_GET_VARA_DOUBLE tmphhmmss',
     &                 err,bi,bj,myThid )
                  err = err + NF_GET_VARA_DOUBLE( fid, varId2,
     &                        vec_start(2),vec_count(2), tmp_lon2 )
                  CALL PROFILES_NF_ERROR(
     &                 'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_lon2',
     &                 err,bi,bj,myThid )
                  err = err + NF_GET_VARA_DOUBLE( fid, varId3,
     &                        vec_start(2),vec_count(2), tmp_lat2 )
                  CALL PROFILES_NF_ERROR(
     &                 'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_lat2',
     &                 err,bi,bj,myThid )
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                  err = err + NF_GET_VARA_DOUBLE( fid, varId4,
     &                        vec_start(2),vec_count(2), tmp_avgbin )
                  CALL PROFILES_NF_ERROR(
     &                 'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_avgbin',
     &                 err,bi,bj,myThid )
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

                  IF (err.NE.NF_NOERR) THEN
                    WRITE(msgBuf,'(3A)')
     &               'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &               '.nc get_vara_double (case 2).'
                    CALL PRINT_ERROR( msgBuf, myThid )

                    stopProfiles = 1

                  ENDIF

C If profilesDoGenGrid, then read interpolation coeffs and indices
                  IF (profilesDoGenGrid) THEN
                    err = NF_GET_VARA_DOUBLE( fid, varId_intp1,
     &                    vec_start(2),vec_count(2), tmp_xC11 )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_xC11',
     &                   err,bi,bj,myThid )
                    err = NF_GET_VARA_DOUBLE( fid, varId_intp2,
     &                    vec_start(2),vec_count(2), tmp_yC11 )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_yC11',
     &                   err,bi,bj,myThid )
                    err = NF_GET_VARA_DOUBLE( fid, varId_intp11,
     &                    vec_start(2),vec_count(2), tmp_xCNINJ )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_xCNINJ',
     &                   err,bi,bj,myThid )
                    err = NF_GET_VARA_DOUBLE( fid, varId_intp22,
     &                    vec_start(2),vec_count(2), tmp_yCNINJ )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_yCNINJ',
     &                   err,bi,bj,myThid )

                    DO q = 1, iINTERP
                      vec_start2(1) = q
                      vec_start2(2) = 1+chunk
                      vec_count2(1) = 1
                      vec_count2(2) = MIN(1000,
     &                                ProfNo(num_file,bi,bj)-chunk)

                      err = NF_GET_VARA_DOUBLE( fid, varId_intp3,
     &                      vec_start2,vec_count2, tmp_weights(1,q) )
                      CALL PROFILES_NF_ERROR(
     &                  'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_weights',
     &                  err,bi,bj,myThid )
                      err = NF_GET_VARA_DOUBLE( fid, varId_intp4,
     &                      vec_start2,vec_count2, tmp_i(1,q) )
                      CALL PROFILES_NF_ERROR(
     &                     'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_i',
     &                     err,bi,bj,myThid )
                      err = NF_GET_VARA_DOUBLE(fid, varId_intp5,
     &                      vec_start2,vec_count2, tmp_j(1,q) )
                      CALL PROFILES_NF_ERROR(
     &                     'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_j',
     &                     err,bi,bj,myThid )

                    ENDDO

                  ENDIF !IF (profilesDoGenGrid)

C5.2) Loop through this chunk
                  DO c = 1, MIN(1000,ProfNo(num_file,bi,bj)-chunk)
                    IF (stopProfiles.EQ.0) THEN

                      profIsInRunTime = 1

                      IF (( (tmpyymmdd(c).GT.yymmddMin).OR.
     &                      ((tmpyymmdd(c).EQ.yymmddMin) .AND.
     &                       (tmphhmmss(c).GT.hhmmssMin)) ) .AND.
     &                    ( (tmpyymmdd(c).LT.yymmddMax).OR.
     &                      ((tmpyymmdd(c).EQ.yymmddMax) .AND.
     &                       (tmphhmmss(c).LT.hhmmssMax)) )) THEN
                        hh = INT(tmphhmmss(c))/10000
                        IF (hh.LT.hoursPerDay) THEN
                          profIsInRunTime = 1

                          CALL CAL_FULLDATE( INT(tmpyymmdd(c)),
     &                         INT(tmphhmmss(c)),tmpdate,myThid )
                          CALL CAL_TIMEPASSED( modelstartdate,tmpdate,
     &                         tmpdiff,myThid )
                          CALL CAL_TOSECONDS(
     &                         tmpdiff,diffsecs,myThid )

                          diffsecs = diffsecs+nIter0*deltaTClock

                        ELSE
C If tmp hhmmss is out of range then disregard profile
                          profIsInRunTime = 0
                          diffsecs = -deltaTClock
                          ProfNo_hh = ProfNo_hh+1
                        ENDIF
                      ELSE
                        profIsInRunTime = 0
                        diffsecs = -deltaTClock
                      ENDIF !IF (( (tmpyymmdd(c)

C 5.2a) Determine profiles in current tile domain (lat-lon grid case)
                      IF ( (.NOT.profilesDoGenGrid) .AND.
     &                     (profIsInRunTime.EQ.1) ) THEN
                        IF (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) THEN
                          tmp_lon = xC(sNx+1,1,bi,bj)+360. _d 0
                        ELSE
                          tmp_lon = xC(sNx+1,1,bi,bj)
                        ENDIF

                        IF ( (xC(1,1,bi,bj).LE.tmp_lon2(c)) .AND.
     &                       (tmp_lon.GT.tmp_lon2(c)) .AND.
     &                       (yC(1,1,bi,bj).LE.tmp_lat2(c)) .AND.
     &                       (yC(1,sNy+1,bi,bj).GT.tmp_lat2(c)) ) THEN
                         lon_cur = tmp_lon2(c)
                         lat_cur = tmp_lat2(c)
                        ELSEIF ( (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj))
     &                     .AND. (xC(1,1,bi,bj).LE.
     &                            tmp_lon2(c)+360. _d 0) .AND.
     &                           (tmp_lon.GT.tmp_lon2(c)+360. _d 0)
     &                     .AND. (yC(1,1,bi,bj).LE.tmp_lat2(c)) .AND.
     &                           (yC(1,sNy+1,bi,bj).GT.tmp_lat2(c)) )
     &                  THEN
                         lon_cur = tmp_lon2(c)+360. _d 0
                         lat_cur = tmp_lat2(c)

                        ELSE
                          profIsInRunTime = 0

                        ENDIF

C Determine value of i,j to the south-ouest of data point
                        prof_i = -10
                        prof_j = -10
                        lon_1 = -10
                        lon_2 = -10
                        lat_1 = -10
                        lat_2 = -10

                        IF (profIsInRunTime.EQ.1) THEN
                          DO j = 1, sNy+1
                            DO i = 1, sNx+1
C Value of j, south of the data point:
                              IF ( (yC(i,j,bi,bj).LE.lat_cur) .AND.
     &                             (yC(i,j+1,bi,bj).GT.lat_cur) )
     &                        THEN
                                prof_j = j
                                lat_1 = yC(i,j,bi,bj)
                                lat_2 = yC(i,j+1,bi,bj)
                              ENDIF

C Value of i, west of the data point:
                              IF (xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj))
     &                        THEN
                                lon_tmp2 = xC(i+1,j,bi,bj)+360
                              ELSE
                                lon_tmp2 = xC(i+1,j,bi,bj)
                              ENDIF
                              IF (xC(i,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
                                lon_tmp1 = xC(i,j,bi,bj)+360
                              ELSE
                                lon_tmp1 = xC(i,j,bi,bj)
                              ENDIF

                              IF ((lon_tmp1.LE.lon_cur) .AND.
     &                            (lon_tmp2.GT.lon_cur)) THEN
                                prof_i = i
                                lon_1 = lon_tmp1
                                lon_2 = lon_tmp2
                              ENDIF

                            ENDDO !DO i
                          ENDDO !DO j
                        ENDIF !IF (profIsRunTime.EQ.1)

                        IF ((prof_i.EQ.-10).OR.(prof_j.EQ.-10)) THEN
                          profIsInRunTime = 0
                        ENDIF

                        IF (profIsInRunTime.EQ.1) THEN
C If yes then store prof_time and longitude and latitude:
                          ProfNo_tile = ProfNo_tile+1
                          prof_time(num_file,ProfNo_tile,bi,bj) =
     &                     diffsecs

                          prof_lon(num_file,ProfNo_tile,bi,bj) =
     &                     lon_cur
                          prof_lat(num_file,ProfNo_tile,bi,bj) =
     &                     lat_cur
                          prof_ind_glob(num_file,ProfNo_tile,bi,bj) =
     &                     c+chunk
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                          prof_ind_avgbin(num_file,
     &                     ProfNo_tile,bi,bj) = tmp_avgbin(c)
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */
C Then store interpolation coeffs and indices
                          lon_fac = (lon_cur-lon_1)/(lon_2-lon_1)
                          lat_fac = (lat_cur-lat_1)/(lat_2-lat_1)

                          prof_interp_weights(num_file,ProfNo_tile,1,
     &                     bi,bj) = (1-lon_fac)*(1-lat_fac)
                          prof_interp_i(num_file,ProfNo_tile,1,
     &                     bi,bj) = prof_i
                          prof_interp_j(num_file,ProfNo_tile,1,
     &                     bi,bj) = prof_j

                          prof_interp_weights(num_file,ProfNo_tile,2,
     &                     bi,bj) = lon_fac*(1-lat_fac)
                          prof_interp_i(num_file,ProfNo_tile,2,
     &                     bi,bj) = prof_i+1
                          prof_interp_j(num_file,ProfNo_tile,2,
     &                     bi,bj) = prof_j

                          prof_interp_weights(num_file,ProfNo_tile,3,
     &                     bi,bj) = (1-lon_fac)*lat_fac
                          prof_interp_i(num_file,ProfNo_tile,3,
     &                     bi,bj) = prof_i
                          prof_interp_j(num_file,ProfNo_tile,3,
     &                     bi,bj) = prof_j+1

                          prof_interp_weights(num_file,ProfNo_tile,4,
     &                     bi,bj) = lon_fac*lat_fac
                          prof_interp_i(num_file,ProfNo_tile,4,
     &                     bi,bj) = prof_i+1
                          prof_interp_j(num_file,ProfNo_tile,4,
     &                     bi,bj) = prof_j+1

                        ENDIF !IF (profIsRunTime.EQ.1)

C 5.2a) Determine profiles in current tile domain (gen grid case)
                      ELSEIF (profIsInRunTime.EQ.1) THEN
                        IF (stopGenericGrid.EQ.0) THEN
                          IF (
     &  ( ABS(tmp_xC11(c)-xC(1,1,bi,bj)).LT.0.0001 _d 0 ) .AND.
     &  ( ABS(tmp_yC11(c)-yC(1,1,bi,bj)).LT.0.0001 _d 0 ) .AND.
     &  ( ABS(tmp_xCNINJ(c)-xC(sNx,sNy,bi,bj)).LT.0.0001 _d 0 ) .AND.
     &  ( ABS(tmp_yCNINJ(c)-yC(sNx,sNy,bi,bj)).LT.0.0001 _d 0 ) .AND.
     &  ( profIsInRunTime.EQ.1 ) ) THEN
C If yes then store prof_time and interpolation coeffs and indices:
                            ProfNo_tile = ProfNo_tile+1
                            prof_time(num_file,ProfNo_tile,bi,bj) =
     &                       diffsecs
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                            prof_ind_avgbin(num_file,
     &                       ProfNo_tile,bi,bj) = tmp_avgbin(c)
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */
                            prof_interp_xC11(num_file,ProfNo_tile,
     &                       bi,bj) = tmp_xC11(c)
                            prof_interp_yC11(num_file,ProfNo_tile,
     &                       bi,bj) = tmp_yC11(c)
                            prof_interp_xCNINJ(num_file,ProfNo_tile,
     &                       bi,bj) = tmp_xCNINJ(c)
                            prof_interp_yCNINJ(num_file,ProfNo_tile,
     &                       bi,bj) = tmp_yCNINJ(c)
                            tmp_sum_weights = 0. _d 0

                            DO q = 1, iINTERP
                              prof_interp_weights(num_file,
     &                         ProfNo_tile,q,bi,bj) = tmp_weights(c,q)
                              prof_interp_i(num_file,ProfNo_tile,
     &                         q,bi,bj) = tmp_i(c,q)
                              prof_interp_j(num_file,ProfNo_tile,
     &                         q,bi,bj) = tmp_j(c,q)
                              tmp_sum_weights = tmp_sum_weights
     &                         + tmp_weights(c,q)

C More test of the inputs: is the offline-computed
C Interpolation information consistent (self and with grid)
                              IF ( (tmp_i(c,q).LT.0) .OR.
     &                             (tmp_j(c,q).LT.0) .OR.
     &                             (tmp_i(c,q).GT.sNx+1) .OR.
     &                             (tmp_j(c,q).GT.sNy+1) ) THEN
                                WRITE(msgBuf,'(4A)')
     &                           'PROFILES_INIT_FIXED: file ',
     &                           profilesfile(1:IL),
     &                           '.nc has inconsistent interp ',
     &                           '(profilesDoGenGrid; out of tile)'
                                CALL PRINT_ERROR( msgBuf, myThid )

                                stopGenericGrid = 1

                              ENDIF

# ifdef ALLOW_PROFILES_EXCLUDE_CORNERS
                              IF (tmp_weights(c,q).NE.0. _d 0) THEN
                                IF (
     &        ((tmp_i(c,q).EQ.0).AND.(tmp_j(c,q).EQ.0)) .OR.
     &        ((tmp_i(c,q).EQ.sNx+1).AND.(tmp_j(c,q).EQ.sNy+1)) .OR.
     &        ((tmp_i(c,q).EQ.0).AND.(tmp_j(c,q).EQ.sNy+1)) .OR.
     &        ((tmp_i(c,q).EQ.sNx+1).AND.(tmp_j(c,q).EQ.0)) ) THEN
                                  WRITE(msgBuf,'(5A)')
     &                             'PROFILES_INIT_FIXED: file ',
     &                             profilesfile(1:IL),
     &                             '.nc has inconsistent interp',
     &                             '(profilesDoGenGrid; overlapping',
     &                             'corner)'
                                  CALL PRINT_ERROR( msgBuf, myThid )

                                  stopGenericGrid = 1

                               ENDIF
                              ENDIF

# endif /* ALLOW_PROFILES_EXCLUDE_CORNERS */
                              IF ( (tmp_weights(c,q).LT.0. _d 0) .OR.
     &                             (tmp_weights(c,q).GT.1. _d 0) )
     &                        THEN
                                WRITE(msgBuf,'(5A)')
     &                           'PROFILES_INIT_FIXED: file ',
     &                           profilesfile(1:IL),
     &                           '.nc has inconsistent interp',
     &                           'weights (profilesDoGenGrid; ',
     &                           'sum oustide 0-1)'
                                CALL PRINT_ERROR( msgBuf, myThid)

                                stopGenericGrid = 1

                              ENDIF

                            ENDDO !DO q

                            IF (ABS(tmp_sum_weights -1. _d 0).GT.
     &                          0.0001 _d 0) THEN
                              WRITE(msgBuf,'(4A)')
     &                         'PROFILES_INIT_FIXED: file ',
     &                         profilesfile(1:IL),
     &                         '.nc has inconsistent interp weights',
     &                         ' (profilesDoGenGrid; dont add to 1)'
                              CALL PRINT_ERROR( msgBuf, myThid)

                              stopGenericGrid = 1

                            ENDIF

                            prof_ind_glob(num_file,
     &                       ProfNo_tile,bi,bj) = c + chunk

                          ENDIF !IF ( ( ABS(tmp_xC11(c)...
                        ENDIF !IF (stopGenericGrid.EQ.0)
                      ENDIF !IF (.NOT.profilesDoGenGrid ... ELSEIF ...

C Check that maximum size was not reached:
                      IF (ProfNo_tile.GE.NOBSGLOB) THEN
                        WRITE(msgBuf,'(3A)')
     &                   'PROFILES_INIT_FIXED: file ',
     &                   profilesfile(1:IL),
     &                   '.nc not read properly (increase NOBSGLOB).'
                        CALL PRINT_ERROR( msgBuf, myThid)

                        stopProfiles = 1

                      ENDIF

                    ENDIF !IF (stopProfiles .EQ. 0)
                  ENDDO !DO c

                ENDIF !IF (MIN(ProfNo(num_file,bi,bj)
              ENDDO !DO chunkProf

              ProfNo(num_file,bi,bj) = ProfNo_tile

              WRITE(msgBuf,'(A,I9)')
     &         '  # of profiles with erroneous HHMMSS values =',
     &         ProfNo_hh
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT, myThid )

              WRITE(msgBuf,'(A,I9)')
     &         '  # of profiles within tile and time period  =',
     &         ProfNo(num_file,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT, myThid )

C6) Available variables in the data set
              DO num_var = 1, NVARMAX
                prof_num_var_cur(num_file,num_var,bi,bj) = 0
              ENDDO
              prof_num_var_tot(num_file,bi,bj) = 0

              DO num_var = 1, NVARMAX
                JL = ILNBLNK( prof_names(num_file,num_var) )
                err = NF_INQ_VARID( fid,
     &                prof_names(num_file,num_var)(1:JL), varId1 )
                CALL PROFILES_NF_ERROR(
     &               'INIT_FIXED: NF_INQ_VARID prof_names',
     &               err,bi,bj,myThid )
                IF (err.EQ.NF_NOERR) THEN
                  vec_quantities(num_file,num_var,bi,bj) = .TRUE.

                  prof_num_var_tot(num_file,bi,bj) =
     &             prof_num_var_tot(num_file,bi,bj) + 1
                  prof_num_var_cur(num_file,num_var,bi,bj) =
     &             prof_num_var_tot(num_file,bi,bj)

                ELSE
                  IF (debugLevel .GE. debLevA) THEN
                   WRITE(msgBuf,'(3A)') 'S/R PROFILES_INIT_FIXED: no',
     &                  prof_names(num_file,num_var)(1:JL),
     &                  ', setting corresponding vec_quantities = F'
                   CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                                 SQUEEZE_RIGHT, myThid )

                  ENDIF

                  vec_quantities(num_file,num_var,bi,bj) = .FALSE.

                ENDIF

                IF (vec_quantities(num_file,num_var,bi,bj)) THEN
                  KL = ILNBLNK( prof_names(num_file,num_var) )
                  JL = ILNBLNK( prof_namesmod(num_file,num_var) )

                  IF (prof_namesmod(num_file,num_var).EQ.'pTracer')
     &            THEN
                    WRITE(msgBuf,'(A,I3,5A,I3)') '  variable #',
     &               num_var,' is ' ,
     &               prof_names(num_file,num_var)(1:KL),' and ',
     &               prof_namesmod(num_file,num_var)(1:JL),' #',
     &               prof_itracer(num_file,num_var)
                  ELSE
                   WRITE(msgBuf,'(A,I3,4A)') '  variable #',
     &              num_var,' is            ' ,
     &              prof_names(num_file,num_var)(1:KL),' and ',
     &              prof_namesmod(num_file,num_var)(1:JL)
                  ENDIF
                  CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                                SQUEEZE_RIGHT, myThid )

                ENDIF !IF (vec_quantities

              ENDDO !DO num_var

C===========================================================
C Create files for model counterparts to observations
C===========================================================

              IF (ProfNo(num_file,bi,bj).GT.0) THEN
                iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
                jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
                JL = ILNBLNK( profilesDir )

                IF (profilesDoNcOutput) THEN
                  WRITE(fnameequinc,'(3A,2(I3.3,A))')
     &             profilesDir(1:JL),profilesfile(1:IL),
     &             '.',iG,'.',jG,'.equi.nc'
# ifdef ALLOW_ADJOINT_RUN
                  WRITE(adfnameequinc,'(4A,2(I3.3,A))')
     &             profilesDir(1:JL),'ad',profilesfile(1:IL),
     &             '.',iG,'.',jG,'.equi.nc'
# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
                  WRITE(tlfnameequinc,'(4A,2(I3.3,A))')
     &             profilesDir(1:JL),'tl',profilesfile(1:IL),
     &             '.',iG,'.',jG,'.equi.nc'
# endif /* ALLOW_TANGENTLINEAR_RUN */

                  JL = ILNBLNK( fnameequinc )
                  INQUIRE( FILE = fnameequinc(1:JL), EXIST = exst )
                  IF (.NOT.exst) THEN
                    CALL PROFILES_INIT_NCFILE( num_file,
     &                   fiddata(num_file,bi,bj),fnameequinc(1:JL),
     &                   fidforward(num_file,bi,bj),
     &                   ProfNo(num_file,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),
     &                   bi,bj,myThid )
                  ELSE
C Obtain existing NetCDF file id
                    err = NF_OPEN( fnameequinc(1:JL), NF_WRITE,
     &                    fidforward(num_file,bi,bj) )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_OPEN fidforward',
     &                   err,bi,bj,myThid )
                  ENDIF

# ifdef ALLOW_ADJOINT_RUN
                  JL = ILNBLNK( adfnameequinc )
                  INQUIRE( FILE = adfnameequinc(1:JL), EXIST = exst )
                  IF (.NOT.exst) THEN
                    CALL PROFILES_INIT_NCFILE( num_file,
     &                   fiddata(num_file,bi,bj),adfnameequinc(1:JL),
     &                   fidadjoint(num_file,bi,bj),
     &                   ProfNo(num_file,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),
     &                   bi,bj,myThid )
                  ELSE
C Obtain existing NetCDF file id
                    err = NF_OPEN( adfnameequinc(1:JL), NF_WRITE,
     &                     fidadjoint(num_file,bi,bj) )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_OPEN fidadjoint',
     &                   err,bi,bj,myThid )
                  ENDIF

# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
                  JL = ILNBLNK( tlfnameequinc )
                  INQUIRE( FILE = tlfnameequinc(1:JL), EXIST = exst )
                  IF (.NOT.exst) THEN
                    CALL PROFILES_INIT_NCFILE( num_file,
     &                   fiddata(num_file,bi,bj),tlfnameequinc(1:JL),
     &                   fidtangent(num_file,bi,bj),
     &                   ProfNo(num_file,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),
     &                   bi,bj,myThid )
                  ELSE
C Obtain existing NetCDF file id
                    err = NF_OPEN( tlfnameequinc(1:JL), NF_WRITE,
     &                     fidtangent(num_file,bi,bj) )
                    CALL PROFILES_NF_ERROR(
     &                   'INIT_FIXED: NF_OPEN fidtangent',
     &                   err,bi,bj,myThid )
                  ENDIF

# endif /* ALLOW_TANGENTLINEAR_RUN */
                ELSE !IF (profilesDoNcOutput
                  WRITE(fnameequinc,'(3A,2(I3.3,A))')
     &             profilesDir(1:JL),profilesfile(1:IL),
     &             '.',iG,'.',jG,'.equi.data'

# ifdef ALLOW_ADJOINT_RUN
                  WRITE(adfnameequinc,'(4A,2(I3.3,A))')
     &             profilesDir(1:JL),'ad',profilesfile(1:IL),
     &             '.',iG,'.',jG,'.equi.data'

# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
                  WRITE(tlfnameequinc,'(4A,2(I3.3,A))')
     &             profilesDir(1:JL),'tl',profilesfile(1:IL),
     &             '.',iG,'.',jG,'.equi.data'

# endif /* ALLOW_TANGENTLINEAR_RUN */

                  JL = ILNBLNK( fnameequinc )
                  INQUIRE( FILE = fnameequinc(1:JL), EXIST = exst )
# ifdef PROFILES_USE_MDSFINDUNITS
                  CALL MDSFINDUNIT( fidforward(num_file,bi,bj),
     &                 myThid )
# else
                  CALL PROFILES_FINDUNIT( fidforward(num_file,bi,bj),
     &                 myThid )
# endif
                  IF (.NOT.exst) THEN
                    CALL PROFILES_INIT_NCFILE( num_file,
     &                   fiddata(num_file,bi,bj),fnameequinc(1:JL),
     &                   fidforward(num_file,bi,bj),
     &                   ProfNo(num_file,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),
     &                   bi,bj,myThid )
                  ELSE
                    OPEN( fidforward(num_file,bi,bj),
     &               FILE = fnameequinc(1:JL), FORM = 'unformatted',
     &               STATUS = 'unknown', ACCESS = 'direct',
     &               RECL = (ProfDepthNo(num_file,bi,bj)+1)
     &                      *WORDLENGTH*2 )
                  ENDIF

# ifdef ALLOW_ADJOINT_RUN
                  JL = ILNBLNK( adfnameequinc )
                  INQUIRE( FILE = adfnameequinc(1:JL), EXIST = exst )
#  ifdef PROFILES_USE_MDSFINDUNITS
                  CALL MDSFINDUNIT( fidadjoint(num_file,bi,bj),
     &                 myThid )
#  else
                  CALL PROFILES_FINDUNIT( fidadjoint(num_file,bi,bj),
     &                 myThid )
#  endif
                  IF (.NOT.exst) THEN
                    CALL PROFILES_INIT_NCFILE( num_file,
     &                   fiddata(num_file,bi,bj),adfnameequinc(1:JL),
     &                   fidadjoint(num_file,bi,bj),
     &                   ProfNo(num_file,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),
     &                   bi,bj,myThid )
                  ELSE
                    OPEN( fidadjoint(num_file,bi,bj),
     &               FILE = adfnameequinc(1:JL), FORM = 'unformatted',
     &               STATUS = 'unknown', ACCESS = 'direct',
     &               RECL = (ProfDepthNo(num_file,bi,bj)+1)
     &                      *WORDLENGTH*2 )
                  ENDIF

# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
                  JL = ILNBLNK( tlfnameequinc )
                  INQUIRE( FILE = tlfnameequinc(1:JL), EXIST = exst )
#  ifdef PROFILES_USE_MDSFINDUNITS
                  CALL MDSFINDUNIT( fidtangent(num_file,bi,bj),
     &                 myThid )
#  else
                  CALL PROFILES_FINDUNIT( fidtangent(num_file,bi,bj),
     &                 myThid )
#  endif
                  IF (.NOT.exst) THEN
                    CALL PROFILES_INIT_NCFILE( num_file,
     &                   fiddata(num_file,bi,bj),tlfnameequinc(1:JL),
     &                   fidtangent(num_file,bi,bj),
     &                   ProfNo(num_file,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),
     &                   bi,bj,myThid )
                  ELSE
                    OPEN( fidtangent(num_file,bi,bj),
     &               FILE = tlfnameequinc(1:JL), FORM = 'unformatted',
     &               STATUS = 'unknown', ACCESS = 'direct',
     &               RECL = (ProfDepthNo(num_file,bi,bj)+1)
     &                      *WORDLENGTH*2 )
                  ENDIF

# endif /* ALLOW_TANGENTLINEAR_RUN */

                ENDIF !IF (profilesDoNcOutput

            ENDIF !IF (ProfNo(num_file,bi,bj).GT.0)

C ===========================================================
            ELSE !IF (IL.NE.0)
              ProfNo(num_file,bi,bj) = 0

              DO num_var = 1, NVARMAX
                prof_num_var_cur(num_file,num_var,bi,bj) = 0
                vec_quantities(num_file,num_var,bi,bj) = .FALSE.
              ENDDO

              prof_num_var_tot(num_file,bi,bj) = 0

              DO prof_num = 1, NOBSGLOB
                prof_time(num_file,prof_num,bi,bj) = -999. _d 0
                prof_lon(num_file,prof_num,bi,bj) = -999. _d 0
                prof_lat(num_file,prof_num,bi,bj) = -999. _d 0
                prof_ind_glob(num_file,prof_num,bi,bj) = 0
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                prof_ind_avgbin(num_file,prof_num,bi,bj) = -999
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

                DO q = 1, NUM_INTERP_POINTS
                  prof_interp_i(num_file,prof_num,q,bi,bj) = 1
                  prof_interp_j(num_file,prof_num,q,bi,bj) = 1
                  prof_interp_weights(num_file,prof_num,q,bi,bj) =
     &             0. _d 0
                ENDDO

                prof_interp_xC11(num_file,prof_num,bi,bj) = -999. _d 0
                prof_interp_yC11(num_file,prof_num,bi,bj) = -999. _d 0
                prof_interp_xCNINJ(num_file,prof_num,bi,bj) =
     &           -999. _d 0
                prof_interp_yCNINJ(num_file,prof_num,bi,bj) =
     &           -999. _d 0

              ENDDO !DO prof_num

            ENDIF !IF (IL.NE.0)

          ENDDO !DO num_file

# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
C Find unique depth levels from all profile datasets
C initialize prof_depth_comb
          IF (bi.EQ.1.AND.bj.EQ.1) THEN
            NLEVELCOMB = 0
            NLEVELCOMBRL = NLEVELCOMB
          ENDIF

          DO kCMax = 1, NLEVELCOMBMAX
            prof_depth_comb(kCMax,bi,bj) = -999. _d 0
          ENDDO

          m = 1
          DO num_file = 1, NFILESPROFMAX
            DO kProf = 1, ProfDepthNo(num_file,bi,bj)
              IF (m.EQ.1) THEN
                prof_depth_comb(m,bi,bj) =
     &           prof_depth(num_file,kProf,bi,bj)
                m = m + 1
C Sort
              ELSE !IF (m.EQ.1)
                DO kCMax = 1, NLEVELCOMBMAX-1
                  IF (prof_depth_comb(kCMax,bi,bj) .NE. -999. _d 0)
     &            THEN
                    IF ( prof_depth(num_file,kProf,bi,bj).LT.
     &                   prof_depth_comb(kCMax,bi,bj).AND.
     &                   kCMax.EQ.1 ) THEN
                     prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &                prof_depth_comb(kCMax,bi,bj)

                     prof_depth_comb(kCMax,bi,bj) =
     &                prof_depth(num_file,kProf,bi,bj)

                     DO kC = NLEVELCOMBMAX-1, kCMax+2, -1
                       prof_depth_comb(kC,bi,bj) =
     &                  prof_depth_comb(kC-1,bi,bj)

                     ENDDO !DO kC

                     prof_depth_comb(kCMax+1,bi,bj) =
     &                    prof_depth_comb(NLEVELCOMBMAX,bi,bj)

                    ELSEIF ( prof_depth(num_file,kProf,bi,bj).GT.
     &                       prof_depth_comb(kCMax,bi,bj) .AND.
     &                       prof_depth(num_file,kProf,bi,bj).LT.
     &                       prof_depth_comb(kCMax+1,bi,bj) ) THEN
                      prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &                 prof_depth_comb(kCMax+1,bi,bj)

                      prof_depth_comb(kCMax+1,bi,bj)i =
     &                 prof_depth(num_file, kProf,bi,bj)

                      DO kC = NLEVELCOMBMAX-1, kCMax+3, -1
                        prof_depth_comb(kC,bi,bj)=
     &                   prof_depth_comb(kC-1,bi,bj)

                      ENDDO !DO kC

                      prof_depth_comb(kCMax+2,bi,bj)=
     &                 prof_depth_comb(NLEVELCOMBMAX,bi,bj)

                    ELSEIF ( prof_depth(num_file,kProf,bi,bj).GT.
     &                       prof_depth_comb(kCMax,bi,bj) .AND.
     &                       prof_depth_comb(kCMax+1,bi,bj).EQ.
     &                        -999. _d 0 ) THEN
                     prof_depth_comb(kCMax+1,bi,bj) =
     &                    prof_depth(num_file, kProf,bi,bj)

                    ENDIF !IF (prof_depth(num_file,kProf,bi,bj).LT.
                  ENDIF !IF (prof_depth_comb(kCMax,bi,bj).NE.-999.
                ENDDO !DO kCMax

              ENDIF !IF (m.EQ.1)

              IF (m.GE.NLEVELCOMBMAX-2) THEN
                WRITE(msgBuf,'(A)') 'increase NLEVELCOMBMAX'
                CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF

            ENDDO ! DO kProf
          ENDDO ! DO num_file

          prof_depth_comb(NLEVELCOMBMAX,bi,bj) = -999. _d 0

C Diagnostics output
          DO kCMax = 1, NLEVELCOMBMAX
            IF ( prof_depth_comb(kCMax,bi,bj) .GE. 0. _d 0
     &          .AND. NLEVELCOMB.LT.kCMax ) THEN
              NLEVELCOMB = kCMax

              IF (kCMax.GE.NLEVELCOMBMAX-2) THEN
                WRITE(msgBuf,'(A,2I6)')
     &           'increase NLEVELCOMBMAX: kCMax, NLEVELCOMBMA  ',
     &           kCMax, NLEVELCOMBMAX
                CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF

            ENDIF !IF ( prof_depth_comb(m,bi,bj) .GE. 0. _d 0
          ENDDO !DO kCMax

          WRITE(msgBuf,'(A,I6,D20.5)') 'NLEVELCOMB = ', NLEVELCOMB
          CALL PRINT_MESSAGE( msgBuf,
     &         standardMessageUnit, SQUEEZE_RIGHT, myThid )
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

C===========================================================
C Error analysis:
C===========================================================

C1) Provide interpolation information
          IF (stopGenericGrid.EQ.2) THEN
            iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
            jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
C XC grid
            CALL MDSFINDUNIT( fid, myThid )
            WRITE(fnameequinc,'(A,2(I3.3,A),2(I4.4,A))')
     &       'profilesXCincl1PointOverlap.',iG,'.',jG,'.',
     &       sNx,'.',sNy,'.data'

            recLen = MDS_RECLEN( 64,(sNx+2)*(sNy+2),myThid )

            WRITE(standardMessageUnit,'(2A,/,A)')
     &       'PROFILES_INIT_FIXED: creating grid from profiles; ',
     &       'file:', fnameequinc
            JL  = ILNBLNK( fnameequinc )
            OPEN( fid, FILE = fnameequinc(1:JL), FORM = 'unformatted',
     &       STATUS = 'unknown', ACCESS = 'direct', RECL = recLen )

            DO j = 0, sNy+1
              DO i = 0, sNx+1
                xy_buffer_r8(i,j) = xC(i,j,bi,bj)
              ENDDO
            ENDDO

# ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( (sNx+2)*(sNy+2),xy_buffer_r8 )
# endif
            WRITE(fid,rec=1) xy_buffer_r8
            CLOSE(fid)
C YC grid
            CALL MDSFINDUNIT( fid, myThid )
            WRITE(fnameequinc,'(A,2(I3.3,A),2(I4.4,A))')
     &       'profilesYCincl1PointOverlap.',iG,'.',jG,'.',
     &       sNx,'.',sNy,'.data'

            recLen = MDS_RECLEN( 64,(sNx+2)*(sNy+2),myThid )

            WRITE(standardMessageUnit,'(2A,/,A)')
     &       'PROFILES_INIT_FIXED: creating grid from profiles; ',
     &       'file:', fnameequinc
            JL  = ILNBLNK( fnameequinc )
            OPEN( fid, FILE = fnameequinc(1:JL), FORM = 'unformatted',
     &       STATUS = 'unknown', ACCESS = 'direct', RECL = recLen )

            DO j = 0, sNy+1
              DO i = 0, sNx+1
                xy_buffer_r8(i,j) = yC(i,j,bi,bj)
              ENDDO
            ENDDO

# ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( (sNx+2)*(sNy+2),xy_buffer_r8 )
# endif
            WRITE(fid,rec=1) xy_buffer_r8
            CLOSE(fid)

            WRITE(msgBuf,'(3A)')
     &       'PROFILES_INIT_FIXED : ',
     &       'when using profilesDoGenGrid ',
     &       'you have to provide interpolation coeffs etc. '
            CALL PRINT_ERROR( msgBuf, myThid )

            WRITE(msgBuf,'(2A)')
     &       'and some of your nc files dont have them. ',
     &       'You could use profiles_prep_mygrid.m and/or'
            CALL PRINT_ERROR( msgBuf, myThid )

            WRITE(msgBuf,'(A)')
     &       'use the grid info in profiles*incl1PointOverlap*data'
            CALL PRINT_ERROR( msgBuf, myThid )

            stopProfiles = 1

          ENDIF !IF (stopGenericGrid.EQ.2)

        ENDDO !DO bi
      ENDDO !DO bj

# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      NLEVELCOMBRL = NLEVELCOMB
      _GLOBAL_MAX_RL( NLEVELCOMBRL, myThid )
      NLEVELCOMB = NLEVELCOMBRL
# endif

      _END_MASTER( myThid )
      _BARRIER

C2) Stop after other kind of errors
      CALL GLOBAL_SUM_INT( stopProfiles, myThid )
      IF ( stopProfiles.GE.1) THEN
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      CALL GLOBAL_SUM_INT( stopGenericGrid, myThid )
      IF ( stopGenericGrid.GE.1) THEN
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// insitu profiles model sampling >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_PROFILES */

      RETURN
      END
