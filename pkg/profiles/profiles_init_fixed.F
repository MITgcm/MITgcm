#include "PROFILES_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     BOP
C     !ROUTINE: PROFILES_INIT_FIXED

C     !INTERFACE:
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
C     EOP

#ifdef ALLOW_PROFILES
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     i,j,bi,bj    :: loop indices
C     kLev         :: vertical level index
C     prof_num, num_file, num_var
C                  :: loop indices for profiles, files, and variables
C     iG,jG        :: global indices
C     ic, chunkProf:: chunk-related indices
C     chunk        :: chunk size
C     ProfNo_tile  :: counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER kLev, prof_num
      INTEGER chunk, chunkProf, ic
      INTEGER i, j, bi, bj, iG, jG, num_file, num_var, ProfNo_tile
      INTEGER fid, dimId, varId1, varId1a, varId1b
      INTEGER varId2, varId3
      _RL tmpyymmdd(1000), tmphhmmss(1000), diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax

      INTEGER tmpdate(4), tmpdiff(4), profIsInRunTime
      _RL tmp_lon, tmp_lon2(1000), tmp_lat2(1000)
      _RL lon_cur, lat_cur
      _RL lon_1, lon_2, lat_1, lat_2
      _RL lon_tmp1, lon_tmp2
      _RL lat_fac, lon_fac
      INTEGER prof_i, prof_j
      INTEGER vec_start(2), vec_count(2), profno_div1000
      CHARACTER*(MAX_LEN_FNAM) profilesfile, fnamedatanc
      CHARACTER*(MAX_LEN_FNAM) fnameequinc
# ifdef ALLOW_ADJOINT_RUN
      CHARACTER*(MAX_LEN_FNAM) adfnameequinc
# endif
# ifdef ALLOW_TANGENTLINEAR_RUN
      CHARACTER*(MAX_LEN_FNAM) tlfnameequinc
# endif
      INTEGER IL, JL, KL, err, nerr
      LOGICAL exst, stopProfiles
      INTEGER stopProfiles_cnt
      INTEGER stopGenericGrid, stopGenericGrid_max

      INTEGER varId_intp1, varId_intp2, varId_intp11, varId_intp22
      INTEGER varId_intp3, varId_intp4, varId_intp5, iq, iINTERP
      _RL tmp_i(1000,NUM_INTERP_POINTS)
      _RL tmp_j(1000,NUM_INTERP_POINTS)
      _RL tmp_weights(1000,NUM_INTERP_POINTS), tmp_sum_weights
      _RL tmp_xC11(1000), tmp_yC11(1000)
      _RL tmp_xCNINJ(1000), tmp_yCNINJ(1000)
      INTEGER vec_start2(2), vec_count2(2)
      INTEGER hh, ProfNo_hh
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      INTEGER m, kC, kCMax
      INTEGER varId4
      _RL tmp_avgbin(1000)
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

      stopProfiles_cnt = 0
      stopGenericGrid_max = 0
      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     '// insitu profiles model sampling >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

      IF ( (.NOT.profilesDoGenGrid) .AND.
     &     (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
       WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &      'profilesDoGenGrid=.true. is required'
       CALL PRINT_ERROR( msgBuf , myThid )
       WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &      'unless usingSphericalGrid=.TRUE. and rotateGrid=.FALSE.'
       CALL PRINT_ERROR( msgBuf , myThid )
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') 'general packages parameters :'

      JL = ILNBLNK( profilesDir )
      IF (JL.NE.0) THEN
       WRITE(msgBuf,'(2A)') '  profilesDir ', profilesDir(1:JL)
      ELSE
       WRITE(msgBuf,'(2A)') '  profilesDir ','./'
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,L5)') '  profilesDoGenGrid  ', profilesDoGenGrid
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,L5)') '  profilesDoNcOutput ', profilesDoNcOutput
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

      _BEGIN_MASTER( myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     0) initialise profiles arrays

      DO bj = 1, nSy
       DO bi = 1, nSx

        profiles_curfile_buff(bi,bj) = 0
        DO kLev = 1, NLEVELMAX
         DO ic = 1, 1000
          DO num_var = 1, NVARMAX
           profiles_data_buff  (kLev,ic,num_var,bi,bj) = 0. _d 0
           profiles_weight_buff(kLev,ic,num_var,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
        DO kCMax = 1, NLEVELCOMBMAX
         prof_depth_comb(kCMax,bi,bj) = -999. _d 0
        ENDDO
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

        DO num_file = 1, NFILESPROFMAX
          ProfNo(num_file,bi,bj) = 0
          prof_num_var_tot(num_file,bi,bj) = 0
          DO num_var = 1, NVARMAX
           prof_num_var_cur(num_file,num_var,bi,bj) = 0
           vec_quantities(num_file,num_var,bi,bj) = .FALSE.
          ENDDO
          DO prof_num = 1, NOBSGLOB
           prof_time      (num_file,prof_num,bi,bj) = -999. _d 0
           prof_lon       (num_file,prof_num,bi,bj) = -999. _d 0
           prof_lat       (num_file,prof_num,bi,bj) = -999. _d 0
           prof_ind_glob  (num_file,prof_num,bi,bj) = 0
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
           prof_ind_avgbin(num_file,prof_num,bi,bj) = -999
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */
           prof_interp_xC11  (num_file,prof_num,bi,bj) = -999. _d 0
           prof_interp_yC11  (num_file,prof_num,bi,bj) = -999. _d 0
           prof_interp_xCNINJ(num_file,prof_num,bi,bj) = -999. _d 0
           prof_interp_yCNINJ(num_file,prof_num,bi,bj) = -999. _d 0
           DO iq = 1, NUM_INTERP_POINTS
            prof_interp_i(num_file,prof_num,iq,bi,bj) = 1
            prof_interp_j(num_file,prof_num,iq,bi,bj) = 1
            prof_interp_weights(num_file,prof_num,iq,bi,bj) = 0. _d 0
           ENDDO
          ENDDO !DO prof_num
        ENDDO !DO num_file

C--   end bi,bj loops
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      yymmddMin = modelstartdate(1)
      yymmddMax = modelenddate(1)
      hhmmssMin = modelstartdate(2)
      hhmmssMax = modelenddate(2)

      DO num_file = 1, NFILESPROFMAX
       profilesfile = profilesfiles(num_file)
       IL = ILNBLNK( profilesfile )

       DO bj = 1, nSy
        DO bi = 1, nSx
         stopProfiles = .FALSE.
         stopGenericGrid = 0
         ProfNo_hh=0

         IF (IL.NE.0) THEN
          WRITE(msgBuf,'(A)') ' '
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A,I3,2A)') 'profiles file #', num_file,
     &         ' is ', profilesfile(1:IL)
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

C===========================================================
C     Open data files and read information
C===========================================================
C-- Note: the NetCdF input file is not specific to this tile,
C     ==> could save I/O if we open this file outside bi,bj loops
C         and drop nSx,nSy dims from "fiddata" vector.
C---------
          WRITE(fnamedatanc,'(2A)') profilesfile(1:IL),'.nc'
          err = NF_OPEN( fnamedatanc, NF_NOWRITE,
     &         fiddata(num_file,bi,bj) )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_OPEN fiddata',err,bi,bj,myThid )

C     1) Read the number of profiles and available dimensions:
          fid = fiddata(num_file,bi,bj)

          err = NF_INQ_DIMID( fid, 'iPROF', dimId )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_DIMID iPROF',err,bi,bj,myThid )
          err = NF_INQ_DIMLEN( fid,
     &         dimId, ProfNo(num_file,bi,bj) )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_DIMLEN ProfNo',
     &         err,bi,bj,myThid )

          err = NF_INQ_DIMID( fid, 'iDEPTH', dimId )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_DIMID iDEPTH',
     &         err,bi,bj,myThid )
          IF (err.NE.NF_NOERR) THEN
           err = NF_INQ_DIMID( fid, 'Z', dimId )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_DIMID Z',err,bi,bj,myThid )
          ENDIF

          err = NF_INQ_DIMLEN( fid, dimId,
     &         ProfDepthNo(num_file,bi,bj) )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_DIMLEN ProfDepthNo',
     &         err,bi,bj,myThid )
C      Check that maximum number of vertical levels is not exceeded
          IF ( ProfDepthNo(num_file,bi,bj) .GT. NLEVELMAX ) THEN
           WRITE(msgBuf,'(4A,I8)')
     &      'PROFILES_INIT_FIXED: in: "',profilesfile(1:IL),'.nc",',
     &      ' number of depth levels exceeds NLEVELMAX=', NLEVELMAX
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &      'increase NLEVELMAX in PROFILES_SIZE.h and recompile'
           CALL PRINT_ERROR( msgBuf, myThid )
           stopProfiles = .TRUE.
          ENDIF

          err = NF_INQ_DIMID( fid, 'iINTERP', dimId )
          IF (err.EQ.NF_NOERR) THEN
           err = NF_INQ_DIMLEN( fid, dimId, iINTERP )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_DIMLEN iINTERP',
     &          err,bi,bj,myThid )
          ELSE
           IF (debugLevel .GE. debLevA) THEN
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_INQ_DIMID iINTERP',
     &           err,bi,bj,myThid )
            WRITE(msgBuf,'(3A,I3)')
     &           'S/R PROFILES_INIT_FIXED: ',
     &           'no iINTERP dim in data file using iINTERP ',
     &           '= NUM_INTERP_POINTS =', NUM_INTERP_POINTS
            CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
           ENDIF
           iINTERP = NUM_INTERP_POINTS
          ENDIF

          WRITE(msgBuf,'(2(A,I4))')
     &         '  current tile is bi,bj                      =',
     &         bi,',',bj
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A,I9)')
     &         '  # of depth levels in file                  =',
     &         ProfDepthNo(num_file,bi,bj)
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A,I9)')
     &         '  # of profiles in file                      =',
     &         ProfNo(num_file,bi,bj)
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

C     2) Read dates and positions:
          err = NF_INQ_VARID( fid,'prof_depth', varId1a )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_VARID prof_depth',
     &         err,bi,bj,myThid )
          IF (err.NE.NF_NOERR) THEN
C     LEGACY: Try old variable name: depth
           err = NF_INQ_VARID( fid,'depth', varId1a )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID depth',
     &          err,bi,bj,myThid )
          ENDIF

          IF (err.NE.NF_NOERR) THEN
C     If neither is found, then stop
           IL = ILNBLNK( profilesfile )
           WRITE(msgBuf,'(4A)')
     &          'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &          '.nc is not in the pkg/profiles format',
     &          ' (no prof_depth etc.)'
           CALL PRINT_ERROR( msgBuf, myThid )
           stopProfiles = .TRUE.
          ENDIF

          IF ( .NOT.stopProfiles ) THEN
           DO kLev = 1, ProfDepthNo(num_file,bi,bj)
            err = NF_GET_VAR1_DOUBLE( fid, varId1a, kLev,
     &           prof_depth(num_file,kLev,bi,bj) )
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_VAR1_DOUBLE prof_depth',
     &           err,bi,bj,myThid )
           ENDDO
          ENDIF

C     Get time, lon, lat varIds
          err = NF_INQ_VARID( fid,'prof_YYYYMMDD', varId1a )
          nerr = ABS( err - NF_NOERR )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_VARID prof_YYYYMMDD',
     &         err,bi,bj,myThid )
          err = NF_INQ_VARID( fid,'prof_HHMMSS', varId1b )
          nerr = nerr + ABS( err - NF_NOERR )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_VARID prof_HHMMSS',
     &         err,bi,bj,myThid )
          err = NF_INQ_VARID( fid,'prof_lon', varId2 )
          nerr = nerr + ABS( err - NF_NOERR )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_VARID prof_lon',
     &         err,bi,bj,myThid )
          err = NF_INQ_VARID( fid,'prof_lat', varId3 )
          nerr = nerr + ABS( err - NF_NOERR )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_VARID prof_lat',
     &         err,bi,bj,myThid )
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
          err = NF_INQ_VARID( fid,'prof_bin_id_a', varId4 )
          nerr = nerr + ABS( err - NF_NOERR )
          CALL PROFILES_NF_ERROR(
     &         'INIT_FIXED: NF_INQ_VARID prof_bin_id_a',
     &         err,bi,bj,myThid )
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

          IF ( nerr.NE.0 ) THEN
           IL = ILNBLNK( profilesfile )
           WRITE(msgBuf,'(4A)')
     &          'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &          '.nc not in pkg/profiles format',
     &          ' (no prof_YYYYMMDD etc.)'
           CALL PRINT_ERROR( msgBuf, myThid )
           stopProfiles = .TRUE.
          ENDIF

          IF (profilesDoGenGrid) THEN
C     3) Read interpolation information (grid points, coeffs, etc.)
           err = NF_INQ_VARID( fid,'prof_interp_XC11', varId_intp1 )
           nerr = ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_XC11',
     &          err,bi,bj,myThid )
           err = NF_INQ_VARID( fid,'prof_interp_YC11', varId_intp2 )
           nerr = nerr + ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_YC11',
     &          err,bi,bj,myThid )
           err = NF_INQ_VARID( fid,'prof_interp_XCNINJ', varId_intp11 )
           nerr = nerr + ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_XCNINJ',
     &          err,bi,bj,myThid )
           err = NF_INQ_VARID( fid,'prof_interp_YCNINJ', varId_intp22 )
           nerr = nerr + ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_YCNINJ',
     &          err,bi,bj,myThid )
           err = NF_INQ_VARID( fid,'prof_interp_weights', varId_intp3 )
           nerr = nerr + ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_weights',
     &          err,bi,bj,myThid )
           err = NF_INQ_VARID( fid,'prof_interp_i', varId_intp4 )
           nerr = nerr + ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_i',
     &          err,bi,bj,myThid )
           err = NF_INQ_VARID( fid,'prof_interp_j', varId_intp5 )
           nerr = nerr + ABS( err - NF_NOERR )
           CALL PROFILES_NF_ERROR(
     &          'INIT_FIXED: NF_INQ_VARID prof_interp_j',
     &          err,bi,bj,myThid )

           IF ( nerr.NE.0 ) THEN
            IL = ILNBLNK( profilesfile )
            WRITE(msgBuf,'(4A)')
     &           'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &           '.nc missing interpolation information',
     &           ' (profilesDoGenGrid)'
            CALL PRINT_ERROR( msgBuf, myThid )
            stopGenericGrid = 2
           ENDIF
          ENDIF !IF (profilesDoGenGrid)

C     4) Default values: now part of initialisation
C     5) Main loop: look for profiles in this bi,bj tile
          ProfNo_tile = 0
          profNo_div1000 = MAX(0,INT(ProfNo(num_file,bi,bj)/1000))

          DO chunkProf = 1, profno_div1000+1
C     5.1) Read a chunk
           chunk = 1000*(chunkProf-1)

           IF (MIN(ProfNo(num_file,bi,bj), 1000*chunkProf).GE.
     &          1+chunk) THEN
            vec_start(1) = 1
            vec_start(2) = 1+chunk
            vec_count(1) = 1
            vec_count(2) = MIN(1000, ProfNo(num_file,bi,bj)-chunk)

            IF ( (vec_count(2).LE.0) .OR.
     &           (vec_count(2).GT.1000) .OR.
     &           (vec_start(2).LE.0) .OR.
     &           (vec_count(2)+vec_start(2)-1.GT.
     &           ProfNo(num_file,bi,bj)) ) THEN
             IL  = ILNBLNK( profilesfile )
             WRITE(msgBuf,'(3A)')
     &            'PROFILES_INIT_FIXED: file ',profilesfile(1:IL),
     &            '.nc was not read properly (case 1).'
             CALL PRINT_ERROR( msgBuf, myThid )
             stopProfiles = .TRUE.
            ENDIF

            err = NF_GET_VARA_DOUBLE( fid, varId1a,
     &           vec_start(2),vec_count(2), tmpyymmdd )
            nerr = ABS( err - NF_NOERR )
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_GET_VARA_DOUBLE tmpyymmdd',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fid, varId1b,
     &           vec_start(2), vec_count(2), tmphhmmss )
            nerr = nerr + ABS( err - NF_NOERR )
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_GET_VARA_DOUBLE tmphhmmss',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fid, varId2,
     &           vec_start(2),vec_count(2), tmp_lon2 )
            nerr = nerr + ABS( err - NF_NOERR )
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_lon2',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fid, varId3,
     &           vec_start(2),vec_count(2), tmp_lat2 )
            nerr = nerr + ABS( err - NF_NOERR )
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_lat2',
     &           err,bi,bj,myThid )
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
            err = NF_GET_VARA_DOUBLE( fid, varId4,
     &           vec_start(2),vec_count(2), tmp_avgbin )
            nerr = nerr + ABS( err - NF_NOERR )
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_avgbin',
     &           err,bi,bj,myThid )
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

            IF ( nerr.NE.0 ) THEN
             WRITE(msgBuf,'(3A)')
     &            'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &            '.nc get_vara_double (case 2).'
             CALL PRINT_ERROR( msgBuf, myThid )
             stopProfiles = .TRUE.
            ENDIF

C     If profilesDoGenGrid, then read interpolation coeffs and indices
            IF ( profilesDoGenGrid .AND. stopGenericGrid.EQ.0 ) THEN
             err = NF_GET_VARA_DOUBLE( fid, varId_intp1,
     &            vec_start(2),vec_count(2), tmp_xC11 )
             CALL PROFILES_NF_ERROR(
     &            'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_xC11',
     &            err,bi,bj,myThid )
             err = NF_GET_VARA_DOUBLE( fid, varId_intp2,
     &            vec_start(2),vec_count(2), tmp_yC11 )
             CALL PROFILES_NF_ERROR(
     &            'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_yC11',
     &            err,bi,bj,myThid )
             err = NF_GET_VARA_DOUBLE( fid, varId_intp11,
     &            vec_start(2),vec_count(2), tmp_xCNINJ )
             CALL PROFILES_NF_ERROR(
     &            'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_xCNINJ',
     &            err,bi,bj,myThid )
             err = NF_GET_VARA_DOUBLE( fid, varId_intp22,
     &            vec_start(2),vec_count(2), tmp_yCNINJ )
             CALL PROFILES_NF_ERROR(
     &            'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_yCNINJ',
     &            err,bi,bj,myThid )

             DO iq = 1, iINTERP
              vec_start2(1) = iq
              vec_start2(2) = 1+chunk
              vec_count2(1) = 1
              vec_count2(2) = MIN(1000,ProfNo(num_file,bi,bj)-chunk)

              err = NF_GET_VARA_DOUBLE( fid, varId_intp3,
     &             vec_start2,vec_count2, tmp_weights(1,iq) )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_weights',
     &             err,bi,bj,myThid )
              err = NF_GET_VARA_DOUBLE( fid, varId_intp4,
     &             vec_start2,vec_count2, tmp_i(1,iq) )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_i',
     &             err,bi,bj,myThid )
              err = NF_GET_VARA_DOUBLE(fid, varId_intp5,
     &             vec_start2,vec_count2, tmp_j(1,iq) )
              CALL PROFILES_NF_ERROR(
     &             'INIT_FIXED: NF_GET_VARA_DOUBLE tmp_j',
     &             err,bi,bj,myThid )
             ENDDO
            ENDIF !IF (profilesDoGenGrid)

C     5.2) Loop through this chunk
            DO ic = 1, MIN(1000,ProfNo(num_file,bi,bj)-chunk)
             IF ( .NOT.stopProfiles ) THEN

              profIsInRunTime = 1

              IF (( (tmpyymmdd(ic).GT.yymmddMin).OR.
     &             ((tmpyymmdd(ic).EQ.yymmddMin) .AND.
     &              (tmphhmmss(ic).GT.hhmmssMin)) ) .AND.
     &            ( (tmpyymmdd(ic).LT.yymmddMax).OR.
     &             ((tmpyymmdd(ic).EQ.yymmddMax) .AND.
     &              (tmphhmmss(ic).LT.hhmmssMax)) )) THEN
               hh = INT(tmphhmmss(ic))/10000
               IF (hh.LT.hoursPerDay) THEN
                profIsInRunTime = 1

                CALL CAL_FULLDATE( INT(tmpyymmdd(ic)),
     &               INT(tmphhmmss(ic)),tmpdate,myThid )
                CALL CAL_TIMEPASSED( modelstartdate,tmpdate,
     &               tmpdiff,myThid )
                CALL CAL_TOSECONDS(
     &               tmpdiff,diffsecs,myThid )

                diffsecs = diffsecs+nIter0*deltaTClock

               ELSE
C     If tmp hhmmss is out of range then disregard profile
                profIsInRunTime = 0
                diffsecs  = -deltaTClock
                ProfNo_hh = ProfNo_hh+1
               ENDIF

              ELSE

               profIsInRunTime = 0
               diffsecs = -deltaTClock

              ENDIF !IF (( (tmpyymmdd(ic)

C     5.2a) Determine profiles in current tile domain (lat-lon grid case)
              IF ( (.NOT.profilesDoGenGrid) .AND.
     &             (profIsInRunTime.EQ.1) ) THEN
               IF (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) THEN
                tmp_lon = xC(sNx+1,1,bi,bj)+360. _d 0
               ELSE
                tmp_lon = xC(sNx+1,1,bi,bj)
               ENDIF

               IF ( (xC(1,1,bi,bj).LE.tmp_lon2(ic)) .AND.
     &              (tmp_lon.GT.tmp_lon2(ic)) .AND.
     &              (yC(1,1,bi,bj).LE.tmp_lat2(ic)) .AND.
     &              (yC(1,sNy+1,bi,bj).GT.tmp_lat2(ic)) ) THEN
                lon_cur = tmp_lon2(ic)
                lat_cur = tmp_lat2(ic)
               ELSEIF ( (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj))
     &               .AND. (xC(1,1,bi,bj).LE. tmp_lon2(ic)+360. _d 0)
     &               .AND. (tmp_lon.GT.tmp_lon2(ic)+360. _d 0)
     &               .AND. (yC(1,1,bi,bj).LE.tmp_lat2(ic))
     &               .AND. (yC(1,sNy+1,bi,bj).GT.tmp_lat2(ic)) ) THEN
                lon_cur = tmp_lon2(ic)+360. _d 0
                lat_cur = tmp_lat2(ic)
               ELSE
                profIsInRunTime = 0
               ENDIF

C     Determine value of i,j to the South-West of data point
               prof_i = -10
               prof_j = -10
               lon_1  = -10
               lon_2  = -10
               lat_1  = -10
               lat_2  = -10

               IF (profIsInRunTime.EQ.1) THEN
                DO j = 1, sNy+1
                 DO i = 1, sNx+1
C     Value of j, south of the data point:
                  IF ( (yC(i,j,bi,bj).LE.lat_cur) .AND.
     &                 (yC(i,j+1,bi,bj).GT.lat_cur) ) THEN
                   prof_j = j
                   lat_1  = yC(i,j,bi,bj)
                   lat_2  = yC(i,j+1,bi,bj)
                  ENDIF

C     Value of i, west of the data point:
                  IF (xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
                   lon_tmp2 = xC(i+1,j,bi,bj)+360
                  ELSE
                   lon_tmp2 = xC(i+1,j,bi,bj)
                  ENDIF
                  IF (xC(i,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
                   lon_tmp1 = xC(i,j,bi,bj)+360
                  ELSE
                   lon_tmp1 = xC(i,j,bi,bj)
                  ENDIF

                  IF ( lon_tmp1.LE.lon_cur .AND.
     &                 lon_tmp2.GT.lon_cur ) THEN
                   prof_i = i
                   lon_1  = lon_tmp1
                   lon_2  = lon_tmp2
                  ENDIF

                 ENDDO !DO i
                ENDDO !DO j
               ENDIF !IF (profIsRunTime.EQ.1)

               IF ((prof_i.EQ.-10).OR.(prof_j.EQ.-10)) THEN
                profIsInRunTime = 0
               ENDIF

               IF (profIsInRunTime.EQ.1) THEN
C     If yes then store prof_time and longitude and latitude:
                ProfNo_tile = ProfNo_tile+1
                prof_time(num_file,ProfNo_tile,bi,bj)     =  diffsecs
                prof_lon(num_file,ProfNo_tile,bi,bj)      = lon_cur
                prof_lat(num_file,ProfNo_tile,bi,bj)      = lat_cur
                prof_ind_glob(num_file,ProfNo_tile,bi,bj) = ic+chunk
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                prof_ind_avgbin(num_file,ProfNo_tile,bi,bj)
     &               = tmp_avgbin(ic)
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */
C     Then store interpolation coeffs and indices
                lon_fac = (lon_cur-lon_1)/(lon_2-lon_1)
                lat_fac = (lat_cur-lat_1)/(lat_2-lat_1)

                prof_interp_weights(num_file,ProfNo_tile,1,bi,bj)
     &               = (1-lon_fac)*(1-lat_fac)
                prof_interp_i(num_file,ProfNo_tile,1,bi,bj) = prof_i
                prof_interp_j(num_file,ProfNo_tile,1,bi,bj) = prof_j

                prof_interp_weights(num_file,ProfNo_tile,2,bi,bj)
     &               = lon_fac*(1-lat_fac)
                prof_interp_i(num_file,ProfNo_tile,2,bi,bj) = prof_i+1
                prof_interp_j(num_file,ProfNo_tile,2,bi,bj) = prof_j

                prof_interp_weights(num_file,ProfNo_tile,3,bi,bj)
     &               = (1-lon_fac)*lat_fac
                prof_interp_i(num_file,ProfNo_tile,3,bi,bj) = prof_i
                prof_interp_j(num_file,ProfNo_tile,3,bi,bj) = prof_j+1

                prof_interp_weights(num_file,ProfNo_tile,4,bi,bj)
     &               = lon_fac*lat_fac
                prof_interp_i(num_file,ProfNo_tile,4,bi,bj) = prof_i+1
                prof_interp_j(num_file,ProfNo_tile,4,bi,bj) = prof_j+1
               ENDIF !IF (profIsRunTime.EQ.1)

C     5.2a) Determine profiles in current tile domain (gen grid case)
              ELSEIF (profIsInRunTime.EQ.1) THEN

               IF (stopGenericGrid.EQ.0) THEN
                IF (ABS(tmp_xC11(ic)-xC(1,1,bi,bj)).LT.0.0001 _d 0
     &         .AND.ABS(tmp_yC11(ic)-yC(1,1,bi,bj)).LT.0.0001 _d 0
     &         .AND.ABS(tmp_xCNINJ(ic)-xC(sNx,sNy,bi,bj)).LT.0.0001 _d 0
     &         .AND.ABS(tmp_yCNINJ(ic)-yC(sNx,sNy,bi,bj)).LT.0.0001 _d 0
     &         .AND.profIsInRunTime.EQ.1 ) THEN
C     If yes then store prof_time and interpolation coeffs and indices:
                 ProfNo_tile = ProfNo_tile+1
                 prof_time(num_file,ProfNo_tile,bi,bj) = diffsecs
# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                 prof_ind_avgbin(num_file,ProfNo_tile,bi,bj)
     &                = tmp_avgbin(ic)
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */
                 prof_interp_xC11(num_file,ProfNo_tile,bi,bj)
     &                = tmp_xC11(ic)
                 prof_interp_yC11(num_file,ProfNo_tile,bi,bj)
     &                = tmp_yC11(ic)
                 prof_interp_xCNINJ(num_file,ProfNo_tile,bi,bj)
     &                = tmp_xCNINJ(ic)
                 prof_interp_yCNINJ(num_file,ProfNo_tile,bi,bj)
     &                = tmp_yCNINJ(ic)
                 tmp_sum_weights = 0. _d 0

                 DO iq = 1, iINTERP

                  prof_interp_weights(num_file,ProfNo_tile,iq,bi,bj)
     &                 = tmp_weights(ic,iq)
                  prof_interp_i(num_file,ProfNo_tile,iq,bi,bj)
     &                 = INT(tmp_i(ic,iq))
                  prof_interp_j(num_file,ProfNo_tile,iq,bi,bj)
     &                 = INT(tmp_j(ic,iq))
                  tmp_sum_weights = tmp_sum_weights + tmp_weights(ic,iq)
C     More test of the inputs: is the offline-computed
C     Interpolation information consistent (self and with grid)
                  IF ( tmp_i(ic,iq).LT.0     .OR. tmp_j(ic,iq).LT.0 .OR.
     &                 tmp_i(ic,iq).GT.sNx+1 .OR. tmp_j(ic,iq).GT.sNy+1
     &                 ) THEN
                   WRITE(msgBuf,'(4A)')
     &                  'PROFILES_INIT_FIXED: file ',
     &                  profilesfile(1:IL),
     &                  '.nc has inconsistent interp ',
     &                  '(profilesDoGenGrid; out of tile)'
                   CALL PRINT_ERROR( msgBuf, myThid )
                   stopGenericGrid = 1
                  ENDIF

# ifdef ALLOW_PROFILES_EXCLUDE_CORNERS
                  IF (tmp_weights(ic,iq).NE.0. _d 0) THEN
C     We should not reach the corners just outside of the current tile.
c             IF ( (tmp_i(ic,iq).EQ.0.AND.tmp_j(ic,iq).EQ.0)
c    &        .OR. (tmp_i(ic,iq).EQ.sNx+1.AND.tmp_j(ic,iq).EQ.sNy+1)
c    &        .OR. (tmp_i(ic,iq).EQ.0.AND.tmp_j(ic,iq).EQ.sNy+1)
c    &        .OR. (tmp_i(ic,iq).EQ.sNx+1.AND.tmp_j(ic,iq).EQ.0) ) THEN
C     This should be the same, fewer evaluations, and easier to read?
                   IF((tmp_i(ic,iq).EQ.0.OR.tmp_i(ic,iq).EQ.sNx+1) .AND.
     &                (tmp_j(ic,iq).EQ.0.OR.tmp_j(ic,iq).EQ.sNy+1)
     &                ) THEN
                    WRITE(msgBuf,'(5A)')
     &                   'PROFILES_INIT_FIXED: file ',
     &                   profilesfile(1:IL),
     &                   '.nc has inconsistent interp',
     &                   '(profilesDoGenGrid; overlapping',
     &                   'corner)'
                    CALL PRINT_ERROR( msgBuf, myThid )
                    stopGenericGrid = 1
                   ENDIF
                  ENDIF
# endif /* ALLOW_PROFILES_EXCLUDE_CORNERS */

                  IF ( (tmp_weights(ic,iq).LT.0. _d 0) .OR.
     &                 (tmp_weights(ic,iq).GT.1. _d 0) ) THEN
                   WRITE(msgBuf,'(5A)')
     &                  'PROFILES_INIT_FIXED: file ',
     &                  profilesfile(1:IL),
     &                  '.nc has inconsistent interp',
     &                  'weights (profilesDoGenGrid; ',
     &                  'sum oustide 0-1)'
                   CALL PRINT_ERROR( msgBuf, myThid)
                   stopGenericGrid = 1
                  ENDIF

                 ENDDO !DO iq

                 IF (ABS(tmp_sum_weights -1. _d 0).GT. 0.0001 _d 0) THEN
                  WRITE(msgBuf,'(4A)')
     &                 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &                 '.nc has inconsistent interp weights',
     &                 ' (profilesDoGenGrid; dont add to 1)'
                  CALL PRINT_ERROR( msgBuf, myThid)
                  stopGenericGrid = 1
                 ENDIF

                 prof_ind_glob(num_file,ProfNo_tile,bi,bj) = ic + chunk

                ENDIF !IF ( ( ABS(tmp_xC11(ic)...
               ENDIF !IF (stopGenericGrid.EQ.0)
              ENDIF!IF (.NOT.profilesDoGenGrid ... ELSEIF ...

C     Check that maximum size was not reached:
              IF (ProfNo_tile.GE.NOBSGLOB) THEN
               WRITE(msgBuf,'(3A)')
     &              'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &              '.nc not read properly (increase NOBSGLOB).'
               CALL PRINT_ERROR( msgBuf, myThid)
               stopProfiles = .TRUE.
              ENDIF

             ENDIF !IF ( .not.stopProfiles )
            ENDDO !DO ic

           ENDIF !IF (MIN(ProfNo(num_file,bi,bj)
          ENDDO !DO chunkProf

          ProfNo(num_file,bi,bj) = ProfNo_tile

          WRITE(msgBuf,'(A,I9)')
     &         '  # of profiles with erroneous HHMMSS values =',
     &         ProfNo_hh
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

          WRITE(msgBuf,'(A,I9)')
     &         '  # of profiles within tile and time period  =',
     &         ProfNo(num_file,bi,bj)
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

C     6) Available variables in the data set
          DO num_var = 1, NVARMAX
           prof_num_var_cur(num_file,num_var,bi,bj) = 0
          ENDDO
          prof_num_var_tot(num_file,bi,bj) = 0

          DO num_var = 1, NVARMAX
           JL = ILNBLNK( prof_names(num_file,num_var) )
           err = NF_INQ_VARID( fid,
     &          prof_names(num_file,num_var)(1:JL), varId1 )

           IF (err.EQ.NF_NOERR) THEN
            vec_quantities(num_file,num_var,bi,bj) = .TRUE.

            prof_num_var_tot(num_file,bi,bj) =
     &           prof_num_var_tot(num_file,bi,bj) + 1
            prof_num_var_cur(num_file,num_var,bi,bj) =
     &           prof_num_var_tot(num_file,bi,bj)
           ELSE
            CALL PROFILES_NF_ERROR(
     &           'INIT_FIXED: NF_INQ_VARID prof_names = '//
     &           prof_names(num_file,num_var)(1:JL),err,bi,bj,myThid )
            IF (debugLevel .GE. debLevA) THEN
             WRITE(msgBuf,'(3A)') 'S/R PROFILES_INIT_FIXED: no ',
     &            prof_names(num_file,num_var)(1:JL),
     &            ', setting corresponding vec_quantities = F'
             CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
            ENDIF
            vec_quantities(num_file,num_var,bi,bj) = .FALSE.
           ENDIF

           IF (vec_quantities(num_file,num_var,bi,bj)) THEN
            KL = ILNBLNK( prof_names(num_file,num_var) )
            JL = ILNBLNK( prof_namesmod(num_file,num_var) )

            IF (prof_namesmod(num_file,num_var).EQ.'pTracer') THEN
             WRITE(msgBuf,'(A,I3,5A,I3)') '  variable #',
     &            num_var,' is ' ,
     &            prof_names(num_file,num_var)(1:KL),' and ',
     &            prof_namesmod(num_file,num_var)(1:JL),' #',
     &            prof_itracer(num_file,num_var)
            ELSE
             WRITE(msgBuf,'(A,I3,4A)') '  variable #',
     &            num_var,' is            ' ,
     &            prof_names(num_file,num_var)(1:KL),' and ',
     &            prof_namesmod(num_file,num_var)(1:JL)
            ENDIF
            CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
           ENDIF !IF (vec_quantities
          ENDDO !DO num_var

C===========================================================
C     Create files for model counterparts to observations
C===========================================================

          IF (ProfNo(num_file,bi,bj).GT.0) THEN
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
           JL = ILNBLNK( profilesDir )

           IF (profilesDoNcOutput) THEN
            WRITE(fnameequinc,'(3A,2(I3.3,A))')
     &           profilesDir(1:JL),profilesfile(1:IL),
     &           '.',iG,'.',jG,'.equi.nc'
# ifdef ALLOW_ADJOINT_RUN
            WRITE(adfnameequinc,'(4A,2(I3.3,A))')
     &           profilesDir(1:JL),'ad',profilesfile(1:IL),
     &           '.',iG,'.',jG,'.equi.nc'
# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
            WRITE(tlfnameequinc,'(4A,2(I3.3,A))')
     &           profilesDir(1:JL),'tl',profilesfile(1:IL),
     &           '.',iG,'.',jG,'.equi.nc'
# endif /* ALLOW_TANGENTLINEAR_RUN */

            JL = ILNBLNK( fnameequinc )
            INQUIRE( FILE = fnameequinc(1:JL), EXIST = exst )
            IF (.NOT.exst) THEN
             CALL PROFILES_INIT_NCFILE( num_file,
     &            fiddata(num_file,bi,bj), fnameequinc(1:JL),
     &            fidforward(num_file,bi,bj),
     &            ProfNo(num_file,bi,bj),
     &            ProfDepthNo(num_file,bi,bj),
     &            bi, bj, myThid )
            ELSE
C     Obtain existing NetCDF file id
             err = NF_OPEN( fnameequinc(1:JL), NF_WRITE,
     &            fidforward(num_file,bi,bj) )
             CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_OPEN fidforward',
     &            err,bi,bj,myThid )
            ENDIF

# ifdef ALLOW_ADJOINT_RUN
            JL = ILNBLNK( adfnameequinc )
            INQUIRE( FILE = adfnameequinc(1:JL), EXIST = exst )
            IF (.NOT.exst) THEN
             CALL PROFILES_INIT_NCFILE( num_file,
     &            fiddata(num_file,bi,bj), adfnameequinc(1:JL),
     &            fidadjoint(num_file,bi,bj),
     &            ProfNo(num_file,bi,bj),
     &            ProfDepthNo(num_file,bi,bj),
     &            bi, bj, myThid )
            ELSE
C     Obtain existing NetCDF file id
             err = NF_OPEN( adfnameequinc(1:JL), NF_WRITE,
     &            fidadjoint(num_file,bi,bj) )
             CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_OPEN fidadjoint',
     &            err,bi,bj,myThid )
            ENDIF

# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
            JL = ILNBLNK( tlfnameequinc )
            INQUIRE( FILE = tlfnameequinc(1:JL), EXIST = exst )
            IF (.NOT.exst) THEN
             CALL PROFILES_INIT_NCFILE( num_file,
     &            fiddata(num_file,bi,bj), tlfnameequinc(1:JL),
     &            fidtangent(num_file,bi,bj),
     &            ProfNo(num_file,bi,bj),
     &            ProfDepthNo(num_file,bi,bj),
     &            bi, bj, myThid )
            ELSE
C     Obtain existing NetCDF file id
             err = NF_OPEN( tlfnameequinc(1:JL), NF_WRITE,
     &            fidtangent(num_file,bi,bj) )
             CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_OPEN fidtangent',
     &            err,bi,bj,myThid )
            ENDIF

# endif /* ALLOW_TANGENTLINEAR_RUN */
           ELSE !IF (profilesDoNcOutput
            WRITE(fnameequinc,'(3A,2(I3.3,A))')
     &           profilesDir(1:JL),profilesfile(1:IL),
     &           '.',iG,'.',jG,'.equi.data'

# ifdef ALLOW_ADJOINT_RUN
            WRITE(adfnameequinc,'(4A,2(I3.3,A))')
     &           profilesDir(1:JL),'ad',profilesfile(1:IL),
     &           '.',iG,'.',jG,'.equi.data'

# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
            WRITE(tlfnameequinc,'(4A,2(I3.3,A))')
     &           profilesDir(1:JL),'tl',profilesfile(1:IL),
     &           '.',iG,'.',jG,'.equi.data'

# endif /* ALLOW_TANGENTLINEAR_RUN */

            JL = ILNBLNK( fnameequinc )
            INQUIRE( FILE = fnameequinc(1:JL), EXIST = exst )
# ifdef PROFILES_USE_MDSFINDUNITS
            CALL MDSFINDUNIT( fidforward(num_file,bi,bj), myThid )
# else
            CALL PROFILES_FINDUNIT( fidforward(num_file,bi,bj), myThid )
# endif
            IF (.NOT.exst) THEN
             CALL PROFILES_INIT_NCFILE( num_file,
     &            fiddata(num_file,bi,bj), fnameequinc(1:JL),
     &            fidforward(num_file,bi,bj),
     &            ProfNo(num_file,bi,bj),
     &            ProfDepthNo(num_file,bi,bj),
     &            bi, bj, myThid )
            ELSE
             OPEN( fidforward(num_file,bi,bj),
     &            FILE = fnameequinc(1:JL), FORM = 'unformatted',
     &            STATUS = 'unknown', ACCESS = 'direct',
     &            RECL = (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
            ENDIF

# ifdef ALLOW_ADJOINT_RUN
            JL = ILNBLNK( adfnameequinc )
            INQUIRE( FILE = adfnameequinc(1:JL), EXIST = exst )
#  ifdef PROFILES_USE_MDSFINDUNITS
            CALL MDSFINDUNIT( fidadjoint(num_file,bi,bj), myThid )
#  else
            CALL PROFILES_FINDUNIT( fidadjoint(num_file,bi,bj), myThid )
#  endif
            IF (.NOT.exst) THEN
             CALL PROFILES_INIT_NCFILE( num_file,
     &            fiddata(num_file,bi,bj), adfnameequinc(1:JL),
     &            fidadjoint(num_file,bi,bj),
     &            ProfNo(num_file,bi,bj),
     &            ProfDepthNo(num_file,bi,bj),
     &            bi, bj, myThid )
            ELSE
             OPEN( fidadjoint(num_file,bi,bj),
     &            FILE = adfnameequinc(1:JL), FORM = 'unformatted',
     &            STATUS = 'unknown', ACCESS = 'direct',
     &            RECL = (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
            ENDIF
# endif /* ALLOW_ADJOINT_RUN */

# ifdef ALLOW_TANGENTLINEAR_RUN
            JL = ILNBLNK( tlfnameequinc )
            INQUIRE( FILE = tlfnameequinc(1:JL), EXIST = exst )
#  ifdef PROFILES_USE_MDSFINDUNITS
            CALL MDSFINDUNIT( fidtangent(num_file,bi,bj), myThid )
#  else
            CALL PROFILES_FINDUNIT( fidtangent(num_file,bi,bj), myThid )
#  endif
            IF (.NOT.exst) THEN
             CALL PROFILES_INIT_NCFILE( num_file,
     &            fiddata(num_file,bi,bj), tlfnameequinc(1:JL),
     &            fidtangent(num_file,bi,bj),
     &            ProfNo(num_file,bi,bj),
     &            ProfDepthNo(num_file,bi,bj),
     &            bi, bj, myThid )
            ELSE
             OPEN( fidtangent(num_file,bi,bj),
     &            FILE = tlfnameequinc(1:JL), FORM = 'unformatted',
     &            STATUS = 'unknown', ACCESS = 'direct',
     &            RECL = (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
            ENDIF

# endif /* ALLOW_TANGENTLINEAR_RUN */

           ENDIF !IF (profilesDoNcOutput

          ENDIF !IF (ProfNo(num_file,bi,bj).GT.0)

C     ===========================================================
         ENDIF !IF (IL.NE.0)

         stopProfiles = stopProfiles .OR. ( stopGenericGrid.GT.0 )
         IF ( stopProfiles ) THEN
          stopProfiles_cnt = stopProfiles_cnt+1
         ENDIF
         stopGenericGrid_max = MAX( stopGenericGrid_max,
     &                              stopGenericGrid )

C--   end bi,bj loops
        ENDDO
       ENDDO
      ENDDO !DO num_file

# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
C     Find unique depth levels from all profile datasets

C     initialize prof_depth_comb
       NLEVELCOMB = 0
      DO bj = 1, nSy
       DO bi = 1, nSx

        m = 1
        DO num_file = 1, NFILESPROFMAX
         DO kLev = 1, MIN( ProfDepthNo(num_file,bi,bj), NLEVELMAX )
          IF (m.EQ.1) THEN
           prof_depth_comb(m,bi,bj) = prof_depth(num_file,kLev,bi,bj)
           m = m + 1
C     Sort
          ELSE !IF (m.EQ.1)
           DO kCMax = 1, NLEVELCOMBMAX-1
            IF (prof_depth_comb(kCMax,bi,bj) .NE. -999. _d 0) THEN
             IF ( prof_depth(num_file,kLev,bi,bj).LT.
     &            prof_depth_comb(kCMax,bi,bj).AND. kCMax.EQ.1 ) THEN

              prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &             prof_depth_comb(kCMax,bi,bj)

              prof_depth_comb(kCMax,bi,bj) =
     &             prof_depth(num_file,kLev,bi,bj)

              DO kC = NLEVELCOMBMAX-1, kCMax+2, -1
               prof_depth_comb(kC,bi,bj) = prof_depth_comb(kC-1,bi,bj)
              ENDDO !DO kC

              prof_depth_comb(kCMax+1,bi,bj) =
     &             prof_depth_comb(NLEVELCOMBMAX,bi,bj)

             ELSEIF ( prof_depth(num_file,kLev,bi,bj).GT.
     &             prof_depth_comb(kCMax,bi,bj) .AND.
     &             prof_depth(num_file,kLev,bi,bj).LT.
     &             prof_depth_comb(kCMax+1,bi,bj) ) THEN

              prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &             prof_depth_comb(kCMax+1,bi,bj)

              prof_depth_comb(kCMax+1,bi,bj) =
     &            prof_depth(num_file, kLev,bi,bj)

              DO kC = NLEVELCOMBMAX-1, kCMax+3, -1
               prof_depth_comb(kC,bi,bj) = prof_depth_comb(kC-1,bi,bj)
              ENDDO !DO kC

              prof_depth_comb(kCMax+2,bi,bj)=
     &             prof_depth_comb(NLEVELCOMBMAX,bi,bj)

             ELSEIF ( prof_depth(num_file,kLev,bi,bj).GT.
     &             prof_depth_comb(kCMax,bi,bj) .AND.
     &             prof_depth_comb(kCMax+1,bi,bj).EQ. -999. _d 0 ) THEN

              prof_depth_comb(kCMax+1,bi,bj) =
     &             prof_depth(num_file, kLev,bi,bj)

             ENDIF !IF (prof_depth(num_file,kLev,bi,bj).LT.
            ENDIF !IF (prof_depth_comb(kCMax,bi,bj).NE.-999.
           ENDDO !DO kCMax

          ENDIF !IF (m.EQ.1)

          IF (m.GE.NLEVELCOMBMAX-2) THEN
           WRITE(msgBuf,'(A)') 'increase NLEVELCOMBMAX'
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF

         ENDDO ! DO kLev

        ENDDO ! DO num_file

        prof_depth_comb(NLEVELCOMBMAX,bi,bj) = -999. _d 0

C     Diagnostics output
        DO kCMax = 1, NLEVELCOMBMAX
         IF ( prof_depth_comb(kCMax,bi,bj) .GE. 0. _d 0
     &        .AND. NLEVELCOMB.LT.kCMax ) THEN
          NLEVELCOMB = kCMax

          IF (kCMax.GE.NLEVELCOMBMAX-2) THEN
           WRITE(msgBuf,'(A,2I6)')
     &          'increase NLEVELCOMBMAX: kCMax, NLEVELCOMBMA  ',
     &          kCMax, NLEVELCOMBMAX
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
         ENDIF !IF ( prof_depth_comb(m,bi,bj) .GE. 0. _d 0
        ENDDO !DO kCMax

        WRITE(msgBuf,'(A,I6,D20.5)') 'NLEVELCOMB = ', NLEVELCOMB
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
C--   end bi,bj loops
       ENDDO
      ENDDO
# endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

C===========================================================
C     Error analysis:
C===========================================================

C     1) Provide interpolation information
      IF ( stopGenericGrid_max.EQ.2) THEN
#ifdef ALLOW_DEBUG
       DO bj = 1, nSy
        DO bi = 1, nSx
C     XC grid
         fnameequinc = 'profilesXCwithOverlap'
         JL  = ILNBLNK( fnameequinc )
         WRITE(iUnit,'(4A,2I4)')
     &        'PROFILES_INIT_FIXED: creating grid from profiles; ',
     &         'file:', fnameequinc(1:JL), ' , bi,bj=', bi, bj
         CALL WRITE_FULLARRAY_RS( fnameequinc, xC(1-OLx,1-OLy,bi,bj),
     &                            1, bi, bj, 1, -1, myThid )
C     YC grid
         fnameequinc = 'profilesYCwithOverlap'
         JL  = ILNBLNK( fnameequinc )
         WRITE(iUnit,'(4A,2I4)')
     &        'PROFILES_INIT_FIXED: creating grid from profiles; ',
     &         'file:', fnameequinc(1:JL), ' , bi,bj=', bi, bj
         CALL WRITE_FULLARRAY_RS( fnameequinc, yC(1-OLx,1-OLy,bi,bj),
     &                             1, bi, bj, 1, -1, myThid )
        ENDDO
       ENDDO
#endif /* ALLOW_DEBUG */

       WRITE(msgBuf,'(2A)')
     &      'PROFILES_INIT_FIXED : when using profilesDoGenGrid ',
     &      'you have to provide interpolation coeffs etc. '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)')
     &      'and some of your nc files dont have them. ',
     &      'You could use profiles_prep_mygrid.m and/or'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'use the grid info in ',
     &                      'profilesXC,YCwithOverlap.xxx.yyy.data'
       CALL PRINT_ERROR( msgBuf, myThid )

      ENDIF !IF (stopGenericGrid_max.EQ.2)

C===========================================================
      _END_MASTER( myThid )
      _BARRIER

# ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      NLEVELCOMBRL = NLEVELCOMB
      _GLOBAL_MAX_RL( NLEVELCOMBRL, myThid )
      NLEVELCOMB = NLEVELCOMBRL
# endif

C     2) Stop after other kind of errors
      CALL GLOBAL_SUM_INT( stopProfiles_cnt, myThid )
      IF ( stopProfiles_cnt.GE.1) THEN
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     '// insitu profiles model sampling >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_PROFILES */

      RETURN
      END
