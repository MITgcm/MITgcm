#include "PROFILES_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_MAKE_NCFILE

C !INTERFACE:
      SUBROUTINE PROFILES_MAKE_NCFILE( myThid )

C     !DESCRIPTION:
C     Combine tiled files into one global netcdf file of
C     model-equivalent profiles

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "PROFILES_SIZE.h"
#include "profiles.h"
#include "netcdf.inc"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER kLev, kProf, k
      INTEGER num_file,num_var,prof_num
      INTEGER bi,bj,iG,jG
      INTEGER err,dimId,irec,fid1,fid2
      INTEGER dimId1,dimId2,vecid(2)
      INTEGER varId0,varId1(NVARMAX*2),varId2(NVARMAX*2)
      INTEGER length,Zlength,optimcycle
      INTEGER IL,JL
      INTEGER vec_start(2),vec_count(2)
      _RL tmpgs
      _RL prof_mask1D(NLEVELMAX)
      _RL prof_traj1D(NLEVELMAX)
      _RL prof_buff(NVARMAX,NLEVELMAX,NOBSGLOB)
      _RL prof_mask_buff(NVARMAX,NLEVELMAX,NOBSGLOB)
      _RL prof_modval_glo(NVARMAX,NLEVELMAX,NOBSGLOB)
      _RL prof_mask_glo(NVARMAX,NLEVELMAX,NOBSGLOB)
      CHARACTER*(MAX_LEN_FNAM) prof_namesequi(NFILESPROFMAX,NVARMAX)
      CHARACTER*(MAX_LEN_FNAM) profFile
      CHARACTER*(MAX_LEN_FNAM) fnameequinc
      CHARACTER*(MAX_LEN_FNAM) fnamedata
      LOGICAL exst

      _BEGIN_MASTER( myThid )

C Set new netcdf variables names
      DO num_file = 1, NFILESPROFMAX
        DO num_var = 1, NVARMAX
          IL = ILNBLNK( prof_names(num_file,num_var) )
          WRITE(prof_namesequi(num_file,num_var),'(2A)')
     &     prof_names(num_file,num_var)(1:IL),'model'
        ENDDO
      ENDDO

C Initialize buffers
      DO num_var = 1, NVARMAX
        DO prof_num = 1, NOBSGLOB
          DO kLev = 1, NLEVELMAX
            prof_buff(num_var,kLev,prof_num) = 0.
            prof_mask_buff(num_var,kLev,prof_num) = 0.
          ENDDO
        ENDDO
      ENDDO

C Loop over files
      DO num_file = 1, NFILESPROFMAX
C File maintenance
        DO bj = 1, nSy
          DO bi = 1, nSx
            IF ( (ProfNo(num_file,bi,bj).GT.0).AND.
     &           (profilesDoNcOutput) ) THEN
C Need to close file so that data is not lost when run finishes
              err = NF_SYNC( fidforward(num_file,bi,bj) )
              CALL PROFILES_NF_ERROR(
     &             'MAKE_NCFILE: NF_SYNC fidforward',
     &             err,bi,bj,myThid )
            ENDIF

C Loop over variables
            DO num_var = 1, NVARMAX
              IF (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.)
     &        THEN
C Loop over profiles
                DO prof_num = 1, NOBSGLOB
                  IF (prof_num.LE.ProfNo(num_file,bi,bj)) THEN
C Initialize
                    DO kLev = 1, NLEVELMAX
                      prof_traj1D(kLev) = 0.
                      prof_mask1D(kLev) = 0.
                      prof_buff(num_var,kLev,prof_num) = 0.
                      prof_mask_buff(num_var,kLev,prof_num) = 0.
                    ENDDO

C Read tiled files
                    CALL ACTIVE_READ_PROFILE( num_file,
     &                   ProfDepthNo(num_file,bi,bj),prof_traj1D,
     &                   num_var,prof_num,.false.,optimcycle,
     &                   bi,bj,myThid,
     &                   profiles_dummy(num_file,num_var,bi,bj) )

C Save model equi and masks in buffer
C Combine all threads
                    irec = prof_ind_glob(num_file,prof_num,bi,bj)

                    DO kProf = 1, ProfDepthNo(num_file,bi,bj)
                      prof_buff(num_var,kProf,irec) =
     &                 prof_buff(num_var,kProf,irec)
     &                 + prof_traj1D(kProf)

                      prof_mask_buff(num_var,kProf,irec) =
     &                 prof_mask_buff(num_var,kProf,irec)
     &                 + prof_mask1D_cur(kProf,bi,bj)

                    ENDDO

                  ENDIF !IF (prof_num.LE.ProfNo(num_file,bi,bj))
                ENDDO !DO prof_num
              ENDIF !IF (vecquantities
            ENDDO !DO num_var

          ENDDO !DO bi
        ENDDO !DO bj

C Combine all processes
        DO num_var = 1, NVARMAX
          DO prof_num = 1, NOBSGLOB
            DO kLev = 1, NLEVELMAX
              tmpgs = prof_buff(num_var,kLev,prof_num)
              _GLOBAL_SUM_RL(tmpgs, myThid)

              prof_modval_glo(num_var,kLev,prof_num) = tmpgs
              tmpgs = prof_mask_buff(num_var,kLev,prof_num)
              _GLOBAL_SUM_RL(tmpgs, myThid)

              prof_mask_glo(num_var,kLev,prof_num) = tmpgs

            ENDDO !DO kLev
          ENDDO !DO prof_num
        ENDDO !DO num_var

        IF (myProcId.EQ.0) THEN
C Get dimensions of input file
          profFile=' '
          IL = ILNBLNK( profilesfiles(num_file) )
          IF (IL.NE.0) THEN
            WRITE(profFile,'(A)') profilesfiles(num_file)(1:IL)
          ENDIF

          IL = ILNBLNK( profFile )
          IF (IL.NE.0) THEN
            WRITE(fnamedata,'(2A)') profFile(1:IL),'.nc'
            err = NF_OPEN( fnamedata, NF_NOWRITE, fid1 )
            CALL PROFILES_NF_ERROR(
     &           'MAKE_NCFILE: NF_OPEN('//fnamedata(1:IL+3)//')',
     &           err,bi,bj,myThid )
            err = NF_INQ_DIMID( fid1,'iPROF', dimId )
            CALL PROFILES_NF_ERROR(
     &           'MAKE_NCFILE: NF_INQ_DIMID iPROF',
     &           err,bi,bj,myThid )
            err = NF_INQ_DIMLEN( fid1, dimId, length )
            CALL PROFILES_NF_ERROR(
     &           'MAKE_NCFILE: NF_INQ_DIMLEN length',
     &           err,bi,bj,myThid )
            err = NF_INQ_DIMID( fid1,'iDEPTH', dimId )
            CALL PROFILES_NF_ERROR(
     &           'MAKE_NCFILE: NF_INQ_DIMID iDEPTH',
     &           err,bi,bj,myThid )
            IF (err.NE.NF_NOERR) THEN
              err = NF_INQ_DIMID( fid1,'Z', dimId )
              CALL PROFILES_NF_ERROR(
     &             'MAKE_NCFILE: NF_INQ_DIMID Z',
     &             err,bi,bj,myThid )
            ENDIF
            err = NF_INQ_DIMLEN( fid1, dimId, Zlength )
            CALL PROFILES_NF_ERROR(
     &           'MAKE_NCFILE: NF_INQ_DIMLEN Zlength',
     &           err,bi,bj,myThid )

C Create new netcdf global file for model-equivalent
            JL = ILNBLNK( profilesDir )
            WRITE(fnameequinc,'(3A)')
     &       profilesDir(1:JL),profFile(1:IL),'.equi.nc'

            JL = ILNBLNK( fnameequinc )
            INQUIRE( FILE = fnameequinc(1:JL), EXIST = exst )
            IF (.NOT.exst) THEN
              err = NF_CREATE(fnameequinc(1:JL),NF_CLOBBER,fid2)
              CALL PROFILES_NF_ERROR(
     &             'MAKE_NCFILE: NF_CREATE('//fnameequinc(1:JL)//')',
     &             err,bi,bj,myThid )
              err = NF_DEF_DIM(fid2,'iDEPTH',Zlength,dimId1)
              CALL PROFILES_NF_ERROR(
     &             'MAKE_NCFILE: NF_DEF_DIM iDepth',
     &             err,bi,bj,myThid )
              err = NF_DEF_DIM(fid2,'iPROF',length,dimId2)
              CALL PROFILES_NF_ERROR(
     &             'MAKE_NCFILE: NF_DEF_DIM iPROF',
     &             err,bi,bj,myThid )

              vecid(1)=dimId1
              vecid(2)=dimId2

C Define variables and attributes
              DO num_var = 1, NVARMAX
                err = NF_INQ_VARID( fid1,
     &                prof_names(num_file,num_var), varId0 )
                CALL PROFILES_NF_ERROR( 'MAKE_NCFILE: NF_INQ_VARID('
     &               //prof_names(num_file,num_var)//')',
     &               err,bi,bj,myThid )

                IF (err.EQ.NF_NOERR) THEN
                  err = NF_DEF_VAR( fid2,
     &                  prof_namesequi(num_file,num_var),
     &                  NF_DOUBLE, 2, vecid, varId1(2+(num_var-1)*2) )
                  CALL PROFILES_NF_ERROR(
     &                 'MAKE_NCFILE: NF_DEF_VAR prof_namesequi',
     &                 err,bi,bj,myThid )
                  err = NF_PUT_ATT_DOUBLE( fid2,
     &                  varId1(2+(num_var-1)*2),
     &                  '_FillValue', NF_DOUBLE, 1, 0. _d 0 )
                  CALL PROFILES_NF_ERROR(
     &                 'MAKE_NCFILE: NF_PUT_ATT_DOUBLE varId(2',
     &                 err,bi,bj,myThid )

                  err = NF_DEF_VAR( fid2,
     &                  prof_namesmask(num_file,num_var),
     &                  NF_DOUBLE, 2, vecid, varId1(3+(num_var-1)*2) )
                  CALL PROFILES_NF_ERROR(
     &                 'MAKE_NCFILE: NF_DEF_VAR prof_namesmask',
     &                 err,bi,bj,myThid )
                  err = NF_PUT_ATT_DOUBLE( fid2,
     &                  varId1(3+(num_var-1)*2),
     &                  '_FillValue', NF_DOUBLE, 1, 0. _d 0 )
                  CALL PROFILES_NF_ERROR(
     &                 'MAKE_NCFILE: NF_PUT_ATT_DOUBLE varId(3',
     &                 err,bi,bj,myThid )

                ENDIF
              ENDDO !DO num_var

              err = NF_ENDDEF( fid2 )
              CALL PROFILES_NF_ERROR( 'MAKE_NCFILE: NF_ENDDEF fid2',
     &             err,bi,bj,myThid )

            ELSE !IF (.NOT.exst)
              JL = ILNBLNK( fnameequinc )
              err = NF_OPEN( fnameequinc(1:JL), NF_WRITE, fid2 )
              CALL PROFILES_NF_ERROR(
     &             'MAKE_NCFILE: NF_OPEN('//fnameequinc(1:JL)//')',
     &             err,bi,bj,myThid )

            ENDIF !IF (.NOT.exst)

C Write profiles
            DO num_var = 1, NVARMAX
              IF (vec_quantities(num_file,num_var,1,1).EQV..TRUE.)
     &        THEN
                DO prof_num = 1, NOBSGLOB
                  IF (prof_num.LE.length) THEN
                    DO k = 1, Zlength
                      prof_traj1D(k) =
     &                 prof_modval_glo(num_var,k,prof_num)

                      prof_mask1D(k) =
     &                 prof_mask_glo(num_var,k,prof_num)

                    ENDDO

                    vec_start(1) = 1
                    vec_start(2) = prof_num
                    vec_count(1) = Zlength
                    vec_count(2) = 1

                    err = NF_INQ_VARID( fid2,
     &                    prof_namesequi(num_file,num_var),
     &                    varId2(2+(num_var-1)*2) )
                    CALL PROFILES_NF_ERROR(
     &                   'MAKE_NCFILE: NF_INQ_VARID prof_namesequi',
     &                   err,bi,bj,myThid )
                    err = NF_PUT_VARA_DOUBLE( fid2,
     &                    varId2(2+(num_var-1)*2),
     &                    vec_start, vec_count, prof_traj1D )
                    CALL PROFILES_NF_ERROR(
     &                  'MAKE_NCFILE: NF_PUT_VARA_DOUBLE prof_traj1D',
     &                  err,bi,bj,myThid )

                    err = NF_INQ_VARID( fid2,
     &                    prof_namesmask(num_file,num_var),
     &                    varId2(3+(num_var-1)*2) )
                    CALL PROFILES_NF_ERROR(
     &                   'MAKE_NCFILE: NF_INQ_VARID prof_namesmask',
     &                   err,bi,bj,myThid )
                    err = NF_PUT_VARA_DOUBLE( fid2,
     &                    varId2(3+(num_var-1)*2),
     &                    vec_start, vec_count, prof_mask1D )
                    CALL PROFILES_NF_ERROR(
     &                  'MAKE_NCFILE: NF_PUT_VARA_DOUBLE prof_mask1D',
     &                  err,bi,bj,myThid )

                  ENDIF !IF (prof_num.LE.length)
                ENDDO !DO prof_num
              ENDIF !IF (vec_quantities(num_file,num_var,1,1).EQV.
            ENDDO !DO num_var

            err = NF_CLOSE( fid2 )
            CALL PROFILES_NF_ERROR( 'MAKE_NCFILE: NF_CLOSE fid2',
     &           err,bi,bj,myThid )

          ENDIF !IF (IL.NE.0)
        ENDIF !IF (myProcId.EQ.0)
      ENDDO !DO num_file

      _END_MASTER( myThid )

      RETURN
      END
