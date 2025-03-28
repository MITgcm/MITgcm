C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PROFILES_OPTIONS.h"

CBOP
C     !ROUTINE: PROFILES_MAKE_NCFILE
C     !INTERFACE:
      SUBROUTINE PROFILES_MAKE_NCFILE( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R PROFILES_MAKE_NCFILE
C     | o combine tiled files into one global netcdf file of
C     |   model-equivalent profiles
C     *==================================================================
C     \ev

      IMPLICIT NONE
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

C !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     ========= Local variables =======================
      INTEGER num_file,num_var,prof_num,k
      INTEGER bi,bj,iG,jG
      INTEGER err,dimid,irec,fid1,fid2
      INTEGER dimid1,dimid2,vecid(2)
      INTEGER varid0,varid1(NVARMAX*2),varid2(NVARMAX*2)
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
      DO num_file=1,NFILESPROFMAX
       DO num_var=1,NVARMAX
        IL = ILNBLNK( prof_names(num_file,num_var) )
        WRITE(prof_namesequi(num_file,num_var),'(2A)')
     &       prof_names(num_file,num_var)(1:IL),'model'
       ENDDO
      ENDDO

C Initialize buffers
      DO num_var=1,NVARMAX
       DO prof_num=1,NOBSGLOB
        DO k=1,NLEVELMAX
         prof_buff(num_var,k,prof_num) = 0.
         prof_mask_buff(num_var,k,prof_num) = 0.
        ENDDO
       ENDDO
      ENDDO

C Loop over files
      DO num_file=1,NFILESPROFMAX

C File maintenance
       DO bj=1,nSy
        DO bi=1,nSx

         IF ( (ProfNo(num_file,bi,bj).GT.0).AND.
     &        (profilesDoNcOutput) ) THEN
C Need to close the file so that the data is not lost when run finishes
          err = NF_CLOSE(fidforward(num_file,bi,bj))
          iG  = bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
          jG  = bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
          IL  = ILNBLNK( profilesFiles(num_file) )
          WRITE(profFile,'(1a)')
     &    profilesFiles(num_file)(1:IL)
          IL  = ILNBLNK( profFile )
          JL  = ILNBLNK( profilesDir )
          WRITE(fnameequinc,'(3a,i3.3,a,i3.3,a)')
     &    profilesDir(1:JL),profFile(1:IL),'.',iG,'.',jG,'.equi.nc'
          err = NF_OPEN(fnameequinc,NF_NOWRITE,
     &    fidforward(num_file,bi,bj))
         ENDIF

C Loop over variables
         DO num_var=1,NVARMAX
          IF (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) THEN

C Loop over profiles
           DO prof_num=1,NOBSGLOB
            IF (prof_num.LE.ProfNo(num_file,bi,bj)) THEN

C Initialize
             DO k=1,NLEVELMAX
              prof_traj1D(k)=0.
              prof_mask1D(k)=0.
              prof_buff(num_var,k,prof_num)=0.
              prof_mask_buff(num_var,k,prof_num)=0.
             ENDDO

C Read tiled files
             CALL active_read_profile(num_file,
     &          ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &          prof_num,.false.,optimcycle,bi,bj,myThid,
     &          profiles_dummy(num_file,num_var,bi,bj))

C Save model equi and masks in buffer
C Combine all threads
             irec = prof_ind_glob(num_file,prof_num,bi,bj)

             DO k=1,ProfDepthNo(num_file,bi,bj)
              prof_buff(num_var,k,irec) = prof_buff(num_var,k,irec)
     &                               +prof_traj1D(k)
              prof_mask_buff(num_var,k,irec) =
     &                               prof_mask_buff(num_var,k,irec)
     &                               +prof_mask1D_cur(k,bi,bj)
             ENDDO

            ENDIF !if (prof_num.LE.ProfNo(num_file,bi,bj)) then
           ENDDO !do prof_num=..
          ENDIF
         ENDDO !do num_var...

        ENDDO !bj
       ENDDO !bi

C Combine all processes
       DO num_var=1,NVARMAX
        DO prof_num=1,NOBSGLOB
         DO k=1,NLEVELMAX

          tmpgs = prof_buff(num_var,k,prof_num)
          _GLOBAL_SUM_RL(tmpgs, myThid)
          prof_modval_glo(num_var,k,prof_num) = tmpgs
          tmpgs = prof_mask_buff(num_var,k,prof_num)
          _GLOBAL_SUM_RL(tmpgs, myThid)
          prof_mask_glo(num_var,k,prof_num) = tmpgs

         ENDDO
        ENDDO !do prof_num=..
       ENDDO !do num_var=..

       IF ( myProcId .EQ. 0 ) THEN

C Get dimensions of input file
        profFile=' '
        IL = ILNBLNK( profilesfiles(num_file) )
        IF (IL.NE.0) THEN
         WRITE(profFile,'(1a)')
     &   profilesfiles(num_file)(1:IL)
        ENDIF

        IL = ILNBLNK( profFile )
        IF (IL.NE.0) THEN
         WRITE(fnamedata,'(2a)') profFile(1:IL),'.nc'
         err = NF_OPEN(fnamedata, 0, fid1)
         err = NF_INQ_DIMID(fid1,'iPROF', dimid )
         err = NF_INQ_DIMLEN(fid1, dimid, length )
         err = NF_INQ_DIMID(fid1,'iDEPTH', dimid )
         IF (err.NE.NF_NOERR) THEN
          err = NF_INQ_DIMID(fid1,'Z', dimid )
         ENDIF
         err = NF_INQ_DIMLEN(fid1, dimid, Zlength )

C Create new netcdf global file for model-equivalent
         JL  = ILNBLNK( profilesDir )
         WRITE(fnameequinc,'(3a)')
     &    profilesDir(1:JL),profFile(1:IL),'.equi.nc'

         inquire( file=fnameequinc, exist=exst )
         IF (.NOT.exst) THEN

          err = NF_CREATE(fnameequinc,NF_CLOBBER,fid2)
          err = NF_DEF_DIM(fid2,'iDEPTH',Zlength,dimid1)
          err = NF_DEF_DIM(fid2,'iPROF',length,dimid2)
          vecid(1)=dimid1
          vecid(2)=dimid2

C Define variables and attributes
          DO num_var=1,NVARMAX

           err = NF_INQ_VARID(fid1,prof_names(num_file,num_var),varid0)

           IF (err.EQ.NF_NOERR) THEN

            err = NF_DEF_VAR(fid2,prof_namesequi(num_file,num_var),
     &        NF_DOUBLE,2,vecid,varid1(2+(num_var-1)*2))
            err = NF_PUT_ATT_DOUBLE(fid2,varid1(2+(num_var-1)*2),
     &        '_FillValue',NF_DOUBLE,1, 0. _d 0)
            err = NF_DEF_VAR(fid2,prof_namesmask(num_file,num_var),
     &        NF_DOUBLE, 2,vecid,varid1(3+(num_var-1)*2))
            err = NF_PUT_ATT_DOUBLE(fid2,varid1(3+(num_var-1)*2),
     &        '_FillValue',NF_DOUBLE,1, 0. _d 0)

           ENDIF
          ENDDO

          err = NF_ENDDEF(fid2)

         ELSE
          err = NF_OPEN(fnameequinc, NF_WRITE, fid2)
         ENDIF

C Write profiles
         DO num_var=1,NVARMAX
          IF (vec_quantities(num_file,num_var,1,1).EQV..TRUE.) THEN
           DO prof_num=1,NOBSGLOB
            IF (prof_num.LE.length) THEN

             DO k=1,Zlength
              prof_traj1D(k)=prof_modval_glo(num_var,k,prof_num)
              prof_mask1D(k)=prof_mask_glo(num_var,k,prof_num)
             ENDDO

             vec_start(1)=1
             vec_start(2)=prof_num
             vec_count(1)=Zlength
             vec_count(2)=1

             err = NF_INQ_VARID(fid2,prof_namesequi(num_file,num_var),
     &         varid2(2+(num_var-1)*2) )
             err = NF_PUT_VARA_DOUBLE(fid2, varid2(2+(num_var-1)*2),
     &         vec_start, vec_count, prof_traj1D)
             err = NF_INQ_VARID(fid2,prof_namesmask(num_file,
     &         num_var), varid2(3+(num_var-1)*2) )
             err = NF_PUT_VARA_DOUBLE(fid2, varid2(3+(num_var-1)*2),
     &         vec_start, vec_count, prof_mask1D)

            ENDIF !if (vec_quantities(num_file,num_var,1,1).EQV..TRUE.)
           ENDDO !do prof_num=..
          ENDIF !if (prof_num.LE.length)
         ENDDO !do num_var=..

         err = NF_CLOSE(fid2)
        ENDIF
       ENDIF
      ENDDO

      _END_MASTER( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
