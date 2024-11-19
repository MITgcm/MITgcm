#include "PROFILES_OPTIONS.h"


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_READPARMS

C !INTERFACE:
      SUBROUTINE PROFILES_READVECTOR(fNb, vNb, irec,
     & vec_loc_length, vec_loc , bi,bj, myThid)

C     !DESCRIPTION:
C     Reads a profile from a netcdf profiles data file
C     Started: Gael Forget 15-March-2006

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
# include "netcdf.inc"
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER vec_loc_length,vNb,bi,bj
      INTEGER irec,fNb,myThid
      _RL vec_loc(vec_loc_length)
CEOP

C     !LOCAL VARIABLES:
      INTEGER num_var,v1,v2
      INTEGER err,varId1,tmpprofno
#ifdef ALLOW_PROFILES
      INTEGER vec_start(2),vec_count(2)
      _RL vec_tmp1(1000*NLEVELMAX),vec_tmp2(1000*NLEVELMAX)
# ifdef ALLOW_PROFILES_CLIMMASK
      _RL vec_tmp3(1000*NLEVELMAX)
# endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( (irec.LT.profiles_minind_buff(bi,bj)) .OR.
     &     (irec.GT.profiles_maxind_buff(bi,bj)) .OR.
     &     (profiles_curfile_buff(bi,bj).NE.fNb) ) THEN
        err = NF_INQ_DIMID(fiddata(fNb,bi,bj),'iPROF', varId1)
        CALL PROFILES_NF_ERROR( 'READVECTOR: NF_INQ_DIMID fiddata',
     &       err,bi,bj,myThid )
        err = NF_INQ_DIMLEN( fiddata(fNb,bi,bj), varId1, tmpprofno )
        CALL PROFILES_NF_ERROR( 'READVECTOR: NF_INQ_DIMLEN tmpprofno',
     &       err,bi,bj,myThid )

        IF (profiles_curfile_buff(bi,bj).NE.fNb) THEN
C No asumption on whether a forward or a backward loop is calling
          profiles_minind_buff(bi,bj) = MAX(1,irec-500+1)
          profiles_maxind_buff(bi,bj) = MIN(tmpprofno,irec+500)

        ELSEIF (irec.LT.profiles_minind_buff(bi,bj)) THEN
C Implies that a backward loop is calling
          profiles_minind_buff(bi,bj) = MAX(1,irec-999)
          profiles_maxind_buff(bi,bj) = irec

        ELSE
C Implies that a forward loop is calling
          profiles_minind_buff(bi,bj) = irec
          profiles_maxind_buff(bi,bj) = MIN(tmpprofno,irec+999)

        ENDIF !IF (profiles_curfile_buff(bi,bj).NE.fNb)

# ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevD ) THEN
          WRITE(msgBuf,'(A,5I9)')
     &     'buffer readvector ',
     &     profiles_minind_buff(bi,bj), profiles_maxind_buff(bi,bj),
     &     irec, profNo(fNb,bi,bj), tmpprofno
          CALL PRINT_MESSAGE( msgBuf,
     &         standardmessageunit, SQUEEZE_RIGHT, myThid )
        ENDIF
# endif

        vec_start(1) = 1
        vec_start(2) = profiles_minind_buff(bi,bj)
        vec_count(1) = vec_loc_length
        vec_count(2) =
     &   profiles_maxind_buff(bi,bj)-profiles_minind_buff(bi,bj)+1

        DO num_var = 1, NVARMAX
          IF (vec_quantities(fNb,num_var,bi,bj).EQV..TRUE.) THEN
            err = NF_INQ_VARID( fiddata(fNb,bi,bj),
     &            prof_names(fNb,num_var), varId1 )
            CALL PROFILES_NF_ERROR(
     &           'READVECTOR: NF_INQ_VARID prof_names',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fiddata(fNb,bi,bj), varId1,
     &            vec_start, vec_count, vec_tmp1 )
            CALL PROFILES_NF_ERROR(
     &           'READVECTOR: NF_GET_VARA_DOUBLE vec_tmp1',
     &           err,bi,bj,myThid )

            err = NF_INQ_VARID( fiddata(fNb,bi,bj),
     &            prof_namesweight(fNb,num_var), varId1 )
            CALL PROFILES_NF_ERROR(
     &           'READVECTOR: NF_INQ_VARID prof_namesweight',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fiddata(fNb,bi,bj),
     &            varId1, vec_start, vec_count, vec_tmp2 )
            CALL PROFILES_NF_ERROR(
     &           'READVECTOR: NF_GET_VARA_DOUBLE vec_tmp2',
     &           err,bi,bj,myThid )

# ifdef ALLOW_PROFILES_CLIMMASK
            err = NF_INQ_VARID( fiddata(fNb,bi,bj),
     &            prof_namesclim(fNb,num_var),varId1 )
            CALL PROFILES_NF_ERROR(
     &           'READVECTOR: NF_INQ_VARID prof_namesclim',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fiddata(fNb,bi,bj),
     &            varId1,vec_start,vec_count, vec_tmp3 )
            CALL PROFILES_NF_ERROR(
     &           'READVECTOR: NF_GET_VARA_DOUBLE vec_tmp3',
     &           err,bi,bj,myThid )
# endif

            IF (err.NE.NF_NOERR) THEN
              WRITE(errorMessageUnit,'(A)')
     &         'WARNING PROFILES_READVECTOR: record not found!!'
            ENDIF

            DO v1 = 1, vec_count(1)
              DO v2 = 1, vec_count(2)
                profiles_data_buff(v1,v2,num_var,bi,bj) =
     &           vec_tmp1((v2-1)*vec_count(1)+v1)

                profiles_weight_buff(v1,v2,num_var,bi,bj) =
     &           vec_tmp2((v2-1)*vec_count(1)+v1)

# ifdef ALLOW_PROFILES_CLIMMASv1
                IF (vec_tmp3((v2-1)*vec_count(1)+v1).LE.-990. _d 0)
     &            profiles_weight_buff(v1,v2,num_var,bi,bj) = 0. _d 0

# endif
              ENDDO !DO v2
            ENDDO !DO v1

          ENDIF !IF (vec_quantities
        ENDDO !DO num_var

        profiles_curfile_buff(bi,bj) = fNb
      ENDIF !IF ( (irec.LT.profiles_minid

C Get vec_loc from the buffer
      IF (vNb.LT.0) THEN
        DO v1 = 1, vec_loc_length
          vec_loc(v1) = profiles_weight_buff
     &     (v1,irec-profiles_minind_buff(bi,bj)+1,-vNb,bi,bj)
        ENDDO

      ELSE
        DO v1 = 1, vec_loc_length
          vec_loc(v1) = profiles_data_buff
     &     (v1,irec-profiles_minind_buff(bi,bj)+1,vNb,bi,bj)
        ENDDO
      ENDIF

#endif /* ALLOW_PROFILES */

      RETURN
      END
