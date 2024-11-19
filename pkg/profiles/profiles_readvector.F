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
      INTEGER vec_loc_length, vNb, k, kk, kkk,bi,bj
      INTEGER irec, fNb, myThid,err,varid1,tmpprofno
      _RL vec_loc(vec_loc_length)
CEOP

C     !LOCAL VARIABLES:
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
        err = NF_INQ_DIMID(fiddata(fNb,bi,bj),'iPROF', varid1)
        CALL PROFILES_NF_ERROR( 'readvector 1: NF_INQ_DIMID',
     &       err,bi,bj,myThid )
        err = NF_INQ_DIMLEN( fiddata(fNb,bi,bj), varid1, tmpprofno )
        CALL PROFILES_NF_ERROR( 'readvector 1: NF_INQ_DIMLEN',
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

        DO kkk = 1, NVARMAX
          IF (vec_quantities(fNb,kkk,bi,bj).EQV..TRUE.) THEN
            err = NF_INQ_VARID( fiddata(fNb,bi,bj),prof_names(fNb,kkk),
     &            varid1 )
            CALL PROFILES_NF_ERROR( 'readvector 3: NF_INQ_VARID',
     &            err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fiddata(fNb,bi,bj),varid1, 
     &            vec_start,vec_count, vec_tmp1 )
            CALL PROFILES_NF_ERROR( 'readvector 4: NF_GET_VARA_DOUBLE',
     &           err,bi,bj,myThid )
            err = NF_INQ_VARID( fiddata(fNb,bi,bj),
     &            prof_namesweight(fNb,kkk),varid1 )
            CALL PROFILES_NF_ERROR( 'readvector 5: NF_INQ_VARID',
     &           err,bi,bj,myThid )
            err = NF_GET_VARA_DOUBLE( fiddata(fNb,bi,bj),
     &            varid1,vec_start,vec_count,vec_tmp2 )
            CALL PROFILES_NF_ERROR( 'readvector 6: NF_GET_VARA_DOUBLE',
     &            err,bi,bj,myThid )
# ifdef ALLOW_PROFILES_CLIMMASK
            err = NF_INQ_VARID( fiddata(fNb,bi,bj),
     &            prof_namesclim(fNb,kkk), varid1 )
            err = NF_GET_VARA_DOUBLE( fiddata(fNb,bi,bj),
     &            varid1,vec_start,vec_count,vec_tmp3 )
# endif

            IF (err.NE.NF_NOERR) THEN
              WRITE(errorMessageUnit,'(A)')
     &         'WARNING PROFILES_READVECTOR: record not found!!'
            ENDIF

            DO k = 1, vec_count(1)
              DO kk = 1, vec_count(2)
                profiles_data_buff(k,kk,kkk,bi,bj) =
     &           vec_tmp1((kk-1)*vec_count(1)+k)

                profiles_weight_buff(k,kk,kkk,bi,bj) =
     &           vec_tmp2((kk-1)*vec_count(1)+k)

# ifdef ALLOW_PROFILES_CLIMMASK
                IF (vec_tmp3((kk-1)*vec_count(1)+k).LE.-990. _d 0)
     &              profiles_weight_buff(k,kk,kkk,bi,bj) = 0. _d 0

# endif
              ENDDO !DO kk
            ENDDO !DO k

          ENDIF !IF (vecquantities
        ENDDO !DO kkk

        profiles_curfile_buff(bi,bj) = fNb
      ENDIF !IF ( (irec.LT.profiles_minid

C Get vec_loc from the buffer
      IF (vNb.LT.0) THEN
        DO k=1,vec_loc_length
          vec_loc(k) = profiles_weight_buff
     &     (k,irec-profiles_minind_buff(bi,bj)+1,-vNb,bi,bj)
        ENDDO

      ELSE
        DO k=1,vec_loc_length
          vec_loc(k) = profiles_data_buff
     &     (k,irec-profiles_minind_buff(bi,bj)+1,vNb,bi,bj)
        ENDDO
      ENDIF

#endif /* ALLOW_PROFILES */

      RETURN
      END
