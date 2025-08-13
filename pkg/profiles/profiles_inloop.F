#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_INLOOP

C !INTERFACE:
      SUBROUTINE PROFILES_INLOOP( myTime, myThid )

C     !DESCRIPTION:
C     Computes and writes model counterparts
C     for netcdf profiles data
C     Started: Gael Forget 15-March-2006

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myTime: time counter for this thread
C     myThid: my thread ID number
      _RL     myTime
      INTEGER myThid
C     !OUTPUT PARAMETERS:
CEOP

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      INTEGER kLev, iq
      INTEGER bi,bj,prof_num, num_file, num_var
      _RL prof_traj1D(NLEVELMAX),prof_mask1D(NLEVELMAX)
      INTEGER prof_i1D(NUM_INTERP_POINTS),prof_j1D(NUM_INTERP_POINTS)
      _RL prof_w1D(NUM_INTERP_POINTS)
# ifndef ALLOW_CTRL
      INTEGER optimcycle
# endif
# ifndef ALLOW_ECCO
      INTEGER i,j,k
# endif

# ifndef ALLOW_CTRL
      optimcycle = 0
# endif

# ifndef ALLOW_ECCO
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, Nr
            DO j = 1, sNy
              DO i =  1, sNx
                m_UE(i,j,k,bi,bj)=0. _d 0
                m_VN(i,j,k,bi,bj)=0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      CALL ROTATE_UV2EN_RL(
     U     uVel, vVel, m_UE, m_VN,
     I     .TRUE., .TRUE., .FALSE., Nr, myThid )
# endif /* not ALLOW_ECCO */

      _BEGIN_MASTER( myThid )

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO num_file = 1, NFILESPROFMAX
            DO prof_num = 1, NOBSGLOB
              IF (prof_num.LE.ProfNo(num_file,bi,bj)) THEN
                IF (
     &           (prof_time(num_file,prof_num,bi,bj).GE.myTime) .AND.
     &           (prof_time(num_file,prof_num,bi,bj).LT.
     &            (myTime+deltaTClock)) ) THEN
                  DO iq = 1, NUM_INTERP_POINTS
                    prof_i1D(iq) =
     &               prof_interp_i(num_file,prof_num,iq,bi,bj)
                    prof_j1D(iq) =
     &               prof_interp_j(num_file,prof_num,iq,bi,bj)
                    prof_w1D(iq) =
     &               prof_interp_weights(num_file,prof_num,iq,bi,bj)

                  ENDDO

                  DO num_var = 1, NVARMAX
                    DO kLev = 1, NLEVELMAX
                      prof_traj1D(kLev) = 0
                      prof_mask1D(kLev) = 0
                    ENDDO

                    IF (vec_quantities(num_file,num_var,bi,bj)) THEN
                      CALL PROFILES_INTERP( prof_traj1D,
     &                     prof_i1D,prof_j1D,prof_w1D,
     &                     prof_namesmod(num_file,num_var),
     &                     prof_itracer(num_file,num_var),
     &                     num_file,myTime,bi,bj,myThid )

                      DO kLev = 1, NLEVELMAX
                        prof_traj1D(kLev) = prof_traj1D(kLev)*
     &                   prof_facmod(num_file,num_var)
                      ENDDO

                      CALL ACTIVE_WRITE_PROFILE( num_file,
     &                     ProfDepthNo(num_file,bi,bj),prof_traj1D,
     &                     num_var,prof_num,optimcycle,bi,bj,myThid,
     &                     profiles_dummy(num_file,num_var,bi,bj) )

                    ENDIF !IF (vec_quantities(num_file,num_var,bi,bj)

                  ENDDO !DO num_var

                ENDIF !IF ((prof_time
              ENDIF !IF (ProfNo(num_file,bi,bj).NE.0)
            ENDDO !DO prof_num
          ENDDO !DO num_file
        ENDDO !DO bi
      ENDDO !DO bj

      _END_MASTER( myThid )

#endif /* ALLOW_PROFILES */

      RETURN
      END
