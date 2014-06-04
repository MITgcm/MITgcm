#include "LAYERS_OPTIONS.h"
C--  File layers_save.F:
C--   Contents
C--   o LAYERS_FILL_SURFACE_FLUX
C--   o LAYERS_FILL_DFX
C--   o LAYERS_FILL_DFY
C--   o LAYERS_FILL_DFR
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LAYERS_FILL_TFLUX
C     !INTERFACE:
      SUBROUTINE LAYERS_FILL_SURFACE_FLUX(
     I               surfflux, trIdentity,
     I               kLev, nLevs, bibjFlg, biArg, bjArg, myThid )
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_FULL_SURFACE_FLUX
C     | "Remember" the surface fluxes for use later in layers_thermodynamics
C     *==========================================================*
C     \ev
C***********************************************************************
C   This is designed to look and work exactly like the a regular
C   diagnostics_fill call.
C***********************************************************************
C     surfflux  :: The surface temperature flux, the same as what is filled into 
C                   the TFLUX and SFLUX diagnostics
C     trIdentity:: Index to let us know what tracer it is (1 for T, 2 for S)
C     kLev      :: Integer flag for vertical levels:
C                  > 0 (any integer): WHICH single level to increment in qdiag.
C                  0,-1 to increment "nLevs" levels in qdiag,
C                  0 : fill-in in the same order as the input array
C                  -1: fill-in in reverse order.
C                   this is a DUMMY ARGUMENT here. Not used!
C     nLevs     :: indicates Number of levels of the input field array
C                  (whether to fill-in all the levels (kLev<1) or just one (kLev>0))
C                   this is a DUMMY ARGUMENT here. Not used!
C     bibjFlg   :: Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array (with overlap regions)
C                  3 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array
C                     AND that the array has no overlap region (interior only)
C                  NOTE - bibjFlg can be NEGATIVE to indicate not to increment counter
C     biArg     :: X-direction tile number - used for bibjFlg=1-3
C     bjArg     :: Y-direction tile number - used for bibjFlg=1-3
C     myThid    :: my thread Id number
C***********************************************************************
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjFlg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************

      _RL surfflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER trIdentity, kLev, nLevs, bibjFlg, biArg, bjArg
      INTEGER myThid
CEOP

#ifdef LAYERS_THERMODYNAMICS

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C msgBuf           :: error message buffer
      INTEGER i,j,bi,bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C -- should be called as:
C CALL LAYERS_FILL_TFLUX( tmp1k, 0,1,0,1,1,myThid )
C
C  This is to make the call look as much as possible like the diagnostics call.
C  However, all of the arguments after tmp1k are NOT USED!
C  This is potentially misleading. However it seems wise to keep in mind that
C  diagnsostics are filled in all sorts of different ways.

C -- only operate on T and S
       IF ((trIdentity.EQ.1).OR.(trIdentity.EQ.2)) THEN
 
        IF ( (kLev.EQ.0) .AND. (nLevs.EQ.1) .AND. (bibjFlg.EQ.0) 
     &       .AND. ((trIdentity.EQ.1) .OR. (trIdentity.EQ.2)) ) THEN
         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
C -- This is how the loops are computed in diagnostics_fill, where there is not
C -- necessarily a halo in the variable
C           DO j = 1,jRun
C            DO i = 1,iRun
C -- But here we need to explicitly fill the halo in order to compute flux divergence
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             layers_surfflux(i,j,1,trIdentity,bi,bj) = 
     &       layers_surfflux(i,j,1,trIdentity,bi,bj) +
     &              surfflux(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ELSE
C -- raise an error if this gets called in an unexpected way
         WRITE(msgBuf,'(2A)')
     &          'S/R LAYERS_FILL_SURFACE_FLUX: ',
     &          'was called in an unexpected way'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R LAYERS_FILL_SURFACE_FLUX'
        ENDIF

      ENDIF
#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
C end of S/R LAYERS_FILL_SURFACE_FLUX

      SUBROUTINE LAYERS_FILL_DFX(
     I               df, trIdentity,
     I               kLev, nLevs, bibjFlg, biArg, bjArg, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_FILL_DFX
C     | "Remember" the zonal diffusive flux for use later in layers_thermodynamics
C     *==========================================================*
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

       _RL df(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER trIdentity, kLev, nLevs, bibjFlg, biArg, bjArg
       INTEGER myThid

#ifdef LAYERS_THERMODYNAMICS

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C msgBuf           :: error message buffer
       INTEGER i,j
       CHARACTER*(MAX_LEN_MBUF) msgBuf

C    CALL LAYERS_FILL_DFX( df, trIdentity, k, 1, 2,bi,bj, myThid )
C -- only operate on T and S
       IF ((trIdentity.EQ.1).OR.(trIdentity.EQ.2)) THEN
 
C -- expect to be called INSIDE the bi-bj loop, with overlap present (bibjFlg=2)
  
       IF ( (nLevs.EQ.1) .AND. (bibjFlg.EQ.2) 
     &     .AND. ((trIdentity.EQ.1) .OR. (trIdentity.EQ.2)) ) THEN  
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          layers_dfx(i,j,kLev,trIdentity,biArg,bjArg) = 
     &      layers_dfx(i,j,kLev,trIdentity,biArg,bjArg) +
     &      df(i,j)
         ENDDO
        ENDDO
       ELSE
C -- raise an error if this gets called in an unexpected way
        WRITE(msgBuf,'(2A)')
     &          'S/R LAYERS_FILL_DFX: ',
     &          'was called in an unexpected way'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R LAYERS_FILL_DFX'
       ENDIF
      ENDIF
#endif /* LAYERS_THERMODYNAMICS */    
      RETURN
      END
C end of S/R LAYERS_FILL_DFX
            
      SUBROUTINE LAYERS_FILL_DFY(
     I               df, trIdentity,
     I               kLev, nLevs, bibjFlg, biArg, bjArg, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_FILL_DFY
C     | "Remember" the merid. diffusive flux for use later in layers_thermodynamics
C     *==========================================================*
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

       _RL df(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER trIdentity, kLev, nLevs, bibjFlg, biArg, bjArg
       INTEGER myThid

#ifdef LAYERS_THERMODYNAMICS

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C msgBuf           :: error message buffer
       INTEGER i,j
       CHARACTER*(MAX_LEN_MBUF) msgBuf

C    CALL LAYERS_FILL_DFY( df, trIdentity, k, 1, 2,bi,bj, myThid )
C -- only operate on T and S
       IF ((trIdentity.EQ.1).OR.(trIdentity.EQ.2)) THEN
 
C -- expect to be called INSIDE the bi-bj loop, with overlap present (bibjFlg=2)
 
       IF ( (nLevs.EQ.1) .AND. (bibjFlg.EQ.2) 
     &     .AND. ((trIdentity.EQ.1) .OR. (trIdentity.EQ.2)) ) THEN  
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          layers_dfy(i,j,kLev,trIdentity,biArg,bjArg) = 
     &      layers_dfy(i,j,kLev,trIdentity,biArg,bjArg) +
     &      df(i,j)
         ENDDO
        ENDDO
       ELSE
C -- raise an error if this gets called in an unexpected way
        WRITE(msgBuf,'(2A)')
     &          'S/R LAYERS_FILL_DFY: ',
     &          'was called in an unexpected way'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R LAYERS_FILL_DFY'
       ENDIF    
      
      ENDIF
#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
C end of S/R LAYERS_FILL_DFY

      SUBROUTINE LAYERS_FILL_DFR(
     I               df, trIdentity,
     I               kLev, nLevs, bibjFlg, biArg, bjArg, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_FILL_DFR
C     | "Remember" the vert. diffusive flux for use later in layers_thermodynamics
C     *==========================================================*
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

       _RL df(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER trIdentity, kLev, nLevs, bibjFlg, biArg, bjArg
       INTEGER myThid

#ifdef LAYERS_THERMODYNAMICS

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C msgBuf           :: error message buffer
       INTEGER i,j
       CHARACTER*(MAX_LEN_MBUF) msgBuf

C    CALL LAYERS_FILL_DFY( df, trIdentity, k, 1, 2,bi,bj, myThid )
C -- only operate on T and S
       IF ((trIdentity.EQ.1).OR.(trIdentity.EQ.2)) THEN
 
C -- expect to be called INSIDE the bi-bj loop, with overlap present (bibjFlg=2)

       IF ( (nLevs.EQ.1) .AND. (bibjFlg.EQ.2) 
     &     .AND. ((trIdentity.EQ.1) .OR. (trIdentity.EQ.2)) ) THEN  
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          layers_dfr(i,j,kLev,trIdentity,biArg,bjArg) = 
     &      layers_dfr(i,j,kLev,trIdentity,biArg,bjArg) +
     &      df(i,j)
         ENDDO
        ENDDO
       ELSE
C -- raise an error if this gets called in an unexpected way
        WRITE(msgBuf,'(2A)')
     &          'S/R LAYERS_FILL_DFY: ',
     &          'was called in an unexpected way'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R LAYERS_FILL_DFY'
       ENDIF    

      ENDIF
#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
C end of S/R LAYERS_FILL_DFR




