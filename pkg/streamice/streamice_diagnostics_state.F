#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_DIAGNOSTICS_STATE( myTime, myIter, myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"

      _RL myTime
      INTEGER myIter, myThid

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_OPENAD
      INTEGER i, j, bi, bj

      _RL u_streamice_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_streamice_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u_surf_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_surf_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL h_streamice_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL area_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL float_frac_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surf_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef STREAMICE_HYBRID_STRESS
      _RL streamice_taubx_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_tauby_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_STREAMICE_2DTRACER
       _RL trac2d_diag
#endif

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           u_streamice_diag(i,j,bi,bj) = u_streamice(i,j,bi,bj)%v
           v_streamice_diag(i,j,bi,bj) = v_streamice(i,j,bi,bj)%v
           h_streamice_diag(i,j,bi,bj) = H_streamice(i,j,bi,bj)%v
           area_diag(i,j,bi,bj) = area_shelf_streamice(i,j,bi,bj)%v
#ifdef STREAMICE_SMOOTH_FLOATATION2
           float_frac_diag(i,j,bi,bj) =
     &         float_frac_streamice(i,j,bi,bj)%v
#endif
           surf_diag(i,j,bi,bj) =
     &         surf_el_streamice(i,j,bi,bj)%v
#ifdef STREAMICE_HYBRID_STRESS
           streamice_taubx_diag(i,j,bi,bj) =
     $         streamice_taubx(i,j,bi,bj)%v
           streamice_tauby_diag(i,j,bi,bj) =
     $         streamice_tauby(i,j,bi,bj)%v
           u_surf_diag(i,j,bi,bj) = streamice_u_surf(i,j,bi,bj)%v
           v_surf_diag(i,j,bi,bj) = streamice_v_surf(i,j,bi,bj)%v
#endif
#ifdef ALLOW_STREAMICE_2DTRACER
           trac2d_diag(i,j,bi,bj)=trac2d(i,j,bi,bj)%v
#endif

         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_OPENAD */

      IF ( useDiagnostics ) THEN
       WRITE(msgBuf,'(A)') 'CALLING FILL DIAGNOSTICS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

#ifdef ALLOW_OPENAD

       CALL DIAGNOSTICS_FILL(U_streamice_diag,'SI_Uvel ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(V_streamice_diag,'SI_Vvel ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(H_streamice_diag,'SI_Thick',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(area_diag,'SI_area ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(surf_diag,'SI_selev',
     &      0,1,0,1,1,myThid)

#else

       CALL DIAGNOSTICS_FILL(U_streamice,'SI_Uvel ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(V_streamice,'SI_Vvel ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(H_streamice,'SI_Thick',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(area_shelf_streamice,'SI_area ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(float_frac_streamice,'SI_float',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(surf_el_streamice,'SI_selev',
     &      0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(bdot_streamice,'BDOT_SI ',
     &      0,1,0,1,1,myThid)

#endif

#ifdef ALLOW_OPENAD
#ifdef STREAMICE_SMOOTH_FLOATATION2
       CALL DIAGNOSTICS_FILL(float_frac_diag,'SI_float',
     &      0,1,0,1,1,myThid)
#else
       CALL DIAGNOSTICS_FILL(float_frac_streamice,'SI_float',
     &      0,1,0,1,1,myThid)
#endif
#endif

       CALL DIAGNOSTICS_FILL_RS(STREAMICE_hmask,'SI_hmask',
     &      0,1,0,1,1,myThid)
#ifdef STREAMICE_HYBRID_STRESS
       CALL DIAGNOSTICS_FILL(streamice_u_bed,'SI_ubase',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(STREAMICE_v_bed,'SI_vbase',
     &      0,1,0,1,1,myThid)

#ifdef ALLOW_OPENAD
       CALL DIAGNOSTICS_FILL(streamice_taubx_diag,'SI_taubx',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(STREAMICE_tauby_diag,'SI_tauby',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(u_surf_diag,'SI_usurf',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(v_surf_diag,'SI_vsurf',
     &      0,1,0,1,1,myThid)
#else
       CALL DIAGNOSTICS_FILL(streamice_taubx,'SI_taubx',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(STREAMICE_tauby,'SI_tauby',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(streamice_u_surf,'SI_usurf',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(streamice_v_surf,'SI_vsurf',
     &      0,1,0,1,1,myThid)
#endif

#endif

#ifdef ALLOW_STREAMICE_2DTRACER
#ifdef ALLOW_OPENAD
       CALL DIAGNOSTICS_FILL(trac2d_diag,'SI2Dtrac',
     &      0,1,0,1,1,myThid)
#else
       CALL DIAGNOSTICS_FILL(trac2d,'SI2Dtrac',
     &      0,1,0,1,1,myThid)
#endif
#endif

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_STREAMICE */
      RETURN
      END
