#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif

CBOP
C     !ROUTINE: UPDATE_SIGMA
C     !INTERFACE:
      SUBROUTINE UPDATE_SIGMA( etaHc, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_SIGMA
C     | o Update the thickness fractions (hFacC,W,S)
C     |   according to the surface r-position = Non-Linear FrSurf
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
c#ifdef ALLOW_AUTODIFF_TAMC
c# include "tamc.h"
c#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     etaHc  :: surface r-anomaly at grid cell center
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id. number
      _RL etaHc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef NONLIN_FRSURF
c#ifndef DISABLE_SIGMA_CODE
C     !LOCAL VARIABLES:
C     Local variables
C     bi, bj     :: tile indices
C     i, j, k    :: Loop counters
C     rEmpty     :: empty column r-position
C     rFullDepth :: maximum depth of a full column
C     tmpFld     :: Temporary array used to compute & write Total Depth
C     msgBuf     :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, k
      _RL rFullDepth
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL hFactmp
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      rFullDepth = rF(1)-rF(Nr+1)

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

c#ifdef ALLOW_AUTODIFF_TAMC
c          act1 = bi - myBxLo(myThid)
c          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
c          act2 = bj - myByLo(myThid)
c          max2 = myByHi(myThid) - myByLo(myThid) + 1
c          act3 = myThid - 1
c          max3 = nTx*nTy
c          act4 = ikey_dynamics - 1
c          idynkey = (act1 + 1) + act2*max1
c     &                      + act3*max1*max2
c     &                      + act4*max1*max2*max3
c#endif /* ALLOW_AUTODIFF_TAMC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c#ifdef ALLOW_OBCS
c# ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE rStarFacC(:,:,bi,bj) =
cCADJ &     comlev1_bibj, key = idynkey, byte = isbyte
cCADJ STORE rStarFacS(:,:,bi,bj) =
cCADJ &     comlev1_bibj, key = idynkey, byte = isbyte
cCADJ STORE rStarFacW(:,:,bi,bj) =
cCADJ &     comlev1_bibj, key = idynkey, byte = isbyte
c# endif /* ALLOW_AUTODIFF_TAMC */
cC-- Apply OBC to rStar_Factor_W,S before updating hFacW,S
c       IF (useOBCS) THEN
c        CALL OBCS_APPLY_R_STAR(
c    I                    bi, bj,
c    U                    rStarFacC, rStarFacW, rStarFacS,
c    I                    myTime, myIter, myThid )
c       ENDIF
c#endif /* ALLOW_OBCS */

C-- Update the fractional thickness hFacC (& "recip_hFac") :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kSurfC(i,j,bi,bj).LE.Nr ) THEN
           tmpFld(i,j) = etaHc(i,j,bi,bj)
     &                 + ( Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj) )
          ELSE
           tmpFld(i,j) = rFullDepth
          ENDIF
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            hFacC(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
     &                         *( dAHybSigF(k)*rFullDepth
     &                           +dBHybSigF(k)*tmpFld(i,j)
     &                          )*recip_drF(k)
            recip_hFacC(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)*drF(k)
     &                         /( dAHybSigF(k)*rFullDepth
     &                           +dBHybSigF(k)*tmpFld(i,j)
     &                          )
          ENDDO
         ENDDO
        ENDDO

C-- Update the fractional thickness hFacW (& "recip_hFac") :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kSurfW(i,j,bi,bj).LE.Nr ) THEN
           tmpFld(i,j) = etaHw(i,j,bi,bj)
     &                 + ( rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj) )
          ELSE
           tmpFld(i,j) = rFullDepth
          ENDIF
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            hFacW(i,j,k,bi,bj) = maskW(i,j,k,bi,bj)
     &                         *( dAHybSigF(k)*rFullDepth
     &                           +dBHybSigF(k)*tmpFld(i,j)
     &                          )*recip_drF(k)
            recip_hFacW(i,j,k,bi,bj) = maskW(i,j,k,bi,bj)*drF(k)
     &                         /( dAHybSigF(k)*rFullDepth
     &                           +dBHybSigF(k)*tmpFld(i,j)
     &                          )
          ENDDO
         ENDDO
        ENDDO

C-- Update the fractional thickness hFacS (& "recip_hFac") :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kSurfS(i,j,bi,bj).LE.Nr ) THEN
           tmpFld(i,j) = etaHs(i,j,bi,bj)
     &                 + ( rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj) )
          ELSE
           tmpFld(i,j) = rFullDepth
          ENDIF
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            hFacS(i,j,k,bi,bj) = maskS(i,j,k,bi,bj)
     &                         *( dAHybSigF(k)*rFullDepth
     &                           +dBHybSigF(k)*tmpFld(i,j)
     &                          )*recip_drF(k)
            recip_hFacS(i,j,k,bi,bj) = maskS(i,j,k,bi,bj)*drF(k)
     &                         /( dAHybSigF(k)*rFullDepth
     &                           +dBHybSigF(k)*tmpFld(i,j)
     &                          )
          ENDDO
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- end bi,bj loop
       ENDDO
      ENDDO

c     _EXCH_XYZ_RS( hFacC, myThid )
c     _EXCH_XYZ_RS( recip_hFacC, myThid )
c     CALL EXCH_UV_XYZ_RS(hFacW,hFacS,.FALSE.,myThid)
c     CALL EXCH_UV_XYZ_RS(recip_hFacW,recip_hFacS,.FALSE.,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c#endif /* ndef DISABLE_SIGMA_CODE */
#endif /* NONLIN_FRSURF */

      RETURN
      END
