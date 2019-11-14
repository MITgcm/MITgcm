#include "SHELFICE_OPTIONS.h"
#define SHELFICE_REMESH_PRINT

CBOP
C     !ROUTINE: SHELFICE_REMESH_C_MASK
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_C_MASK(
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_C_MASK
C     | o Loops through top level cells and determines those where
C     |   hFac is too large and hence splits into two cells,
C     |   and cells where hFac is too small, and merges cell with
C     |   below.
C     | o Affects all dynamic arrays, as well as R_shelfice
C     | o Recomputes hFacC using formula from initialisation
C     |   based on new R_shelfice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef NONLIN_FRSURF
#ifdef ALLOW_SHELFICE_REMESHING
C     !LOCAL VARIABLES:
C     bi,bj   :: tile indices
C     i,j,k   :: Loop counters
C     ioUnit  :: temp for writing msg unit
C     msgBuf  :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, k
#ifdef SHELFICE_REMESH_PRINT
      LOGICAL prtFirst
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      _RL sync_fac, stag_fac
      _RL adjust, eta_old, r_newDz
      _RL tmpVar, mergeFac1,  mergeFac2
      _RS hFacS_old(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS hFacW_old(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)

#ifdef SHELFICE_REMESH_PRINT
      prtFirst = .TRUE.
      ioUnit = -1
      IF ( debugLevel.GE.debLevB ) ioUnit = standardMessageUnit
#endif
      IF ( staggerTimestep ) THEN
        sync_fac = 0.0
        stag_fac = 1.0
      ELSE
        sync_fac = 1.0
        stag_fac = 0.0
      ENDIF

C-- Update etaN
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx

C-- SPLIT CELLS
          IF ( R_shelfice(i,j,bi,bj) .LT. 0.0) THEN
           k  = MAX(1,kTopC(i,j,bi,bj))
           IF ( k.GT.1 .AND.
     &          hFac_surfC(i,j,bi,bj).GT.SHELFICEsplitThreshold ) THEN
            IF ( (hFac_surfC(i,j,bi,bj)-1)*drF(k)*recip_drF(k-1)
     &           .GT. SHELFICEmergeThreshold ) THEN

             adjust = rF(k-1) - R_shelfice(i,j,bi,bj)
             eta_old = etaH(i,j,bi,bj)
#ifdef SHELFICE_REMESH_PRINT
             IF ( ioUnit.GE.0 ) THEN
              IF ( prtFirst ) THEN
               WRITE(msgBuf,'(A,I10)') 'SHI_REMESH at it=', myIter
               CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
               prtFirst = .FALSE.
              ENDIF
              WRITE(msgBuf,'(A,2I5,2I4,A,1P2E12.3)') '--> REMESH in:',
     &            i, j, bi, bj, ' , x,y=', XC(i,j,bi,bj),YC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' before:',
     &        '  ks=', kSurfC(i,j,bi,bj), ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=', hFac_surfC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif
C-  decrement eta and increment R_shelfice and Ro_surf
             etaN(i,j,bi,bj) = etaN(i,j,bi,bj)- adjust
             etaH(i,j,bi,bj) = etaH(i,j,bi,bj)- adjust
             etaHnm1(i,j,bi,bj) = etaHnm1(i,j,bi,bj)- adjust
             R_shelfIce(i,j,bi,bj) = R_shelfIce(i,j,bi,bj)+adjust
             Ro_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)+adjust
C-  initialise new cell-centered variables from values below
             salt(i,j,k-1,bi,bj)=salt(i,j,k,bi,bj)
             theta(i,j,k-1,bi,bj)=theta(i,j,k,bi,bj)

C     also need to increment geometric factors and masks
C     maskC, hFacC, hFac_surfC, kSurfC, kTopC, recip_hFacC
             IF ( k.LT.kLowC(i,j,bi,bj) ) THEN
               h0FacC(i,j,k,bi,bj) = 1.
             ELSE
               h0FacC(i,j,k,bi,bj) = (rF(k)-r_low(i,j,bi,bj)) *
     &          recip_drF(k)
             ENDIF
             h0FacC(i,j,k-1,bi,bj) = 1.
#ifdef SHELFICE_REMESH_PRINT
             IF ( ioUnit.GE.0 ) THEN
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' after :',
     &        '  ks=', k-1, ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=',
     &           h0FacC(i,j,k-1,bi,bj)+etaH(i,j,bi,bj)*recip_drF(k-1)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif
            ENDIF
           ENDIF
          ENDIF

C-- MERGE CELLS
          IF ( kTopC(i,j,bi,bj).LT.kLowC (i,j,bi,bj) ) THEN
           k = MAX(1,kTopC(i,j,bi,bj))
           IF ( hFac_surfC(i,j,bi,bj) .LT.
     &          SHELFICEmergeThreshold ) THEN
            IF ( (hFac_surfC(i,j,bi,bj)*drF(k)*recip_drF(k+1)+1) .LT.
     &          SHELFICEsplitThreshold ) THEN

             adjust = R_shelfice(i,j,bi,bj)-rF(k+1)
             eta_old = etaH(i,j,bi,bj)
#ifdef SHELFICE_REMESH_PRINT
             IF ( ioUnit.GE.0 ) THEN
              IF ( prtFirst ) THEN
               WRITE(msgBuf,'(A,I10)') 'SHI_REMESH at it=', myIter
               CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
               prtFirst = .FALSE.
              ENDIF
              WRITE(msgBuf,'(A,2I5,2I4,A,1P2E12.3)') '--> REMESH in:',
     &            i, j, bi, bj, ' , x,y=', XC(i,j,bi,bj),YC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' before:',
     &        '  ks=', kSurfC(i,j,bi,bj), ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=', hFac_surfC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif
C-  increment eta and decrement R_shelfice and Ro_sur
             etaN(i,j,bi,bj) = etaN(i,j,bi,bj) + adjust
             etaH(i,j,bi,bj) = etaH(i,j,bi,bj) + adjust
             etaHnm1(i,j,bi,bj) = etaHnm1(i,j,bi,bj) + adjust
             R_shelfice(i,j,bi,bj) = R_shelfice(i,j,bi,bj)-adjust
             Ro_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - adjust

C-  first find the volume of new cell -- taking bathy into account
             IF ( k+1.LT.kLowC(i,j,bi,bj) ) THEN
              r_newDz = oneRL / ( drF(k+1) + etaH(i,j,bi,bj) )
             ELSE
              r_newDz = oneRL
     &                / ( rF(k+1)-R_low(i,j,bi,bj) + etaH(i,j,bi,bj) )
             ENDIF
             mergeFac1 = ( stag_Fac*hFac_surfC(i,j,bi,bj)
     &                   + sync_fac*hFacC(i,j,k,bi,bj)
     &                   )*drF(k)*r_newDz
             mergeFac2 = hFacC(i,j,k+1,bi,bj)*drF(k+1)*r_newDz
             theta(i,j,k+1,bi,bj) = theta(i,j, k ,bi,bj)*mergeFac1
     &                            + theta(i,j,k+1,bi,bj)*mergeFac2
             theta(i,j,k,bi,bj) = 0.0
             salt(i,j,k+1,bi,bj) = salt(i,j, k ,bi,bj)*mergeFac1
     &                           + salt(i,j,k+1,bi,bj)*mergeFac2
             salt(i,j,k,bi,bj) = 0.0
C     also need to increment geometric factors and masks
C     maskC, hFacC, hFac_surfC, kSurfC, kTopC, recip_hFacC
             h0FacC(i,j,k,bi,bj) = 0.
             IF ( k+1.LT.kLowC(i,j,bi,bj) ) THEN
              h0FacC(i,j,k+1,bi,bj) = (drF(k+1) + 0*eta_old) *
     &          recip_drF(k+1)
             ELSE
               h0FacC(i,j,k+1,bi,bj) = (rF(k+1)-r_low(i,j,bi,bj)+
     &          0*eta_old) * recip_drF(k+1)
             ENDIF
#ifdef SHELFICE_REMESH_PRINT
             IF ( ioUnit.GE.0 ) THEN
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' after :',
     &        '  ks=', k+1, ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=',
     &           h0FacC(i,j,k+1,bi,bj)+etaH(i,j,bi,bj)*recip_drF(k+1)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif
            ENDIF
           ENDIF
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef SHELFICE_REMESH_PRINT
      IF ( ioUnit.GE.0 .AND. .NOT. prtFirst ) THEN
        WRITE(msgBuf,'(A,A)') 'SHI_REMESH :', ' end of report'
        CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
      ENDIF
#endif

      CALL EXCH_XYZ_RS( h0FacC, myThid )

      DO k=1,Nr
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           hFacC(i,j,k,bi,bj) = h0FacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-    Calculate quantities derived from XY depth map
        DO j=1,sNy
         DO i=1,sNx
C         Total fluid column thickness (r_unit) :
           tmpVar = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
C          Inverse of fluid column thickness (1/r_unit)
           IF ( tmpVar .LE. zeroRL ) THEN
            recip_Rcol(i,j,bi,bj) = 0.
           ELSE
            recip_Rcol(i,j,bi,bj) = 1. _d 0 / tmpVar
           ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C- fill in the overlap (+ BARRIER):
      CALL EXCH_XYZ_RL( theta, myThid )
      CALL EXCH_XYZ_RL( salt,  myThid )
      CALL EXCH_XYZ_RS( maskC, myThid )
      CALL EXCH_XY_RL( EtaHnm1, myThid )
      CALL EXCH_XY_RL( EtaN, myThid )
      CALL EXCH_XY_RL( EtaH, myThid )
      CALL EXCH_XY_RS( Ro_Surf, myThid )
      CALL EXCH_XY_RS( R_shelfIce, myThid )
      CALL EXCH_XY_RS( recip_Rcol, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           rSurfW(i,j,bi,bj) =
     &           MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfW(i,j,bi,bj) =
     &           MAX( rSurfW(i,j,bi,bj), rLowW(i,j,bi,bj) )
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
           rSurfS(i,j,bi,bj) =
     &           MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfS(i,j,bi,bj) =
     &           MAX( rSurfS(i,j,bi,bj), rLowS(i,j,bi,bj) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kSurfC(i,j,bi,bj) = Nr+1
         ENDDO
        ENDDO
        DO k=Nr,1,-1
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( hFacC(i,j,k,bi,bj).NE.zeroRS ) kSurfC(i,j,bi,bj) = k
          ENDDO
         ENDDO
        ENDDO
C- DNG do not need to update these for vertical remeshing
C  DNG but will need to update for horizontal remeshing
c       DO j=1-OLy,sNy+OLy
c        DO i=1-OLx,sNx+OLx
c          maskInC(i,j,bi,bj) = 0.
c          IF ( kSurfC(i,j,bi,bj).LE.Nr ) maskInC(i,j,bi,bj) = 1.
c        ENDDO
c       ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &         R_shelfIce(i,j,bi,bj).LT.zeroRS ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
          ELSE
            kTopC(i,j,bi,bj) = 0
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           hFacW_old(i,j,k,bi,bj) = hFacW(i,j,k,bi,bj)
           hFacS_old(i,j,k,bi,bj) = hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL SHELFICE_REMESH_UVMASKS (myThid)
#ifdef ALLOW_OBCS
      IF (useobcs) THEN
C- DNG call will be needed for horizontal remeshing
c       CALL OBCS_UPDATE_REMESH( myThid )
      ENDIF
#endif
      CALL CALC_SURF_DR( etaHnm1, myTime, myIter, myThid )
      CALL UPDATE_SURF_DR(.TRUE., myTime, myIter, myThid)
      CALL CALC_SURF_DR( etaH, myTime, myIter, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx

            IF ( maskW(i,j,k,bi,bj).EQ.zeroRS .AND.
     &           hFacW_old(i,j,k,bi,bj).NE.zeroRS ) THEN
             r_newDz = recip_drF(k+1)/hFacW(i,j,k+1,bi,bj)
             mergeFac1 = hFacW_old(i,j, k ,bi,bj)*drF( k )*r_newDz
             mergeFac2 = hFacW_old(i,j,k+1,bi,bj)*drF(k+1)*r_newDz
             uVel(i,j,k+1,bi,bj) = uVel(i,j, k ,bi,bj)*mergeFac1
     &                           + uVel(i,j,k+1,bi,bj)*mergeFac2
             uVel(i,j,k,bi,bj) = 0.0
#ifdef ALLOW_ADAMSBASHFORTH_3
#else
             guNm1(i,j,k+1,bi,bj) = guNm1(i,j, k ,bi,bj)*mergeFac1
     &                            + guNm1(i,j,k+1,bi,bj)*mergeFac2
             guNm1(i,j,k,bi,bj) = 0.0
#endif
            ENDIF

            IF ( maskS(i,j,k,bi,bj).EQ.zeroRS .AND.
     &           hFacS_old(i,j,k,bi,bj).NE.zeroRS ) THEN
             r_newDz = recip_drF(k+1)/hFacS(i,j,k+1,bi,bj)
             mergeFac1 = hFacS_old(i,j, k ,bi,bj)*drF( k )*r_newDz
             mergeFac2 = hFacS_old(i,j,k+1,bi,bj)*drF(k+1)*r_newDz
             vVel(i,j,k+1,bi,bj) = vVel(i,j, k ,bi,bj)*mergeFac1
     &                           + vVel(i,j,k+1,bi,bj)*mergeFac2
             vVel(i,j,k,bi,bj) = 0.0
#ifdef ALLOW_ADAMSBASHFORTH_3
#else
             gvNm1(i,j,k+1,bi,bj) = gvNm1(i,j, k ,bi,bj)*mergeFac1
     &                            + gvNm1(i,j,k+1,bi,bj)*mergeFac2
             gvNm1(i,j,k,bi,bj) = 0.0
#endif
            ENDIF

          ENDDO
         ENDDO
        ENDDO

        DO k=1,Nr-1
         DO j=1,sNy
          DO i=1,sNx
            IF ( maskW(i,j,k,bi,bj).EQ.oneRS .AND.
     &           hFacW_old(i,j,k,bi,bj).EQ.zeroRS ) THEN
             uVel(i,j,k,bi,bj) = uVel(i,j,k+1,bi,bj)
#ifdef ALLOW_ADAMSBASHFORTH_3
#else
             guNm1(i,j,k,bi,bj) = guNm1(i,j,k+1,bi,bj)
#endif
            ENDIF
            IF ( maskS(i,j,k,bi,bj).EQ.oneRS .AND.
     &           hFacS_old(i,j,k,bi,bj).EQ.zeroRS ) THEN
             vVel(i,j,k,bi,bj) = vVel(i,j,k+1,bi,bj)
#ifdef ALLOW_ADAMSBASHFORTH_3
#else
             gvNm1(i,j,k,bi,bj) = gvNm1(i,j,k+1,bi,bj)
#endif
            ENDIF
          ENDDO
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      CALL EXCH_XYZ_RS( hFacC, myThid )
      CALL EXCH_UV_XYZ_RS( hFacW, hFacS, .FALSE., myThid )
      CALL EXCH_UV_XYZ_RS( maskW, maskS, .FALSE., myThid )
      CALL EXCH_UV_XYZ_RL( uVel, vVel, .TRUE., myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
      CALL EXCH_UV_XYZ_RL( guNm(1-OLx,1-OLy,1,1,1,1),
     &                     gvNm(1-OLx,1-OLy,1,1,1,1),.TRUE., myThid )
      CALL EXCH_UV_XYZ_RL( guNm(1-OLx,1-OLy,1,1,1,2),
     &                     gvNm(1-OLx,1-OLy,1,1,1,2),.TRUE., myThid )
#else
      CALL EXCH_UV_XYZ_RL( guNm1, gvNm1, .TRUE., myThid )
#endif

#endif /* ALLOW_SHELFICE_REMESHING */
#endif /* NONLIN_FRSURF */
      RETURN
      END
