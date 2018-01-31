#include "CHEAPAML_OPTIONS.h"
#undef CHEAPAML_OLD_MASK_SETTING

CBOP
C     !ROUTINE: CHEAPAML_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE CHEAPAML_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_INIT_FIXED
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CHEAPAML.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     bi,bj  :: tile indices
C     i,j    :: grid-point indices
C     msgBuf :: Informational/error message buffer
C     relaxMask :: relaxation mask [no units]
C     xgs       :: relaxation coefficient [units: 1/s]
      INTEGER bi, bj
      INTEGER i, j
      INTEGER iG,jG
      INTEGER xmw
      _RL xmf, tmpVar
      _RL relaxMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL xgs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iL, ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef CHEAPAML_OLD_MASK_SETTING
      _RL recipMW
      _RL cheapaml_taurelax, cheapaml_taurelaxocean
#endif /* CHEAPAML_OLD_MASK_SETTING */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ioUnit = standardMessageUnit

C--   Initialise CheapAML local & fixed variables
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          relaxMask(i,j,bi,bj) = 0. _d 0
          xgs      (i,j,bi,bj) = 0. _d 0
          xrelf    (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef CHEAPAML_OLD_MASK_SETTING
      cheapaml_taurelax      =  cheap_tauRelax   /86400. _d 0
      cheapaml_taurelaxocean =  cheap_tauRelaxOce/86400. _d 0

c--   Setup CheapAML mask (for relaxation):
C Do  mask
      IF ( cheapMaskFile .NE. ' ') THEN
         iL = ILNBLNK(cheapMaskFile)
         WRITE(msgBuf,'(4A)') 'CHEAPAML_INIT_VARIA: ',
     &      'Relaxation Mask read from ->', cheapMaskFile(1:iL), '<-'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL READ_FLD_XY_RL( cheapMaskFile,' ',relaxMask,0,myThid )
      ELSE
         WRITE(msgBuf,'(4A)') 'CHEAPAML_INIT_VARIA: ',
     &      'Generate Cheapaml mask'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         xmw = Cheapaml_mask_width
         recipMW = ( xmw - 1 )
         IF ( xmw.NE.1 ) recipMW = 1. _d 0 / recipMW
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             xmf = 0. _d 0
             iG=myXGlobalLo-1+(bi-1)*sNx+i
             jG = myYGlobalLo-1+(bj-1)*sNy+j
             IF (jG.GT.xmw) THEN
               IF (jG.LT.Ny-xmw+1) THEN
                 IF (iG.LE.xmw)      xmf = 1. _d 0 - (iG-1 )*recipMW
                 IF (iG.GE.Nx-xmw+1) xmf = 1. _d 0 - (Nx-iG)*recipMW
               ELSE
                 xmf = 1. _d 0 - (Ny-jG)*recipMW
                 IF (iG.LE.xmw) THEN
                   xmf =  1. _d 0 - (iG-1 )*recipMW *(Ny-jG)*recipMW
                 ELSEIF (iG.GE.Nx-xmw+1) THEN
                   xmf =  1. _d 0 - (Nx-iG)*recipMW *(Ny-jG)*recipMW
                 ENDIF
               ENDIF
             ELSE
               xmf = 1. _d 0 - (jG-1)*recipMW
               IF (iG.LE.xmw) THEN
                 xmf = 1. _d 0 - (iG-1 )*recipMW*(jG-1)*recipMW
               ELSEIF (iG.GE.Nx-xmw+1) THEN
                 xmf = 1. _d 0 - (Nx-iG)*recipMW*(jG-1)*recipMW
               ENDIF
             ENDIF
             relaxMask(i,j,bi,bj) = xmf*cheapaml_taurelax
            ENDDO
           ENDDO
          ENDDO
         ENDDO
      ENDIF

C     relaxation forced on land
       DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
             DO i=1,sNx
               IF( maskC(i,j,1,bi,bj).EQ.0. _d 0) THEN
                 relaxMask(i,j,bi,bj)=cheapaml_taurelax
C     relaxation over the ocean
               ELSEIF( relaxMask(i,j,bi,bj).EQ.0. _d 0) THEN
                 relaxMask(i,j,bi,bj)=cheapaml_taurelaxocean
               ENDIF
             ENDDO
           ENDDO
         ENDDO
       ENDDO
       _EXCH_XY_RL( relaxMask, myThid )

C relaxation time scales from input
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF (relaxMask(i,j,bi,bj).NE.0.) THEN
            xgs(i,j,bi,bj)=1. _d 0/relaxMask(i,j,bi,bj)/8.64 _d 4
           ELSE
            xgs(i,j,bi,bj)=0. _d 0
           ENDIF
           xrelf(i,j,bi,bj)= xgs(i,j,bi,bj)*deltaT
     &                     /(1. _d 0+xgs(i,j,bi,bj)*deltaT)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c      _EXCH_XY_RL( xgs, myThid )
c      _EXCH_XY_RL( xrelf, myThid )

#else /* CHEAPAML_OLD_MASK_SETTING */

C--   Setup CheapAML mask (for relaxation):
      IF ( cheapMaskFile .NE. ' ' ) THEN
C-    read mask from file
        iL = ILNBLNK(cheapMaskFile)
        WRITE(msgBuf,'(4A)') 'CHEAPAML_INIT_VARIA: ',
     &      'Relaxation Mask read from ->', cheapMaskFile(1:iL), '<-'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        CALL READ_FLD_XY_RL( cheapMaskFile,' ',relaxMask,0,myThid )
      ELSE
        WRITE(msgBuf,'(4A)') 'CHEAPAML_INIT_VARIA: ',
     &     'Generate Cheapaml mask'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C-    set mask according to boundaries
          IF ( Cheapaml_mask_width.LE.0 .OR.
     &         ( cheapamlXperiodic .AND. cheapamlYperiodic ) ) THEN
            DO j=1,sNy
             DO i=1,sNx
               relaxMask(i,j,bi,bj) = 0.
             ENDDO
            ENDDO
          ELSE
            xmw = Cheapaml_mask_width
            tmpVar = xmw
            tmpVar = oneRL / tmpVar
            DO j=1,sNy
             DO i=1,sNx
              xmf = 0. _d 0
              iG = myXGlobalLo-1+(bi-1)*sNx+i
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              IF ( .NOT.cheapamlXperiodic ) THEN
                IF (iG.LE.xmw)      xmf = oneRL - (iG-1 )*tmpVar
                IF (iG.GE.Nx-xmw+1) xmf = oneRL - (Nx-iG)*tmpVar
              ENDIF
              IF ( .NOT.cheapamlYperiodic ) THEN
                IF (jG.LE.xmw)
     &                    xmf = MAX( xmf, oneRL - (jG-1 )*tmpVar )
                IF (jG.GE.Ny-xmw+1)
     &                    xmf = MAX( xmf, oneRL - (Ny-jG)*tmpVar )
              ENDIF
              relaxMask(i,j,bi,bj) = xmf
             ENDDO
            ENDDO
          ENDIF
C-    set mask to one over land:
          DO j=1,sNy
            DO i=1,sNx
              relaxMask(i,j,bi,bj) = MAX( relaxMask(i,j,bi,bj),
     &                               (oneRL - maskC(i,j,1,bi,bj)) )
            ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
      _EXCH_XY_RL( relaxMask, myThid )

C-    Set relaxation coeff "xgs"
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( cheap_tauRelax .LE. zeroRL ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              xgs(i,j,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
         ELSE
           tmpVar =  oneRL/cheap_tauRelax
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              xgs(i,j,bi,bj) = relaxMask(i,j,bi,bj)*tmpVar
            ENDDO
           ENDDO
         ENDIF
         IF ( cheap_tauRelaxOce .GT. zeroRL
     &        .AND. cheapMaskFile .EQ. ' ' ) THEN
           tmpVar =  oneRL/cheap_tauRelaxOce
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              xgs(i,j,bi,bj) = MAX( xgs(i,j,bi,bj), tmpVar )
            ENDDO
           ENDDO
         ENDIF
C-    Calculate implicit relaxation factor "xrelf"
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            tmpVar = xgs(i,j,bi,bj)*deltaT
            xrelf(i,j,bi,bj)= tmpVar/( oneRL + tmpVar )
          ENDDO
         ENDDO
       ENDDO
      ENDDO
#endif /* CHEAPAML_OLD_MASK_SETTING */

      IF ( debugLevel.GE.debLevB .AND. nIter0.EQ.0 ) THEN
        CALL WRITE_FLD_XY_RL('CheapMask',  ' ', relaxMask, 0, myThid )
      ENDIF
      IF ( debugLevel.GE.debLevC .AND. nIter0.EQ.0 ) THEN
        CALL WRITE_FLD_XY_RL('Cheap_xgs',   ' ', xgs,   0, myThid )
        CALL WRITE_FLD_XY_RL('Cheap_xrelf', ' ', xrelf, 0, myThid )
      ENDIF

      _BEGIN_MASTER( myThid )

C-    Initialise AB starting level
      cheapTairStartAB = nIter0
      cheapQairStartAB = nIter0
      cheapTracStartAB = nIter0

      _END_MASTER( myThid )

C-    Everyone else must wait for parameters to be set
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
c     IF (useMNC) THEN
c     ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL CHEAPAML_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
