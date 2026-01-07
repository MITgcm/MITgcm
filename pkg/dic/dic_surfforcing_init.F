#include "DIC_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: DIC_SURFFORCING_INIT

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_SURFFORCING_INIT(
     I          myThid )

C !DESCRIPTION:
C  Calculate first guess of pH

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "GCHEM_VARS.h"
#include "DIC_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER  myThid

#ifdef ALLOW_DIC

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j, kLev, it
      INTEGER iMin,iMax,jMin,jMax, bi, bj
       _RL co3dummy
C local variables for carbon chem
      _RL surfalk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfphos(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsi(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surftemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfdic(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iprt,jprt
      LOGICAL debugPrt
#ifdef ALLOW_DEBUG
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
CEOP

C =================================================================

      kLev = 1
      jMin=1
      jMax=sNy
      iMin=1
      iMax=sNx

C Solubility relation coefficients
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ak0(i,j,bi,bj)=0. _d 0
          ak1(i,j,bi,bj)=0. _d 0
          ak2(i,j,bi,bj)=0. _d 0
          akw(i,j,bi,bj)=0. _d 0
          akb(i,j,bi,bj)=0. _d 0
          akf(i,j,bi,bj)=0. _d 0
          ak1p(i,j,bi,bj)=0. _d 0
          ak2p(i,j,bi,bj)=0. _d 0
          ak3p(i,j,bi,bj)=0. _d 0
          aksi(i,j,bi,bj)=0. _d 0
          fugf(i,j,bi,bj)=0. _d 0
          ff(i,j,bi,bj)=0. _d 0
          ft(i,j,bi,bj)=0. _d 0
          st(i,j,bi,bj)=0. _d 0
          bt(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C determine inorganic carbon chem coefficients
        DO j=jMin,jMax
         DO i=iMin,iMax
#ifdef DIC_BIOTIC
#ifdef DIC_BOUNDS
            surfalk(i,j)  = MAX( 0.4 _d 0,
     &                      MIN( 10. _d 0, PTRACER(i,j,kLev,bi,bj,2) ) )
     &                    * maskC(i,j,kLev,bi,bj)
            surfphos(i,j) = MAX( 1.0 _d -11,
     &                      MIN( 1. _d -1, PTRACER(i,j,kLev,bi,bj,3) ) )
     &                    * maskC(i,j,kLev,bi,bj)
#else
            surfalk(i,j)  = PTRACER(i,j,kLev,bi,bj,2)
     &                    * maskC(i,j,kLev,bi,bj)
            surfphos(i,j) = PTRACER(i,j,kLev,bi,bj,3)
     &                    * maskC(i,j,kLev,bi,bj)
#endif
#else /* DIC_BIOTIC */
            surfalk(i,j)  = 2.366595 _d 0 *salt(i,j,kLev,bi,bj)/35. _d 0
     &                    * maskC(i,j,kLev,bi,bj)
            surfphos(i,j) = 5.1225 _d -4 * maskC(i,j,kLev,bi,bj)
#endif /* DIC_BIOTIC */
C for non-interactive Si
            surfsi(i,j)   = silicaSurf(i,j,bi,bj)*maskC(i,j,kLev,bi,bj)
#ifdef DIC_BOUNDS
            surftemp(i,j) = MAX( -4. _d 0,
     &                      MIN( 50. _d 0, theta(i,j,kLev,bi,bj) ) )
            surfsalt(i,j) = MAX( 4. _d 0,
     &                      MIN( 50. _d 0, salt(i,j,kLev,bi,bj) ) )
            surfdic(i,j)  = MAX( 0.4 _d 0,
     &                      MIN( 10. _d 0, PTRACER(i,j,kLev,bi,bj,1) ) )
#else
            surftemp(i,j) = theta(i,j,kLev,bi,bj)
            surfsalt(i,j) = salt(i,j,kLev,bi,bj)
            surfdic(i,j)  = PTRACER(i,j,kLev,bi,bj,1)
     &                    * maskC(i,j,kLev,bi,bj)
#endif
         ENDDO
        ENDDO

#ifndef ALLOW_AUTODIFF
        IF ( selectModernCoeffs.GT.0 ) THEN
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('GCHEM_COEFFS_SURF',myThid)
#endif
         CALL GCHEM_COEFFS_SURF(
     I                       surftemp,surfsalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)
        ELSE
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('GCHEM_CARBON_COEFFS',myThid)
#endif
         CALL GCHEM_CARBON_COEFFS(
     I                       surftemp,surfsalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,0,myThid)
#ifndef ALLOW_AUTODIFF
        ENDIF
#endif

C====================================================================

        IF ( .NOT.pH_isLoaded(1) ) THEN
C set guess of 2-d surface pH for first step here
#ifdef ALLOW_DEBUG
          IF (debugMode) THEN
            WRITE(msgBuf,'(A)') 'Initial pCO2 approximation method'
            CALL DEBUG_MSG(msgBuf(1:33),myThid)
          ENDIF
#endif

          debugPrt = debugMode
C first approximation
C$TAF LOOP = parallel
          DO j=jMin,jMax
C$TAF LOOP = parallel
           DO i=iMin,iMax
            IF ( maskC(i,j,kLev,bi,bj) .NE. 0. _d 0) THEN
C$TAF init dic_surf = static, 10
#ifndef ALLOW_AUTODIFF
             IF ( selectPHsolver.GT.0 ) THEN
C Use Munhoven (2013) Solvesaphe routine to initialize pH
#ifdef ALLOW_DEBUG
              IF (debugPrt) CALL DEBUG_CALL('GCHEM_AHINI_FOR_AT',myThid)
#endif
C call GCHEM_AHINI_FOR_AT to get better initial guess of pH
              CALL GCHEM_AHINI_FOR_AT(
     I           surfalk(i,j)*permil,
     I           surfdic(i,j)*permil,
     I           bt(i,j,bi,bj),
     U           pH(i,j,bi,bj),
     I           i,j,kLev,bi,bj,nIter0,myThid )

C$TAF STORE pH(i,j,bi,bj)                              = dic_surf
C$TAF STORE surfalk(i,j), surfphos(i,j), surfsi(i,j)   = dic_surf

#ifdef ALLOW_DEBUG
              IF (debugPrt)
     &          CALL DEBUG_CALL('GCHEM_CALC_PCO2_SOLVESAPHE',myThid)
#endif
              CALL GCHEM_CALC_PCO2_SOLVESAPHE(
     I         surftemp(i,j),surfsalt(i,j),
     I         surfdic(i,j), surfphos(i,j),
     I         surfsi(i,j),surfalk(i,j), permil,
     U         pH(i,j,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I         i,j,kLev,bi,bj, debugPrt, nIter0, myThid )
              debugPrt = .FALSE.
             ELSE
#endif /* ALLOW_AUTODIFF */
C Use the original Follows et al. (2006) solver
#ifdef ALLOW_DEBUG
              IF (debugPrt) THEN
                CALL DEBUG_CALL('GCHEM_CALC_PCO2_APPROX',myThid)
                debugPrt = .FALSE.
              ENDIF
#endif
              DO it=1,10
cC$TAF STORE pH(i,j,bi,bj)                              = dic_surf
cC$TAF STORE surfalk(i,j), surfphos(i,j), surfsi(i,j)   = dic_surf
               CALL GCHEM_CALC_PCO2_APPROX(
     I          surftemp(i,j),surfsalt(i,j),
     I          surfdic(i,j), surfphos(i,j),
     I          surfsi(i,j),surfalk(i,j),
     I          ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I          ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I          aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I          aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I          ak0(i,j,bi,bj), fugf(i,j,bi,bj),
     I          ff(i,j,bi,bj),
     I          bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     I          permil,
     U          pH(i,j,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I          i,j,kLev,bi,bj, it ,  myThid )
              ENDDO
#ifndef ALLOW_AUTODIFF
             ENDIF
#endif
            ENDIF
           ENDDO
          ENDDO

#ifdef ALLOW_DEBUG
         IF (debugMode) THEN
          iprt = MIN(20,sNx)
          jprt = MIN(20,sNy)
          WRITE(msgBuf,'(4(A,F9.6),2(A,F11.8),A,F9.6)')
     &        ' first guess pH=', pH(iprt,jprt,bi,bj),
     &        ', Temp=',theta(iprt,jprt,1,bi,bj),
     &        ', Salt=',salt(iprt,jprt,1,bi,bj),
     &        ', DIC=', surfdic(iprt,jprt),
     &        ', PO4=', surfphos(iprt,jprt),
     &        ', SiT=', surfsi(iprt,jprt),
     &        ', ALK=', surfalk(iprt,jprt)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
c         WRITE(standardMessageUnit,*) 'QQ first guess pH ',
c    &        pH(iprt,jprt,bi,bj),' Temp ',theta(iprt,jprt,1,bi,bj),
c    &        ' Salt ',salt(iprt,jprt,1,bi,bj),
c    &        ' DIC ' ,surfdic(iprt,jprt),
c    &        ' PO4 ' ,surfphos(iprt,jprt),
c    &        ' SiT ' ,surfsi(iprt,jprt),
c    &        ' ALK ' ,surfalk(iprt,jprt)
c         CALL DEBUG_MSG(msgBuf,myThid)
         ENDIF
#endif

C end if-block (.NOT.pH_isLoaded)
        ENDIF
C     end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_DIC */
      RETURN
      END
