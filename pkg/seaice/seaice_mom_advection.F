#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SEAICE_MOM_ADVECTION

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_MOM_ADVECTION(
     I        bi,bj,iMin,iMax,jMin,jMax,
     I        uIceLoc, vIceLoc,
     O        gU, gV,
     I        myTime, myIter, myThid )
C     *==========================================================*
C     | S/R SEAICE_MOM_ADVECTION                                 |
C     | o Form the advection of sea ice momentum to be added to  |
C     |   the right hand-side of the momentum equation.          |
C     *==========================================================*
C     | Most of the code is take from S/R MOM_VECINV and reuses  |
C     | code from mom_vecinv and mom_common                      |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     == Routine arguments ==
C     bi,bj   :: current tile indices
C     iMin,iMax,jMin,jMax :: loop ranges
C     uIceLoc ::
C     vIceLoc ::

C     gU      :: advection tendency (all explicit terms), u component
C     gV      :: advection tendency (all explicit terms), v component
C     myTime  :: current time
C     myIter  :: current time-step number
C     myThid  :: my Thread Id number
      INTEGER bi,bj
      INTEGER iMin,iMax,jMin,jMax
      _RL uIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_ALLOW_MOM_ADVECTION

C     == Functions ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Local variables ==
      _RL      uCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     i,j    :: Loop counters
C     k      :: surface level index
      INTEGER i,j,k
C     later these will be run time parameters
CML      LOGICAL SEAICEhighOrderVorticity, SEAICEupwindVorticity
CML      LOGICAL SEAICEuseAbsVorticity,
      LOGICAL vorticityFlag
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER imomkey
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
      act0 = k - 1
      max0 = Nr
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      imomkey = (act0 + 1)
     &         + act1*max0
     &         + act2*max0*max1
     &         + act3*max0*max1*max2
     &         + act4*max0*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

CML      SEAICEselectKEscheme     = selectKEscheme
CML      SEAICEselectVortScheme   = selectVortScheme
CML      SEAICEhighOrderVorticity = highOrderVorticity
CML      SEAICEupwindVorticity    = upwindVorticity
CML      SEAICEuseAbsVorticity    = useAbsVorticity
CML      SEAICEuseJamartMomAdv    = useJamartMomAdv

C--   Initialise intermediate terms
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uCf(i,j)   = 0.
        vCf(i,j)   = 0.
        gU(i,j)    = 0.
        gV(i,j)    = 0.
        vort3(i,j) = 0.
        KE(i,j)    = 0.
#ifdef ALLOW_AUTODIFF
        hFacZ(i,j)   = 0. _d 0
#endif
       ENDDO
      ENDDO

      k = 1
C--   Calculate open water fraction at vorticity points
      CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

C     Make local copies of horizontal flow field
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uFld(i,j) = uIceLoc(i,j,bi,bj)
        vFld(i,j) = vIceLoc(i,j,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ufld(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
CADJ STORE vfld(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
CADJ STORE hFacZ(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
CADJ STORE r_hFacZ(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
#endif

      CALL MOM_CALC_KE(bi,bj,k,SEAICEselectKEscheme,uFld,vFld,KE,myThid)

      CALL MOM_CALC_RELVORT3(bi,bj,k,uFld,vFld,hFacZ,vort3,myThid)

CMLC-    calculate absolute vorticity
CML      IF (useAbsVorticity) THEN
CML       DO j=1-Oly,sNy+Oly
CML        DO i=1-Olx,sNx+Olx
CML         vort3(i,j) = vort3(i,j) + fCoriG(i,j,bi,bj)
CML        ENDDO
CML       ENDDO
CML      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ke(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
CADJ STORE vort3(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ucf(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
CADJ STORE vcf(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
#endif

C--   Horizontal advection of relative (or absolute) vorticity
      vorticityFlag = SEAICEhighOrderVorticity.OR.SEAICEupwindVorticity
      IF ( vorticityFlag ) THEN
       CALL MOM_VI_U_CORIOLIS_C4(bi,bj,k,SEAICEselectVortScheme,
     &                           SEAICEhighOrderVorticity,
     &                           SEAICEupwindVorticity,
     &                           vFld,vort3,r_hFacZ,
     &                           uCf,myThid)
      ELSE
       CALL MOM_VI_U_CORIOLIS(bi,bj,k,SEAICEselectVortScheme,
     &                        SEAICEuseJamartMomAdv,
     &                        vFld,vort3,hFacZ,r_hFacZ,
     &                        uCf,myThid)
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j) = gU(i,j)+uCf(i,j)
       ENDDO
      ENDDO
      IF ( vorticityFlag ) THEN
       CALL MOM_VI_V_CORIOLIS_C4(bi,bj,k,SEAICEselectVortScheme,
     &                           SEAICEhighOrderVorticity,
     &                           SEAICEupwindVorticity,
     &                           uFld,vort3,r_hFacZ,
     &                           vCf,myThid)
      ELSE
       CALL MOM_VI_V_CORIOLIS(bi,bj,k,SEAICEselectVortScheme,
     &                        SEAICEuseJamartMomAdv,
     &                        uFld,vort3,hFacZ,r_hFacZ,
     &                        vCf,myThid)
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j) = gV(i,j)+vCf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ucf(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
CADJ STORE vcf(:,:) =
CADJ &     comlev1_bibj_lsr, key = imomkey, byte = isbyte
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(uCf,'SIuAdvZ3',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(vCf,'SIvAdvZ3',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Bernoulli term
      CALL MOM_VI_U_GRAD_KE(bi,bj,k,KE,uCf,myThid)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j) = gU(i,j)+uCf(i,j)
       ENDDO
      ENDDO
      CALL MOM_VI_V_GRAD_KE(bi,bj,k,KE,vCf,myThid)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j) = gV(i,j)+vCf(i,j)
       ENDDO
      ENDDO
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(uCf,'SIKEx   ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(vCf,'SIKEy   ',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Set du/dt & dv/dt on boundaries to zero
C     apply masks for interior (important when we have open boundaries)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j) = gU(i,j)*maskInW(i,j,bi,bj)
        gV(i,j) = gV(i,j)*maskInS(i,j,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(KE,     'SImomKE ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(gU,     'SIuMmAdv',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(gV,     'SIvMmAdv',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* SEAICE_ALLOW_MOM_ADVECTION */

      RETURN
      END
