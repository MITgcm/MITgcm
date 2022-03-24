#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: BIO_EXPORT

C !INTERFACE: ==========================================================
      SUBROUTINE BIO_EXPORT( PTR_PO4 ,
#ifdef ALLOW_FE
     I           PTR_FE,
#endif
     O           bioac,
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           myIter,myTime,myThid)

c !DESCRIPTION:
C  Calculate biological activity and export

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#ifdef USE_QSW
#include "FFIELDS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  PTR_PO4              :: phosphate tracer field
C  PTR_FE               :: iron tracer field
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_FE
      _RL  PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER iMin, iMax, jMin, jMax, bi, bj

C !OUTPUT PARAMETERS: ==================================================
C  bioac               :: biological productivity (will be split
C                         between export and dissolved pool)
      _RL bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#if (defined ALLOW_PTRACERS && defined DIC_BIOTIC)

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
       INTEGER i,j,k
      _RL sfac    (1-OLy:sNy+OLy)
      _RL lit     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kall, atten
      _RL nutlimit
      _RL tmppo4
#ifdef ALLOW_FE
      _RL tmpfe
#endif
#ifdef AD_SAFE
      _RL thx, thy, thaux
#endif
CEOP

#ifndef READ_PAR
#ifndef USE_QSW
      CALL INSOL(myTime,sfac,bi,bj,myThid)
#endif
#endif
cph: following init assumes nlev=nr
cph: set in dic_init_fixed.F (ALLOW_AUTODIFF)
C$TAF INIT bio_export = static, nsx*nsy*nr
C FORTRAN-77 dynamic memory uses adstore adresto
CRG C$TAF INIT bio_export = memory
      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef READ_PAR
        lit(i,j)=PAR(i,j,bi,bj)
#elif (defined USE_QSW)
        lit(i,j)=-parfrac*Qsw(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#else
        lit(i,j)=sfac(j)
#endif
        IF ( .NOT. QSW_underice ) THEN
c if using Qsw but not seaice/thsice or coupled, then
c ice fraction needs to be taken into account
         lit(i,j)=lit(i,j)*(1. _d 0 - FIce(i,j,bi,bj))
        ENDIF
       ENDDO
      ENDDO

      kall = k0
      DO k=1,nlev
C$TAF STORE lit = bio_export
       DO j=jMin,jMax
        DO i=iMin,iMax
#ifdef LIGHT_CHL
c   Add self-shading effects to light attenuation coefficient
         kall = k0+kchl*CHL(i,j,bi,bj)
#endif
         atten = kall*drF(k)*hFacC(i,j,k,bi,bj)*.5 _d 0
         if (k.gt.1) atten = atten
     &         +( kall*drF(k-1)*hFacC(i,j,k-1,bi,bj)*.5 _d 0 )
         lit(i,j)=lit(i,j)*exp(-atten)
#ifndef TARGET_NEC_SX
C     this statement breaks vectorization and causes a dramatic
C     performance drop on vector computers
          IF (lit(i,j).LT.0. _d 0.OR.lit(i,j).GT.350. _d 0) THEN
           print*,'QQ lit',i,j,lit(i,j)
          ENDIF
#endif
#ifdef DIC_NO_NEG
         tmppo4=max(0. _d 0, PTR_PO4(i,j,k))
         lit(i,j)=max(0. _d 0,lit(i,j))
#else
         tmppo4=PTR_PO4(i,j,k)
#endif

#ifdef ALLOW_FE
#ifdef DIC_NO_NEG
         tmpfe=max(0. _d 0,PTR_FE(i,j,k))
#else
         tmpfe=PTR_FE(i,j,k)
#endif
#ifdef AD_SAFE
         thx = tmppo4/(tmppo4+KPO4)
         thy = tmpfe/(tmpfe+KFE)
c        thx = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4)
c        thy = PTR_FE(i,j,k)/(PTR_FE(i,j,k)+KFE)
         thaux = tanh( (thx-thy)*1. _d 6 )
         nutlimit= ( 1. _d 0 - thaux ) * thx * 0.5 _d 0
     &        +    ( 1. _d 0 + thaux ) * thy * 0.5 _d 0
#else
         nutlimit=min( tmppo4/(tmppo4+KPO4),tmpfe/(tmpfe+KFE) )
#endif
#else
         nutlimit=     tmppo4/(tmppo4+KPO4)
#endif

         bioac(i,j,k)=alpha(i,j,bi,bj)*
     &        lit(i,j)/(lit(i,j)+lit0)*maskC(i,j,k,bi,bj)*
     &        nutlimit
        ENDDO
       ENDDO
      ENDDO
c
#endif /* definded ALLOW_PTRACERS && defined DIC_BIOTIC */
      RETURN
      END
