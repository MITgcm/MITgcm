C $Header: /u/gcmpack/MITgcm/model/src/ini_p_ground.F,v 1.2 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_P_GROUND
C     !INTERFACE:
      SUBROUTINE INI_P_GROUND( 
     I                        Hfld, 
     O                        Pfld, 
     I                        myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_P_GROUND                                    
C     | o Convert Topography [m] to (reference) Surface Pressure  
C     |   according to tRef profile,                              
C     |   using same discretisation as in calc_phi_hyd            
C     |                                                           
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     Hfld (input)  :: Ground elevation [m] 
C     Pfld (output) :: reference Pressure at the ground [Pa] 
      _RS Hfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS Pfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

c #ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,K, Ks
      _RL ddPI, Po_surf
      _RL phiRef(2*Nr+1), rHalf(2*Nr+1)
CEOP

      DO K=1,Nr
        rHalf(2*K-1) = rF(K)
        rHalf(2*K)   = rC(K)
      ENDDO
       rHalf(2*Nr+1) = rF(Nr+1)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C- Compute Reference Geopotential at Half levels :
C    Tracer level: phiRef(2K)  ;  Interface_W level: phiRef(2K+1)

      phiRef(1) = 0. 

      IF (Integr_GeoPot.EQ.1) THEN
C- Finite Volume Form, linear by half level :
        DO K=1,2*Nr
          Ks = (K+1)/2
          ddPI=atm_cp*( ((rHalf( K )/atm_po)**atm_kappa)
     &                 -((rHalf(K+1)/atm_po)**atm_kappa) ) 
          phiRef(K+1) = phiRef(K)+ddPI*tRef(Ks)
        ENDDO
C------
      ELSE
C- Finite Difference Form, linear between Tracer level :
C   works with Integr_GeoPot = 0, 2 or 3
        K = 1
          ddPI=atm_cp*( ((rF(K)/atm_po)**atm_kappa)
     &                 -((rC(K)/atm_po)**atm_kappa) ) 
          phiRef(2*K)   = phiRef(1) + ddPI*tRef(K) 
        DO K=1,Nr-1
          ddPI=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                 -((rC(K+1)/atm_po)**atm_kappa) ) 
          phiRef(2*K+1) = phiRef(2*K) + ddPI*0.5*tRef(K)
          phiRef(2*K+2) = phiRef(2*K)
     &                  + ddPI*0.5*(tRef(K)+tRef(K+1)) 
        ENDDO
        K = Nr
          ddPI=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                 -((rF(K+1)/atm_po)**atm_kappa) ) 
          phiRef(2*K+1) = phiRef(2*K) + ddPI*tRef(K)
C------
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- Convert phiRef to Z unit :
      DO K=1,2*Nr+1
        phiRef(K) = phiRef(K)*recip_gravity
      ENDDO

C- Write to check :
      write(6,'(2A)') 'INI_P_GROUND: PhiRef/g [m] at Center ',
     & '(integer) and Interface (half-int.) levels:'
      DO K=1,2*Nr+1
        write(6,'(A,F5.1,A,F10.1,A,F12.3)') '  K=', K*0.5, 
     &     '  ;  r=', rHalf(K), '  ;  phiRef/g=', phiRef(K)
      ENDDO

C- Find Po_surf : Linear between 2 half levels :
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           Ks = 1
           DO K=1,2*Nr
             IF (Hfld(i,j,bi,bj).GE.phiRef(K)) Ks = K 
           ENDDO
           Po_surf = rHalf(Ks) + (rHalf(Ks+1)-rHalf(Ks))*
     &       (Hfld(i,j,bi,bj)-phiRef(Ks))/(phiRef(Ks+1)-phiRef(Ks))

c          IF (Hfld(i,j,bi,bj).LT.phiRef(1)) Po_surf= rHalf(1)
c          IF (Hfld(i,j,bi,bj).GT.phiRef(2*Nr+1)) Po_surf= rHalf(2*Nr+1)
           Pfld(i,j,bi,bj) = Po_surf
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c #endif/* INCLUDE_PHIHYD_CALCULATION_CODE */ 

      RETURN
      END
