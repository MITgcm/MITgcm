C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_phiref.F,v 1.2 2005/12/05 22:13:57 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PHIREF
C     !INTERFACE:
      SUBROUTINE INI_PHIREF(
     I                       myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PHIREF                                    
C     | o Set reference potential at level center and 
C     |   level interface, using tRef,sRef profiles.
C     | note: use same discretisation as in calc_phi_hyd            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER K, Ks, stdUnit
      _RL ddPI
      _RL rHalf(2*Nr+1)
CEOP

      _BEGIN_MASTER( myThid )

      DO K=1,2*Nr
        phiRef(K) = 0.
      ENDDO
      stdUnit = standardMessageUnit

      DO K=1,Nr
        rHalf(2*K-1) = rF(K)
        rHalf(2*K)   = rC(K)
      ENDDO
      rHalf(2*Nr+1) = rF(Nr+1)

      IF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C- Compute Reference Geopotential at Half levels :
C    Tracer level: phiRef(2K)  ;  Interface_W level: phiRef(2K+1)

       phiRef(1) = 0. 

       IF (integr_GeoPot.EQ.1) THEN
C- Finite Volume Form, linear by half level :
        DO K=1,2*Nr
          Ks = (K+1)/2
          ddPI=atm_Cp*( ((rHalf( K )/atm_Po)**atm_kappa)
     &                 -((rHalf(K+1)/atm_Po)**atm_kappa) ) 
          phiRef(K+1) = phiRef(K)+ddPI*tRef(Ks)
        ENDDO
C------
       ELSE
C- Finite Difference Form, linear between Tracer level :
C   works with integr_GeoPot = 0, 2 or 3
        K = 1
          ddPI=atm_Cp*( ((rF(K)/atm_Po)**atm_kappa)
     &                 -((rC(K)/atm_Po)**atm_kappa) ) 
          phiRef(2*K)   = phiRef(1) + ddPI*tRef(K) 
        DO K=1,Nr-1
          ddPI=atm_Cp*( ((rC( K )/atm_Po)**atm_kappa)
     &                 -((rC(K+1)/atm_Po)**atm_kappa) ) 
          phiRef(2*K+1) = phiRef(2*K) + ddPI*0.5*tRef(K)
          phiRef(2*K+2) = phiRef(2*K)
     &                  + ddPI*0.5*(tRef(K)+tRef(K+1)) 
        ENDDO
        K = Nr
          ddPI=atm_Cp*( ((rC( K )/atm_Po)**atm_kappa)
     &                 -((rF(K+1)/atm_Po)**atm_kappa) ) 
          phiRef(2*K+1) = phiRef(2*K) + ddPI*tRef(K)
C------
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- Write to check :
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') 
     &  'INI_PHIREF: PhiRef/g [m] at level Center (integer)'
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') 
     &  '                     and at level Interface (half-int.) :'
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       DO K=1,2*Nr+1
        WRITE(msgBuf,'(A,F5.1,A,F10.1,A,F12.3)')
     &    ' K=',K*0.5,'  ;  r=',rHalf(K),'  ;  phiRef/g=', 
     &    phiRef(K)*recip_gravity
        CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       ENDDO

C-- endif buoyancyRelation 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
