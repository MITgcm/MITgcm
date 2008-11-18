C $Header: /u/gcmpack/MITgcm/pkg/fizhi/getcon.F,v 1.3 2008/11/18 21:37:57 jmc Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: GETCON

C     !INTERFACE:
      FUNCTION GETCON(NAME)

C     !DESCRIPTION:
C     Generic function getcon is a repository of global variables,
C     i.e. a memory for scalar values needed throughout a large
C     program.  This function remembers floating point values.  The
C     function is called with a character name to interrogate a value.
      
C     !USES:
      implicit none
#include "CPP_EEOPTIONS.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) NAME
CEOP

      integer I
      INTEGER MAXCON
      PARAMETER (MAXCON=46)
      CHARACTER*16 ANAME(MAXCON)
      _RL  ACON (MAXCON)
      _RL  GETCON
      _RL  VECMAX, UNDEF, OB, ECC, AE, EQNX, SOLS, S0, GRAV, 
     &     SRFPRS, PIMEAN, PSTD, TSTD, SDAY, SSALB, CO2, 
     &     CFC11, CFC12, CFC22, ATMPOPA, PI, CPD, CPV, ALHL, ALHS, 
     &     STFBOL, AIRMW, H2OMW, RUNIV, RGAS, RVAP, RKAP, HEATW, 
     &     HEATI, TICE, VKRM, DEG2RAD, CALTOJ, EPS, VIRTCON, EPSFAC,
     &     YRLEN, DAYSCY, PERHDEG, OBLDEG

C     COMPUTATIONAL CONSTANTS
C     -----------------------
      PARAMETER ( VECMAX = 65535.5    )
      PARAMETER ( UNDEF  = 1.d15      )

C     ASTRONOMICAL CONSTANTS
C     ----------------------
      PARAMETER ( OB     = 23.45      )
      PARAMETER ( AE     = 6371E3     )
      PARAMETER ( SOLS   = 176.5      )
      PARAMETER ( S0     = 1365.0     )
      PARAMETER ( EQNX   = 80.9028    )
      PARAMETER ( ECC    = 0.0167     )
      PARAMETER ( PERHDEG= 102.0      )
      PARAMETER ( OBLDEG = 23.45      )

C     TERRESTRIAL CONSTANTS
C     ---------------------
      PARAMETER ( GRAV   = 9.81d0     )
      PARAMETER ( SRFPRS = 984.7      )
      PARAMETER ( PIMEAN = 984.7      )
      PARAMETER ( PSTD   = 1000.0     )
      PARAMETER ( TSTD   = 280.0      )
      PARAMETER ( SDAY   = 86400.0    )
      PARAMETER ( SSALB  = 0.99       )
      PARAMETER ( CO2    = 355.0      )
      PARAMETER ( CFC11  = 0.3        )
      PARAMETER ( CFC12  = 0.5        )
      PARAMETER ( CFC22  = 0.2        )
      PARAMETER ( ATMPOPA= 1.d5       )
      PARAMETER ( PI     = 3.14159d0  )
      PARAMETER ( YRLEN  = 365.25     )
      PARAMETER ( DAYSCY = 365 *4 +1  )

C     THERMODYNAMIC CONSTANTS
C     -----------------------
      PARAMETER ( CPD    = 1004.16d0  )
      PARAMETER ( CPV    = 1869.46    )
      PARAMETER ( ALHL   = 2.499E6    )
      PARAMETER ( ALHS   = 2.845E6    )
      PARAMETER ( STFBOL = 5.67E-8    )
      PARAMETER ( AIRMW  = 28.97d0    )
      PARAMETER ( H2OMW  = 18.01d0    )
      PARAMETER ( RUNIV  = 8314.3d0   )
      PARAMETER ( RGAS   = RUNIV/AIRMW)
      PARAMETER ( RVAP   = RUNIV/H2OMW)
      PARAMETER ( RKAP   = RGAS/CPD   )
      PARAMETER ( HEATW  = 597.2      )
      PARAMETER ( HEATI  = 680.0      )
      PARAMETER ( TICE   = 273.16     )

C     TURBULENCE CONSTANTS
C     --------------------
      PARAMETER ( VKRM   = 0.4        )

C     CONVERSION CONSTANTS
C     --------------------
      PARAMETER ( DEG2RAD  = PI/180.d0)
      PARAMETER ( CALTOJ = 4184.      )

C     MOISTURE CONSTANTS
C     ------------------
      PARAMETER ( EPS    = 0.622      )
      PARAMETER ( VIRTCON= 0.609      )
      PARAMETER ( EPSFAC = EPS*HEATW/RGAS*CALTOJ )


       DATA ANAME(1 ),ACON(1 ) / 'CP              ', CPD    /
       DATA ANAME(2 ),ACON(2 ) / 'RGAS            ', RGAS   /
       DATA ANAME(3 ),ACON(3 ) / 'KAPPA           ', RKAP   /
       DATA ANAME(4 ),ACON(4 ) / 'LATENT HEAT COND', ALHL   /
       DATA ANAME(5 ),ACON(5 ) / 'GRAVITY         ', GRAV   /
       DATA ANAME(6 ),ACON(6 ) / 'STEFAN-BOLTZMAN ', STFBOL /
       DATA ANAME(7 ),ACON(7 ) / 'VON KARMAN      ', VKRM   /
       DATA ANAME(8 ),ACON(8 ) / 'EARTH RADIUS    ', AE     /
       DATA ANAME(9 ),ACON(9 ) / 'OBLIQUITY       ', OB     /
       DATA ANAME(10),ACON(10) / 'ECCENTRICITY    ', ECC    /
       DATA ANAME(11),ACON(11) / 'PERIHELION      ', PERHDEG/
       DATA ANAME(12),ACON(12) / 'VERNAL EQUINOX  ', EQNX   /
       DATA ANAME(13),ACON(13) / 'SUMMER SOLSTICE ', SOLS   /
       DATA ANAME(14),ACON(14) / 'MAX VECT LENGTH ', VECMAX /
       DATA ANAME(15),ACON(15) / 'MOL WT H2O      ', H2OMW  /
       DATA ANAME(16),ACON(16) / 'MOL WT AIR      ', AIRMW  /
       DATA ANAME(17),ACON(17) / 'CPV             ', CPV    /
       DATA ANAME(18),ACON(18) / 'CPD             ', CPD    /
       DATA ANAME(19),ACON(19) / 'UNIV GAS CONST  ', RUNIV  /
       DATA ANAME(20),ACON(20) / 'LATENT HEAT SBLM', ALHS   /
       DATA ANAME(21),ACON(21) / 'FREEZING-POINT  ', TICE   /
       DATA ANAME(23),ACON(23) / 'CALTOJ          ', CALTOJ /
       DATA ANAME(24),ACON(24) / 'EPS             ', EPS    /
       DATA ANAME(25),ACON(25) / 'HEATW           ', HEATW  /
       DATA ANAME(26),ACON(26) / 'EPSFAC          ', EPSFAC /
       DATA ANAME(27),ACON(27) / 'VIRTCON         ', VIRTCON/
       DATA ANAME(28),ACON(28) / 'PIMEAN          ', PIMEAN /
       DATA ANAME(29),ACON(29) / 'SDAY            ', SDAY   /
       DATA ANAME(30),ACON(30) / 'HEATI           ', HEATI  /
       DATA ANAME(31),ACON(31) / 'S0              ', S0     /
       DATA ANAME(32),ACON(32) / 'PSTD            ', PSTD   /
       DATA ANAME(33),ACON(33) / 'TSTD            ', TSTD   /
       DATA ANAME(34),ACON(34) / 'SSALB           ', SSALB  /
       DATA ANAME(35),ACON(35) / 'UNDEF           ', UNDEF  /
       DATA ANAME(36),ACON(36) / 'CO2             ', CO2    /
       DATA ANAME(37),ACON(37) / 'RVAP            ', RVAP   /
       DATA ANAME(38),ACON(38) / 'CFC11           ', CFC11  /
       DATA ANAME(39),ACON(39) / 'CFC12           ', CFC12  /
       DATA ANAME(40),ACON(40) / 'CFC22           ', CFC22  /
       DATA ANAME(41),ACON(41) / 'DEG2RAD         ', DEG2RAD /
       DATA ANAME(42),ACON(42) / 'ATMPOPA         ', ATMPOPA/
       DATA ANAME(43),ACON(43) / 'PI              ', PI     /
       DATA ANAME(44),ACON(44) / 'YRLEN           ', YRLEN  /
       DATA ANAME(45),ACON(45) / 'DAYSCY          ',DAYSCY  /
       DATA ANAME(46),ACON(46) / 'OBLDEG          ',OBLDEG  /

      DO 10 I=1,MAXCON
      IF(NAME.EQ.ANAME(I)) THEN
         GETCON = ACON(I)
         RETURN
      ENDIF
10    CONTINUE

900   PRINT *,' CANNOT FIND FLOATING POINT CONSTANT - ',NAME
      PRINT *,' GETCON - CANNOT FIND CONSTANT REQUESTED'
      RETURN
      END
