      FUNCTION GETCON (NAME)                                                    
C***********************************************************************        
C  GENERIC FUNCTION GETCON IS A REPOSITORY OF GLOBAL VARIABLES, I.E.            
C  A MEMORY FOR SCALAR VALUES NEEDED THROUGHOUT A LARGE PROGRAM.                
C                                                                               
C  THIS SPECIFIC FUNCTION, GETCON, REMEMBERS FLOATING POINT VALUES.             
C  THE FUNCTION IS CALLED WITH A CHARACTER NAME TO INTERROGATE A VALUE.         
C***********************************************************************        
C                                                                               
      INTEGER MAXCON
      PARAMETER (MAXCON=43)                                                     
      CHARACTER*(*) NAME                                                        
      CHARACTER*16 ANAME(MAXCON)                                                
      real         ACON (MAXCON)                                                
      integer I

C COMPUTATIONAL CONSTANTS                                                       
C -----------------------                                                       
      PARAMETER ( VECMAX = 65535.5    )                                         
      PARAMETER ( UNDEF  = 1.E15      )                                         

C ASTRONOMICAL CONSTANTS                                                        
C ----------------------                                                        
      PARAMETER ( OB     = 23.45      )                                         
      PARAMETER ( PER    = 102.       )                                         
      PARAMETER ( ECC    = .0167      )                                         
      PARAMETER ( AE     = 6371E3     )                                         
      PARAMETER ( EQNX   = 80.5       )                                         
      PARAMETER ( SOLS   = 176.5      )                                         
      PARAMETER ( S0     = 1365.0     )                                         

C TERRESTRIAL CONSTANTS                                                         
C ---------------------                                                         
      PARAMETER ( GRAV   = 9.81       )                                         
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
      PARAMETER ( ATMPOPA= 1.E5       )                                         
      PARAMETER ( PI     = 3.14159    )                                         

C THERMODYNAMIC CONSTANTS                                                       
C -----------------------                                                       
      PARAMETER ( CPD    = 1004.16    )                                         
      PARAMETER ( CPV    = 1869.46    )                                         
      PARAMETER ( ALHL   = 2.499E6    )                                         
      PARAMETER ( ALHS   = 2.845E6    )                                         
      PARAMETER ( STFBOL = 5.67E-8    )                                         
      PARAMETER ( AIRMW  = 28.97      )                                         
      PARAMETER ( H2OMW  = 18.01      )                                         
      PARAMETER ( RUNIV  = 8314.3     )                                         
      PARAMETER ( RGAS   = RUNIV/AIRMW)                                         
      PARAMETER ( RVAP   = RUNIV/H2OMW)                                         
      PARAMETER ( RKAP   = RGAS/CPD   )                                         
      PARAMETER ( HEATW  = 597.2      )                                         
      PARAMETER ( HEATI  = 680.0      )                                         
      PARAMETER ( TICE   = 273.16     )                                         

C TURBULENCE CONSTANTS                                                          
C --------------------                                                          
      PARAMETER ( VKRM   = 0.4        )

C CONVERSION CONSTANTS                                                            
C ------------------                                                            
      PARAMETER ( DEG2RAD  = 2.*PI/360. )                                        
      PARAMETER ( CALTOJ = 4184.      )                                         

C MOISTURE CONSTANTS                                                            
C ------------------                                                            
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
       DATA ANAME(11),ACON(11) / 'PERIHELION      ', PER    /                   
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
