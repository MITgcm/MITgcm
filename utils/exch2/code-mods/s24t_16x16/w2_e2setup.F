       SUBROUTINE W2_E2SETUP                       
                                                  
C      Tile toplogy settings data structures      
#include "W2_EXCH2_TOPOLOGY.h"
                                                  
C      Tile settings table                        
       CALL W2_E2SETUP_1
       CALL W2_E2SETUP_2
       CALL W2_E2SETUP_3
       CALL W2_E2SETUP_4
       CALL W2_E2SETUP_5
       CALL W2_E2SETUP_6
       CALL W2_E2SETUP_7
       CALL W2_E2SETUP_8
       CALL W2_E2SETUP_9
       CALL W2_E2SETUP_10
       CALL W2_E2SETUP_11
       CALL W2_E2SETUP_12
       CALL W2_E2SETUP_13
       CALL W2_E2SETUP_14
       CALL W2_E2SETUP_15
       CALL W2_E2SETUP_16
       CALL W2_E2SETUP_17
       CALL W2_E2SETUP_18
       CALL W2_E2SETUP_19
       CALL W2_E2SETUP_20
       CALL W2_E2SETUP_21
       CALL W2_E2SETUP_22
       CALL W2_E2SETUP_23
       CALL W2_E2SETUP_24
       RETURN
       END
       SUBROUTINE W2_E2SETUP_1
C      Topology for tile 1
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(1)=16
       exch2_tny(1)=16
       exch2_tbasex(1)=0
       exch2_tbasey(1)=0
       exch2_txglobalo(1)=1
       exch2_tyglobalo(1)=1
       exch2_isWedge(1)=1
       exch2_isNedge(1)=0
       exch2_isEedge(1)=0
       exch2_isSedge(1)=1
       exch2_myFace(1)=1
       exch2_mydnx(1)=32
       exch2_mydny(1)=32
       exch2_nNeighbours(1)=4

       exch2_neighbourId(1,1)=3
       exch2_pi(1,1,1)=1
       exch2_pi(2,1,1)=0
       exch2_pj(1,1,1)=0
       exch2_pj(2,1,1)=1
       exch2_oi(1,1)=0
       exch2_oi_f(1,1)=0
       exch2_oj(1,1)=0
       exch2_oj_f(1,1)=0
       exch2_itlo_c(1,1)=0
       exch2_ithi_c(1,1)=17
       exch2_jtlo_c(1,1)=0
       exch2_jthi_c(1,1)=0
       exch2_opposingSend_record(1,1)=2

       exch2_neighbourId(2,1)=23
       exch2_pi(1,2,1)=1
       exch2_pi(2,2,1)=0
       exch2_pj(1,2,1)=0
       exch2_pj(2,2,1)=1
       exch2_oi(2,1)=0
       exch2_oi_f(2,1)=0
       exch2_oj(2,1)=-32
       exch2_oj_f(2,1)=-32
       exch2_itlo_c(2,1)=0
       exch2_ithi_c(2,1)=17
       exch2_jtlo_c(2,1)=17
       exch2_jthi_c(2,1)=17
       exch2_opposingSend_record(2,1)=1

       exch2_neighbourId(3,1)=2
       exch2_pi(1,3,1)=1
       exch2_pi(2,3,1)=0
       exch2_pj(1,3,1)=0
       exch2_pj(2,3,1)=1
       exch2_oi(3,1)=0
       exch2_oi_f(3,1)=0
       exch2_oj(3,1)=0
       exch2_oj_f(3,1)=0
       exch2_itlo_c(3,1)=0
       exch2_ithi_c(3,1)=0
       exch2_jtlo_c(3,1)=0
       exch2_jthi_c(3,1)=17
       exch2_opposingSend_record(3,1)=4

       exch2_neighbourId(4,1)=20
       exch2_pi(1,4,1)=0
       exch2_pi(2,4,1)=1
       exch2_pj(1,4,1)=-1
       exch2_pj(2,4,1)=0
       exch2_oi(4,1)=-32
       exch2_oi_f(4,1)=-32
       exch2_oj(4,1)=33
       exch2_oj_f(4,1)=34
       exch2_itlo_c(4,1)=0
       exch2_ithi_c(4,1)=17
       exch2_jtlo_c(4,1)=17
       exch2_jthi_c(4,1)=17
       exch2_opposingSend_record(4,1)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_2
C      Topology for tile 2
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(2)=16
       exch2_tny(2)=16
       exch2_tbasex(2)=16
       exch2_tbasey(2)=0
       exch2_txglobalo(2)=17
       exch2_tyglobalo(2)=1
       exch2_isWedge(2)=0
       exch2_isNedge(2)=0
       exch2_isEedge(2)=1
       exch2_isSedge(2)=1
       exch2_myFace(2)=1
       exch2_mydnx(2)=32
       exch2_mydny(2)=32
       exch2_nNeighbours(2)=4

       exch2_neighbourId(1,2)=4
       exch2_pi(1,1,2)=1
       exch2_pi(2,1,2)=0
       exch2_pj(1,1,2)=0
       exch2_pj(2,1,2)=1
       exch2_oi(1,2)=0
       exch2_oi_f(1,2)=0
       exch2_oj(1,2)=0
       exch2_oj_f(1,2)=0
       exch2_itlo_c(1,2)=0
       exch2_ithi_c(1,2)=17
       exch2_jtlo_c(1,2)=0
       exch2_jthi_c(1,2)=0
       exch2_opposingSend_record(1,2)=2

       exch2_neighbourId(2,2)=24
       exch2_pi(1,2,2)=1
       exch2_pi(2,2,2)=0
       exch2_pj(1,2,2)=0
       exch2_pj(2,2,2)=1
       exch2_oi(2,2)=0
       exch2_oi_f(2,2)=0
       exch2_oj(2,2)=-32
       exch2_oj_f(2,2)=-32
       exch2_itlo_c(2,2)=0
       exch2_ithi_c(2,2)=17
       exch2_jtlo_c(2,2)=17
       exch2_jthi_c(2,2)=17
       exch2_opposingSend_record(2,2)=1

       exch2_neighbourId(3,2)=5
       exch2_pi(1,3,2)=1
       exch2_pi(2,3,2)=0
       exch2_pj(1,3,2)=0
       exch2_pj(2,3,2)=1
       exch2_oi(3,2)=32
       exch2_oi_f(3,2)=32
       exch2_oj(3,2)=0
       exch2_oj_f(3,2)=0
       exch2_itlo_c(3,2)=0
       exch2_ithi_c(3,2)=0
       exch2_jtlo_c(3,2)=0
       exch2_jthi_c(3,2)=17
       exch2_opposingSend_record(3,2)=4

       exch2_neighbourId(4,2)=1
       exch2_pi(1,4,2)=1
       exch2_pi(2,4,2)=0
       exch2_pj(1,4,2)=0
       exch2_pj(2,4,2)=1
       exch2_oi(4,2)=0
       exch2_oi_f(4,2)=0
       exch2_oj(4,2)=0
       exch2_oj_f(4,2)=0
       exch2_itlo_c(4,2)=17
       exch2_ithi_c(4,2)=17
       exch2_jtlo_c(4,2)=0
       exch2_jthi_c(4,2)=17
       exch2_opposingSend_record(4,2)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_3
C      Topology for tile 3
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(3)=16
       exch2_tny(3)=16
       exch2_tbasex(3)=0
       exch2_tbasey(3)=16
       exch2_txglobalo(3)=1
       exch2_tyglobalo(3)=17
       exch2_isWedge(3)=1
       exch2_isNedge(3)=1
       exch2_isEedge(3)=0
       exch2_isSedge(3)=0
       exch2_myFace(3)=1
       exch2_mydnx(3)=32
       exch2_mydny(3)=32
       exch2_nNeighbours(3)=4

       exch2_neighbourId(1,3)=11
       exch2_pi(1,1,3)=0
       exch2_pi(2,1,3)=-1
       exch2_pj(1,1,3)=1
       exch2_pj(2,1,3)=0
       exch2_oi(1,3)=33
       exch2_oi_f(1,3)=34
       exch2_oj(1,3)=32
       exch2_oj_f(1,3)=32
       exch2_itlo_c(1,3)=0
       exch2_ithi_c(1,3)=0
       exch2_jtlo_c(1,3)=0
       exch2_jthi_c(1,3)=17
       exch2_opposingSend_record(1,3)=4

       exch2_neighbourId(2,3)=1
       exch2_pi(1,2,3)=1
       exch2_pi(2,2,3)=0
       exch2_pj(1,2,3)=0
       exch2_pj(2,2,3)=1
       exch2_oi(2,3)=0
       exch2_oi_f(2,3)=0
       exch2_oj(2,3)=0
       exch2_oj_f(2,3)=0
       exch2_itlo_c(2,3)=1
       exch2_ithi_c(2,3)=16
       exch2_jtlo_c(2,3)=17
       exch2_jthi_c(2,3)=17
       exch2_opposingSend_record(2,3)=1

       exch2_neighbourId(3,3)=4
       exch2_pi(1,3,3)=1
       exch2_pi(2,3,3)=0
       exch2_pj(1,3,3)=0
       exch2_pj(2,3,3)=1
       exch2_oi(3,3)=0
       exch2_oi_f(3,3)=0
       exch2_oj(3,3)=0
       exch2_oj_f(3,3)=0
       exch2_itlo_c(3,3)=0
       exch2_ithi_c(3,3)=0
       exch2_jtlo_c(3,3)=0
       exch2_jthi_c(3,3)=17
       exch2_opposingSend_record(3,3)=4

       exch2_neighbourId(4,3)=19
       exch2_pi(1,4,3)=0
       exch2_pi(2,4,3)=1
       exch2_pj(1,4,3)=-1
       exch2_pj(2,4,3)=0
       exch2_oi(4,3)=-32
       exch2_oi_f(4,3)=-32
       exch2_oj(4,3)=33
       exch2_oj_f(4,3)=34
       exch2_itlo_c(4,3)=0
       exch2_ithi_c(4,3)=17
       exch2_jtlo_c(4,3)=17
       exch2_jthi_c(4,3)=17
       exch2_opposingSend_record(4,3)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_4
C      Topology for tile 4
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(4)=16
       exch2_tny(4)=16
       exch2_tbasex(4)=16
       exch2_tbasey(4)=16
       exch2_txglobalo(4)=17
       exch2_tyglobalo(4)=17
       exch2_isWedge(4)=0
       exch2_isNedge(4)=1
       exch2_isEedge(4)=1
       exch2_isSedge(4)=0
       exch2_myFace(4)=1
       exch2_mydnx(4)=32
       exch2_mydny(4)=32
       exch2_nNeighbours(4)=4

       exch2_neighbourId(1,4)=9
       exch2_pi(1,1,4)=0
       exch2_pi(2,1,4)=-1
       exch2_pj(1,1,4)=1
       exch2_pj(2,1,4)=0
       exch2_oi(1,4)=33
       exch2_oi_f(1,4)=34
       exch2_oj(1,4)=32
       exch2_oj_f(1,4)=32
       exch2_itlo_c(1,4)=0
       exch2_ithi_c(1,4)=0
       exch2_jtlo_c(1,4)=0
       exch2_jthi_c(1,4)=17
       exch2_opposingSend_record(1,4)=4

       exch2_neighbourId(2,4)=2
       exch2_pi(1,2,4)=1
       exch2_pi(2,2,4)=0
       exch2_pj(1,2,4)=0
       exch2_pj(2,2,4)=1
       exch2_oi(2,4)=0
       exch2_oi_f(2,4)=0
       exch2_oj(2,4)=0
       exch2_oj_f(2,4)=0
       exch2_itlo_c(2,4)=1
       exch2_ithi_c(2,4)=16
       exch2_jtlo_c(2,4)=17
       exch2_jthi_c(2,4)=17
       exch2_opposingSend_record(2,4)=1

       exch2_neighbourId(3,4)=7
       exch2_pi(1,3,4)=1
       exch2_pi(2,3,4)=0
       exch2_pj(1,3,4)=0
       exch2_pj(2,3,4)=1
       exch2_oi(3,4)=32
       exch2_oi_f(3,4)=32
       exch2_oj(3,4)=0
       exch2_oj_f(3,4)=0
       exch2_itlo_c(3,4)=0
       exch2_ithi_c(3,4)=0
       exch2_jtlo_c(3,4)=0
       exch2_jthi_c(3,4)=17
       exch2_opposingSend_record(3,4)=4

       exch2_neighbourId(4,4)=3
       exch2_pi(1,4,4)=1
       exch2_pi(2,4,4)=0
       exch2_pj(1,4,4)=0
       exch2_pj(2,4,4)=1
       exch2_oi(4,4)=0
       exch2_oi_f(4,4)=0
       exch2_oj(4,4)=0
       exch2_oj_f(4,4)=0
       exch2_itlo_c(4,4)=17
       exch2_ithi_c(4,4)=17
       exch2_jtlo_c(4,4)=0
       exch2_jthi_c(4,4)=17
       exch2_opposingSend_record(4,4)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_5
C      Topology for tile 5
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(5)=16
       exch2_tny(5)=16
       exch2_tbasex(5)=0
       exch2_tbasey(5)=0
       exch2_txglobalo(5)=33
       exch2_tyglobalo(5)=1
       exch2_isWedge(5)=1
       exch2_isNedge(5)=0
       exch2_isEedge(5)=0
       exch2_isSedge(5)=1
       exch2_myFace(5)=2
       exch2_mydnx(5)=32
       exch2_mydny(5)=32
       exch2_nNeighbours(5)=4

       exch2_neighbourId(1,5)=7
       exch2_pi(1,1,5)=1
       exch2_pi(2,1,5)=0
       exch2_pj(1,1,5)=0
       exch2_pj(2,1,5)=1
       exch2_oi(1,5)=0
       exch2_oi_f(1,5)=0
       exch2_oj(1,5)=0
       exch2_oj_f(1,5)=0
       exch2_itlo_c(1,5)=0
       exch2_ithi_c(1,5)=17
       exch2_jtlo_c(1,5)=0
       exch2_jthi_c(1,5)=0
       exch2_opposingSend_record(1,5)=2

       exch2_neighbourId(2,5)=24
       exch2_pi(1,2,5)=0
       exch2_pi(2,2,5)=-1
       exch2_pj(1,2,5)=1
       exch2_pj(2,2,5)=0
       exch2_oi(2,5)=33
       exch2_oi_f(2,5)=34
       exch2_oj(2,5)=-32
       exch2_oj_f(2,5)=-32
       exch2_itlo_c(2,5)=17
       exch2_ithi_c(2,5)=17
       exch2_jtlo_c(2,5)=0
       exch2_jthi_c(2,5)=17
       exch2_opposingSend_record(2,5)=3

       exch2_neighbourId(3,5)=6
       exch2_pi(1,3,5)=1
       exch2_pi(2,3,5)=0
       exch2_pj(1,3,5)=0
       exch2_pj(2,3,5)=1
       exch2_oi(3,5)=0
       exch2_oi_f(3,5)=0
       exch2_oj(3,5)=0
       exch2_oj_f(3,5)=0
       exch2_itlo_c(3,5)=0
       exch2_ithi_c(3,5)=0
       exch2_jtlo_c(3,5)=0
       exch2_jthi_c(3,5)=17
       exch2_opposingSend_record(3,5)=4

       exch2_neighbourId(4,5)=2
       exch2_pi(1,4,5)=1
       exch2_pi(2,4,5)=0
       exch2_pj(1,4,5)=0
       exch2_pj(2,4,5)=1
       exch2_oi(4,5)=-32
       exch2_oi_f(4,5)=-32
       exch2_oj(4,5)=0
       exch2_oj_f(4,5)=0
       exch2_itlo_c(4,5)=17
       exch2_ithi_c(4,5)=17
       exch2_jtlo_c(4,5)=0
       exch2_jthi_c(4,5)=17
       exch2_opposingSend_record(4,5)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_6
C      Topology for tile 6
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(6)=16
       exch2_tny(6)=16
       exch2_tbasex(6)=16
       exch2_tbasey(6)=0
       exch2_txglobalo(6)=49
       exch2_tyglobalo(6)=1
       exch2_isWedge(6)=0
       exch2_isNedge(6)=0
       exch2_isEedge(6)=1
       exch2_isSedge(6)=1
       exch2_myFace(6)=2
       exch2_mydnx(6)=32
       exch2_mydny(6)=32
       exch2_nNeighbours(6)=4

       exch2_neighbourId(1,6)=8
       exch2_pi(1,1,6)=1
       exch2_pi(2,1,6)=0
       exch2_pj(1,1,6)=0
       exch2_pj(2,1,6)=1
       exch2_oi(1,6)=0
       exch2_oi_f(1,6)=0
       exch2_oj(1,6)=0
       exch2_oj_f(1,6)=0
       exch2_itlo_c(1,6)=0
       exch2_ithi_c(1,6)=17
       exch2_jtlo_c(1,6)=0
       exch2_jthi_c(1,6)=0
       exch2_opposingSend_record(1,6)=2

       exch2_neighbourId(2,6)=22
       exch2_pi(1,2,6)=0
       exch2_pi(2,2,6)=-1
       exch2_pj(1,2,6)=1
       exch2_pj(2,2,6)=0
       exch2_oi(2,6)=33
       exch2_oi_f(2,6)=34
       exch2_oj(2,6)=-32
       exch2_oj_f(2,6)=-32
       exch2_itlo_c(2,6)=17
       exch2_ithi_c(2,6)=17
       exch2_jtlo_c(2,6)=0
       exch2_jthi_c(2,6)=17
       exch2_opposingSend_record(2,6)=3

       exch2_neighbourId(3,6)=14
       exch2_pi(1,3,6)=0
       exch2_pi(2,3,6)=1
       exch2_pj(1,3,6)=-1
       exch2_pj(2,3,6)=0
       exch2_oi(3,6)=32
       exch2_oi_f(3,6)=32
       exch2_oj(3,6)=33
       exch2_oj_f(3,6)=34
       exch2_itlo_c(3,6)=0
       exch2_ithi_c(3,6)=17
       exch2_jtlo_c(3,6)=0
       exch2_jthi_c(3,6)=0
       exch2_opposingSend_record(3,6)=2

       exch2_neighbourId(4,6)=5
       exch2_pi(1,4,6)=1
       exch2_pi(2,4,6)=0
       exch2_pj(1,4,6)=0
       exch2_pj(2,4,6)=1
       exch2_oi(4,6)=0
       exch2_oi_f(4,6)=0
       exch2_oj(4,6)=0
       exch2_oj_f(4,6)=0
       exch2_itlo_c(4,6)=17
       exch2_ithi_c(4,6)=17
       exch2_jtlo_c(4,6)=0
       exch2_jthi_c(4,6)=17
       exch2_opposingSend_record(4,6)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_7
C      Topology for tile 7
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(7)=16
       exch2_tny(7)=16
       exch2_tbasex(7)=0
       exch2_tbasey(7)=16
       exch2_txglobalo(7)=33
       exch2_tyglobalo(7)=17
       exch2_isWedge(7)=1
       exch2_isNedge(7)=1
       exch2_isEedge(7)=0
       exch2_isSedge(7)=0
       exch2_myFace(7)=2
       exch2_mydnx(7)=32
       exch2_mydny(7)=32
       exch2_nNeighbours(7)=4

       exch2_neighbourId(1,7)=9
       exch2_pi(1,1,7)=1
       exch2_pi(2,1,7)=0
       exch2_pj(1,1,7)=0
       exch2_pj(2,1,7)=1
       exch2_oi(1,7)=0
       exch2_oi_f(1,7)=0
       exch2_oj(1,7)=32
       exch2_oj_f(1,7)=32
       exch2_itlo_c(1,7)=0
       exch2_ithi_c(1,7)=17
       exch2_jtlo_c(1,7)=0
       exch2_jthi_c(1,7)=0
       exch2_opposingSend_record(1,7)=2

       exch2_neighbourId(2,7)=5
       exch2_pi(1,2,7)=1
       exch2_pi(2,2,7)=0
       exch2_pj(1,2,7)=0
       exch2_pj(2,2,7)=1
       exch2_oi(2,7)=0
       exch2_oi_f(2,7)=0
       exch2_oj(2,7)=0
       exch2_oj_f(2,7)=0
       exch2_itlo_c(2,7)=1
       exch2_ithi_c(2,7)=16
       exch2_jtlo_c(2,7)=17
       exch2_jthi_c(2,7)=17
       exch2_opposingSend_record(2,7)=1

       exch2_neighbourId(3,7)=8
       exch2_pi(1,3,7)=1
       exch2_pi(2,3,7)=0
       exch2_pj(1,3,7)=0
       exch2_pj(2,3,7)=1
       exch2_oi(3,7)=0
       exch2_oi_f(3,7)=0
       exch2_oj(3,7)=0
       exch2_oj_f(3,7)=0
       exch2_itlo_c(3,7)=0
       exch2_ithi_c(3,7)=0
       exch2_jtlo_c(3,7)=0
       exch2_jthi_c(3,7)=17
       exch2_opposingSend_record(3,7)=4

       exch2_neighbourId(4,7)=4
       exch2_pi(1,4,7)=1
       exch2_pi(2,4,7)=0
       exch2_pj(1,4,7)=0
       exch2_pj(2,4,7)=1
       exch2_oi(4,7)=-32
       exch2_oi_f(4,7)=-32
       exch2_oj(4,7)=0
       exch2_oj_f(4,7)=0
       exch2_itlo_c(4,7)=17
       exch2_ithi_c(4,7)=17
       exch2_jtlo_c(4,7)=0
       exch2_jthi_c(4,7)=17
       exch2_opposingSend_record(4,7)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_8
C      Topology for tile 8
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(8)=16
       exch2_tny(8)=16
       exch2_tbasex(8)=16
       exch2_tbasey(8)=16
       exch2_txglobalo(8)=49
       exch2_tyglobalo(8)=17
       exch2_isWedge(8)=0
       exch2_isNedge(8)=1
       exch2_isEedge(8)=1
       exch2_isSedge(8)=0
       exch2_myFace(8)=2
       exch2_mydnx(8)=32
       exch2_mydny(8)=32
       exch2_nNeighbours(8)=4

       exch2_neighbourId(1,8)=10
       exch2_pi(1,1,8)=1
       exch2_pi(2,1,8)=0
       exch2_pj(1,1,8)=0
       exch2_pj(2,1,8)=1
       exch2_oi(1,8)=0
       exch2_oi_f(1,8)=0
       exch2_oj(1,8)=32
       exch2_oj_f(1,8)=32
       exch2_itlo_c(1,8)=0
       exch2_ithi_c(1,8)=17
       exch2_jtlo_c(1,8)=0
       exch2_jthi_c(1,8)=0
       exch2_opposingSend_record(1,8)=2

       exch2_neighbourId(2,8)=6
       exch2_pi(1,2,8)=1
       exch2_pi(2,2,8)=0
       exch2_pj(1,2,8)=0
       exch2_pj(2,2,8)=1
       exch2_oi(2,8)=0
       exch2_oi_f(2,8)=0
       exch2_oj(2,8)=0
       exch2_oj_f(2,8)=0
       exch2_itlo_c(2,8)=1
       exch2_ithi_c(2,8)=16
       exch2_jtlo_c(2,8)=17
       exch2_jthi_c(2,8)=17
       exch2_opposingSend_record(2,8)=1

       exch2_neighbourId(3,8)=13
       exch2_pi(1,3,8)=0
       exch2_pi(2,3,8)=1
       exch2_pj(1,3,8)=-1
       exch2_pj(2,3,8)=0
       exch2_oi(3,8)=32
       exch2_oi_f(3,8)=32
       exch2_oj(3,8)=33
       exch2_oj_f(3,8)=34
       exch2_itlo_c(3,8)=0
       exch2_ithi_c(3,8)=17
       exch2_jtlo_c(3,8)=0
       exch2_jthi_c(3,8)=0
       exch2_opposingSend_record(3,8)=2

       exch2_neighbourId(4,8)=7
       exch2_pi(1,4,8)=1
       exch2_pi(2,4,8)=0
       exch2_pj(1,4,8)=0
       exch2_pj(2,4,8)=1
       exch2_oi(4,8)=0
       exch2_oi_f(4,8)=0
       exch2_oj(4,8)=0
       exch2_oj_f(4,8)=0
       exch2_itlo_c(4,8)=17
       exch2_ithi_c(4,8)=17
       exch2_jtlo_c(4,8)=0
       exch2_jthi_c(4,8)=17
       exch2_opposingSend_record(4,8)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_9
C      Topology for tile 9
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(9)=16
       exch2_tny(9)=16
       exch2_tbasex(9)=0
       exch2_tbasey(9)=0
       exch2_txglobalo(9)=65
       exch2_tyglobalo(9)=1
       exch2_isWedge(9)=1
       exch2_isNedge(9)=0
       exch2_isEedge(9)=0
       exch2_isSedge(9)=1
       exch2_myFace(9)=3
       exch2_mydnx(9)=32
       exch2_mydny(9)=32
       exch2_nNeighbours(9)=4

       exch2_neighbourId(1,9)=11
       exch2_pi(1,1,9)=1
       exch2_pi(2,1,9)=0
       exch2_pj(1,1,9)=0
       exch2_pj(2,1,9)=1
       exch2_oi(1,9)=0
       exch2_oi_f(1,9)=0
       exch2_oj(1,9)=0
       exch2_oj_f(1,9)=0
       exch2_itlo_c(1,9)=0
       exch2_ithi_c(1,9)=17
       exch2_jtlo_c(1,9)=0
       exch2_jthi_c(1,9)=0
       exch2_opposingSend_record(1,9)=2

       exch2_neighbourId(2,9)=7
       exch2_pi(1,2,9)=1
       exch2_pi(2,2,9)=0
       exch2_pj(1,2,9)=0
       exch2_pj(2,2,9)=1
       exch2_oi(2,9)=0
       exch2_oi_f(2,9)=0
       exch2_oj(2,9)=-32
       exch2_oj_f(2,9)=-32
       exch2_itlo_c(2,9)=0
       exch2_ithi_c(2,9)=17
       exch2_jtlo_c(2,9)=17
       exch2_jthi_c(2,9)=17
       exch2_opposingSend_record(2,9)=1

       exch2_neighbourId(3,9)=10
       exch2_pi(1,3,9)=1
       exch2_pi(2,3,9)=0
       exch2_pj(1,3,9)=0
       exch2_pj(2,3,9)=1
       exch2_oi(3,9)=0
       exch2_oi_f(3,9)=0
       exch2_oj(3,9)=0
       exch2_oj_f(3,9)=0
       exch2_itlo_c(3,9)=0
       exch2_ithi_c(3,9)=0
       exch2_jtlo_c(3,9)=0
       exch2_jthi_c(3,9)=17
       exch2_opposingSend_record(3,9)=4

       exch2_neighbourId(4,9)=4
       exch2_pi(1,4,9)=0
       exch2_pi(2,4,9)=1
       exch2_pj(1,4,9)=-1
       exch2_pj(2,4,9)=0
       exch2_oi(4,9)=-32
       exch2_oi_f(4,9)=-32
       exch2_oj(4,9)=33
       exch2_oj_f(4,9)=34
       exch2_itlo_c(4,9)=0
       exch2_ithi_c(4,9)=17
       exch2_jtlo_c(4,9)=17
       exch2_jthi_c(4,9)=17
       exch2_opposingSend_record(4,9)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_10
C      Topology for tile 10
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(10)=16
       exch2_tny(10)=16
       exch2_tbasex(10)=16
       exch2_tbasey(10)=0
       exch2_txglobalo(10)=81
       exch2_tyglobalo(10)=1
       exch2_isWedge(10)=0
       exch2_isNedge(10)=0
       exch2_isEedge(10)=1
       exch2_isSedge(10)=1
       exch2_myFace(10)=3
       exch2_mydnx(10)=32
       exch2_mydny(10)=32
       exch2_nNeighbours(10)=4

       exch2_neighbourId(1,10)=12
       exch2_pi(1,1,10)=1
       exch2_pi(2,1,10)=0
       exch2_pj(1,1,10)=0
       exch2_pj(2,1,10)=1
       exch2_oi(1,10)=0
       exch2_oi_f(1,10)=0
       exch2_oj(1,10)=0
       exch2_oj_f(1,10)=0
       exch2_itlo_c(1,10)=0
       exch2_ithi_c(1,10)=17
       exch2_jtlo_c(1,10)=0
       exch2_jthi_c(1,10)=0
       exch2_opposingSend_record(1,10)=2

       exch2_neighbourId(2,10)=8
       exch2_pi(1,2,10)=1
       exch2_pi(2,2,10)=0
       exch2_pj(1,2,10)=0
       exch2_pj(2,2,10)=1
       exch2_oi(2,10)=0
       exch2_oi_f(2,10)=0
       exch2_oj(2,10)=-32
       exch2_oj_f(2,10)=-32
       exch2_itlo_c(2,10)=0
       exch2_ithi_c(2,10)=17
       exch2_jtlo_c(2,10)=17
       exch2_jthi_c(2,10)=17
       exch2_opposingSend_record(2,10)=1

       exch2_neighbourId(3,10)=13
       exch2_pi(1,3,10)=1
       exch2_pi(2,3,10)=0
       exch2_pj(1,3,10)=0
       exch2_pj(2,3,10)=1
       exch2_oi(3,10)=32
       exch2_oi_f(3,10)=32
       exch2_oj(3,10)=0
       exch2_oj_f(3,10)=0
       exch2_itlo_c(3,10)=0
       exch2_ithi_c(3,10)=0
       exch2_jtlo_c(3,10)=0
       exch2_jthi_c(3,10)=17
       exch2_opposingSend_record(3,10)=4

       exch2_neighbourId(4,10)=9
       exch2_pi(1,4,10)=1
       exch2_pi(2,4,10)=0
       exch2_pj(1,4,10)=0
       exch2_pj(2,4,10)=1
       exch2_oi(4,10)=0
       exch2_oi_f(4,10)=0
       exch2_oj(4,10)=0
       exch2_oj_f(4,10)=0
       exch2_itlo_c(4,10)=17
       exch2_ithi_c(4,10)=17
       exch2_jtlo_c(4,10)=0
       exch2_jthi_c(4,10)=17
       exch2_opposingSend_record(4,10)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_11
C      Topology for tile 11
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(11)=16
       exch2_tny(11)=16
       exch2_tbasex(11)=0
       exch2_tbasey(11)=16
       exch2_txglobalo(11)=65
       exch2_tyglobalo(11)=17
       exch2_isWedge(11)=1
       exch2_isNedge(11)=1
       exch2_isEedge(11)=0
       exch2_isSedge(11)=0
       exch2_myFace(11)=3
       exch2_mydnx(11)=32
       exch2_mydny(11)=32
       exch2_nNeighbours(11)=4

       exch2_neighbourId(1,11)=19
       exch2_pi(1,1,11)=0
       exch2_pi(2,1,11)=-1
       exch2_pj(1,1,11)=1
       exch2_pj(2,1,11)=0
       exch2_oi(1,11)=33
       exch2_oi_f(1,11)=34
       exch2_oj(1,11)=32
       exch2_oj_f(1,11)=32
       exch2_itlo_c(1,11)=0
       exch2_ithi_c(1,11)=0
       exch2_jtlo_c(1,11)=0
       exch2_jthi_c(1,11)=17
       exch2_opposingSend_record(1,11)=4

       exch2_neighbourId(2,11)=9
       exch2_pi(1,2,11)=1
       exch2_pi(2,2,11)=0
       exch2_pj(1,2,11)=0
       exch2_pj(2,2,11)=1
       exch2_oi(2,11)=0
       exch2_oi_f(2,11)=0
       exch2_oj(2,11)=0
       exch2_oj_f(2,11)=0
       exch2_itlo_c(2,11)=1
       exch2_ithi_c(2,11)=16
       exch2_jtlo_c(2,11)=17
       exch2_jthi_c(2,11)=17
       exch2_opposingSend_record(2,11)=1

       exch2_neighbourId(3,11)=12
       exch2_pi(1,3,11)=1
       exch2_pi(2,3,11)=0
       exch2_pj(1,3,11)=0
       exch2_pj(2,3,11)=1
       exch2_oi(3,11)=0
       exch2_oi_f(3,11)=0
       exch2_oj(3,11)=0
       exch2_oj_f(3,11)=0
       exch2_itlo_c(3,11)=0
       exch2_ithi_c(3,11)=0
       exch2_jtlo_c(3,11)=0
       exch2_jthi_c(3,11)=17
       exch2_opposingSend_record(3,11)=4

       exch2_neighbourId(4,11)=3
       exch2_pi(1,4,11)=0
       exch2_pi(2,4,11)=1
       exch2_pj(1,4,11)=-1
       exch2_pj(2,4,11)=0
       exch2_oi(4,11)=-32
       exch2_oi_f(4,11)=-32
       exch2_oj(4,11)=33
       exch2_oj_f(4,11)=34
       exch2_itlo_c(4,11)=0
       exch2_ithi_c(4,11)=17
       exch2_jtlo_c(4,11)=17
       exch2_jthi_c(4,11)=17
       exch2_opposingSend_record(4,11)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_12
C      Topology for tile 12
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(12)=16
       exch2_tny(12)=16
       exch2_tbasex(12)=16
       exch2_tbasey(12)=16
       exch2_txglobalo(12)=81
       exch2_tyglobalo(12)=17
       exch2_isWedge(12)=0
       exch2_isNedge(12)=1
       exch2_isEedge(12)=1
       exch2_isSedge(12)=0
       exch2_myFace(12)=3
       exch2_mydnx(12)=32
       exch2_mydny(12)=32
       exch2_nNeighbours(12)=4

       exch2_neighbourId(1,12)=17
       exch2_pi(1,1,12)=0
       exch2_pi(2,1,12)=-1
       exch2_pj(1,1,12)=1
       exch2_pj(2,1,12)=0
       exch2_oi(1,12)=33
       exch2_oi_f(1,12)=34
       exch2_oj(1,12)=32
       exch2_oj_f(1,12)=32
       exch2_itlo_c(1,12)=0
       exch2_ithi_c(1,12)=0
       exch2_jtlo_c(1,12)=0
       exch2_jthi_c(1,12)=17
       exch2_opposingSend_record(1,12)=4

       exch2_neighbourId(2,12)=10
       exch2_pi(1,2,12)=1
       exch2_pi(2,2,12)=0
       exch2_pj(1,2,12)=0
       exch2_pj(2,2,12)=1
       exch2_oi(2,12)=0
       exch2_oi_f(2,12)=0
       exch2_oj(2,12)=0
       exch2_oj_f(2,12)=0
       exch2_itlo_c(2,12)=1
       exch2_ithi_c(2,12)=16
       exch2_jtlo_c(2,12)=17
       exch2_jthi_c(2,12)=17
       exch2_opposingSend_record(2,12)=1

       exch2_neighbourId(3,12)=15
       exch2_pi(1,3,12)=1
       exch2_pi(2,3,12)=0
       exch2_pj(1,3,12)=0
       exch2_pj(2,3,12)=1
       exch2_oi(3,12)=32
       exch2_oi_f(3,12)=32
       exch2_oj(3,12)=0
       exch2_oj_f(3,12)=0
       exch2_itlo_c(3,12)=0
       exch2_ithi_c(3,12)=0
       exch2_jtlo_c(3,12)=0
       exch2_jthi_c(3,12)=17
       exch2_opposingSend_record(3,12)=4

       exch2_neighbourId(4,12)=11
       exch2_pi(1,4,12)=1
       exch2_pi(2,4,12)=0
       exch2_pj(1,4,12)=0
       exch2_pj(2,4,12)=1
       exch2_oi(4,12)=0
       exch2_oi_f(4,12)=0
       exch2_oj(4,12)=0
       exch2_oj_f(4,12)=0
       exch2_itlo_c(4,12)=17
       exch2_ithi_c(4,12)=17
       exch2_jtlo_c(4,12)=0
       exch2_jthi_c(4,12)=17
       exch2_opposingSend_record(4,12)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_13
C      Topology for tile 13
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(13)=16
       exch2_tny(13)=16
       exch2_tbasex(13)=0
       exch2_tbasey(13)=0
       exch2_txglobalo(13)=97
       exch2_tyglobalo(13)=1
       exch2_isWedge(13)=1
       exch2_isNedge(13)=0
       exch2_isEedge(13)=0
       exch2_isSedge(13)=1
       exch2_myFace(13)=4
       exch2_mydnx(13)=32
       exch2_mydny(13)=32
       exch2_nNeighbours(13)=4

       exch2_neighbourId(1,13)=15
       exch2_pi(1,1,13)=1
       exch2_pi(2,1,13)=0
       exch2_pj(1,1,13)=0
       exch2_pj(2,1,13)=1
       exch2_oi(1,13)=0
       exch2_oi_f(1,13)=0
       exch2_oj(1,13)=0
       exch2_oj_f(1,13)=0
       exch2_itlo_c(1,13)=0
       exch2_ithi_c(1,13)=17
       exch2_jtlo_c(1,13)=0
       exch2_jthi_c(1,13)=0
       exch2_opposingSend_record(1,13)=2

       exch2_neighbourId(2,13)=8
       exch2_pi(1,2,13)=0
       exch2_pi(2,2,13)=-1
       exch2_pj(1,2,13)=1
       exch2_pj(2,2,13)=0
       exch2_oi(2,13)=33
       exch2_oi_f(2,13)=34
       exch2_oj(2,13)=-32
       exch2_oj_f(2,13)=-32
       exch2_itlo_c(2,13)=17
       exch2_ithi_c(2,13)=17
       exch2_jtlo_c(2,13)=0
       exch2_jthi_c(2,13)=17
       exch2_opposingSend_record(2,13)=3

       exch2_neighbourId(3,13)=14
       exch2_pi(1,3,13)=1
       exch2_pi(2,3,13)=0
       exch2_pj(1,3,13)=0
       exch2_pj(2,3,13)=1
       exch2_oi(3,13)=0
       exch2_oi_f(3,13)=0
       exch2_oj(3,13)=0
       exch2_oj_f(3,13)=0
       exch2_itlo_c(3,13)=0
       exch2_ithi_c(3,13)=0
       exch2_jtlo_c(3,13)=0
       exch2_jthi_c(3,13)=17
       exch2_opposingSend_record(3,13)=4

       exch2_neighbourId(4,13)=10
       exch2_pi(1,4,13)=1
       exch2_pi(2,4,13)=0
       exch2_pj(1,4,13)=0
       exch2_pj(2,4,13)=1
       exch2_oi(4,13)=-32
       exch2_oi_f(4,13)=-32
       exch2_oj(4,13)=0
       exch2_oj_f(4,13)=0
       exch2_itlo_c(4,13)=17
       exch2_ithi_c(4,13)=17
       exch2_jtlo_c(4,13)=0
       exch2_jthi_c(4,13)=17
       exch2_opposingSend_record(4,13)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_14
C      Topology for tile 14
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(14)=16
       exch2_tny(14)=16
       exch2_tbasex(14)=16
       exch2_tbasey(14)=0
       exch2_txglobalo(14)=113
       exch2_tyglobalo(14)=1
       exch2_isWedge(14)=0
       exch2_isNedge(14)=0
       exch2_isEedge(14)=1
       exch2_isSedge(14)=1
       exch2_myFace(14)=4
       exch2_mydnx(14)=32
       exch2_mydny(14)=32
       exch2_nNeighbours(14)=4

       exch2_neighbourId(1,14)=16
       exch2_pi(1,1,14)=1
       exch2_pi(2,1,14)=0
       exch2_pj(1,1,14)=0
       exch2_pj(2,1,14)=1
       exch2_oi(1,14)=0
       exch2_oi_f(1,14)=0
       exch2_oj(1,14)=0
       exch2_oj_f(1,14)=0
       exch2_itlo_c(1,14)=0
       exch2_ithi_c(1,14)=17
       exch2_jtlo_c(1,14)=0
       exch2_jthi_c(1,14)=0
       exch2_opposingSend_record(1,14)=2

       exch2_neighbourId(2,14)=6
       exch2_pi(1,2,14)=0
       exch2_pi(2,2,14)=-1
       exch2_pj(1,2,14)=1
       exch2_pj(2,2,14)=0
       exch2_oi(2,14)=33
       exch2_oi_f(2,14)=34
       exch2_oj(2,14)=-32
       exch2_oj_f(2,14)=-32
       exch2_itlo_c(2,14)=17
       exch2_ithi_c(2,14)=17
       exch2_jtlo_c(2,14)=0
       exch2_jthi_c(2,14)=17
       exch2_opposingSend_record(2,14)=3

       exch2_neighbourId(3,14)=22
       exch2_pi(1,3,14)=0
       exch2_pi(2,3,14)=1
       exch2_pj(1,3,14)=-1
       exch2_pj(2,3,14)=0
       exch2_oi(3,14)=32
       exch2_oi_f(3,14)=32
       exch2_oj(3,14)=33
       exch2_oj_f(3,14)=34
       exch2_itlo_c(3,14)=0
       exch2_ithi_c(3,14)=17
       exch2_jtlo_c(3,14)=0
       exch2_jthi_c(3,14)=0
       exch2_opposingSend_record(3,14)=2

       exch2_neighbourId(4,14)=13
       exch2_pi(1,4,14)=1
       exch2_pi(2,4,14)=0
       exch2_pj(1,4,14)=0
       exch2_pj(2,4,14)=1
       exch2_oi(4,14)=0
       exch2_oi_f(4,14)=0
       exch2_oj(4,14)=0
       exch2_oj_f(4,14)=0
       exch2_itlo_c(4,14)=17
       exch2_ithi_c(4,14)=17
       exch2_jtlo_c(4,14)=0
       exch2_jthi_c(4,14)=17
       exch2_opposingSend_record(4,14)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_15
C      Topology for tile 15
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(15)=16
       exch2_tny(15)=16
       exch2_tbasex(15)=0
       exch2_tbasey(15)=16
       exch2_txglobalo(15)=97
       exch2_tyglobalo(15)=17
       exch2_isWedge(15)=1
       exch2_isNedge(15)=1
       exch2_isEedge(15)=0
       exch2_isSedge(15)=0
       exch2_myFace(15)=4
       exch2_mydnx(15)=32
       exch2_mydny(15)=32
       exch2_nNeighbours(15)=4

       exch2_neighbourId(1,15)=17
       exch2_pi(1,1,15)=1
       exch2_pi(2,1,15)=0
       exch2_pj(1,1,15)=0
       exch2_pj(2,1,15)=1
       exch2_oi(1,15)=0
       exch2_oi_f(1,15)=0
       exch2_oj(1,15)=32
       exch2_oj_f(1,15)=32
       exch2_itlo_c(1,15)=0
       exch2_ithi_c(1,15)=17
       exch2_jtlo_c(1,15)=0
       exch2_jthi_c(1,15)=0
       exch2_opposingSend_record(1,15)=2

       exch2_neighbourId(2,15)=13
       exch2_pi(1,2,15)=1
       exch2_pi(2,2,15)=0
       exch2_pj(1,2,15)=0
       exch2_pj(2,2,15)=1
       exch2_oi(2,15)=0
       exch2_oi_f(2,15)=0
       exch2_oj(2,15)=0
       exch2_oj_f(2,15)=0
       exch2_itlo_c(2,15)=1
       exch2_ithi_c(2,15)=16
       exch2_jtlo_c(2,15)=17
       exch2_jthi_c(2,15)=17
       exch2_opposingSend_record(2,15)=1

       exch2_neighbourId(3,15)=16
       exch2_pi(1,3,15)=1
       exch2_pi(2,3,15)=0
       exch2_pj(1,3,15)=0
       exch2_pj(2,3,15)=1
       exch2_oi(3,15)=0
       exch2_oi_f(3,15)=0
       exch2_oj(3,15)=0
       exch2_oj_f(3,15)=0
       exch2_itlo_c(3,15)=0
       exch2_ithi_c(3,15)=0
       exch2_jtlo_c(3,15)=0
       exch2_jthi_c(3,15)=17
       exch2_opposingSend_record(3,15)=4

       exch2_neighbourId(4,15)=12
       exch2_pi(1,4,15)=1
       exch2_pi(2,4,15)=0
       exch2_pj(1,4,15)=0
       exch2_pj(2,4,15)=1
       exch2_oi(4,15)=-32
       exch2_oi_f(4,15)=-32
       exch2_oj(4,15)=0
       exch2_oj_f(4,15)=0
       exch2_itlo_c(4,15)=17
       exch2_ithi_c(4,15)=17
       exch2_jtlo_c(4,15)=0
       exch2_jthi_c(4,15)=17
       exch2_opposingSend_record(4,15)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_16
C      Topology for tile 16
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(16)=16
       exch2_tny(16)=16
       exch2_tbasex(16)=16
       exch2_tbasey(16)=16
       exch2_txglobalo(16)=113
       exch2_tyglobalo(16)=17
       exch2_isWedge(16)=0
       exch2_isNedge(16)=1
       exch2_isEedge(16)=1
       exch2_isSedge(16)=0
       exch2_myFace(16)=4
       exch2_mydnx(16)=32
       exch2_mydny(16)=32
       exch2_nNeighbours(16)=4

       exch2_neighbourId(1,16)=18
       exch2_pi(1,1,16)=1
       exch2_pi(2,1,16)=0
       exch2_pj(1,1,16)=0
       exch2_pj(2,1,16)=1
       exch2_oi(1,16)=0
       exch2_oi_f(1,16)=0
       exch2_oj(1,16)=32
       exch2_oj_f(1,16)=32
       exch2_itlo_c(1,16)=0
       exch2_ithi_c(1,16)=17
       exch2_jtlo_c(1,16)=0
       exch2_jthi_c(1,16)=0
       exch2_opposingSend_record(1,16)=2

       exch2_neighbourId(2,16)=14
       exch2_pi(1,2,16)=1
       exch2_pi(2,2,16)=0
       exch2_pj(1,2,16)=0
       exch2_pj(2,2,16)=1
       exch2_oi(2,16)=0
       exch2_oi_f(2,16)=0
       exch2_oj(2,16)=0
       exch2_oj_f(2,16)=0
       exch2_itlo_c(2,16)=1
       exch2_ithi_c(2,16)=16
       exch2_jtlo_c(2,16)=17
       exch2_jthi_c(2,16)=17
       exch2_opposingSend_record(2,16)=1

       exch2_neighbourId(3,16)=21
       exch2_pi(1,3,16)=0
       exch2_pi(2,3,16)=1
       exch2_pj(1,3,16)=-1
       exch2_pj(2,3,16)=0
       exch2_oi(3,16)=32
       exch2_oi_f(3,16)=32
       exch2_oj(3,16)=33
       exch2_oj_f(3,16)=34
       exch2_itlo_c(3,16)=0
       exch2_ithi_c(3,16)=17
       exch2_jtlo_c(3,16)=0
       exch2_jthi_c(3,16)=0
       exch2_opposingSend_record(3,16)=2

       exch2_neighbourId(4,16)=15
       exch2_pi(1,4,16)=1
       exch2_pi(2,4,16)=0
       exch2_pj(1,4,16)=0
       exch2_pj(2,4,16)=1
       exch2_oi(4,16)=0
       exch2_oi_f(4,16)=0
       exch2_oj(4,16)=0
       exch2_oj_f(4,16)=0
       exch2_itlo_c(4,16)=17
       exch2_ithi_c(4,16)=17
       exch2_jtlo_c(4,16)=0
       exch2_jthi_c(4,16)=17
       exch2_opposingSend_record(4,16)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_17
C      Topology for tile 17
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(17)=16
       exch2_tny(17)=16
       exch2_tbasex(17)=0
       exch2_tbasey(17)=0
       exch2_txglobalo(17)=129
       exch2_tyglobalo(17)=1
       exch2_isWedge(17)=1
       exch2_isNedge(17)=0
       exch2_isEedge(17)=0
       exch2_isSedge(17)=1
       exch2_myFace(17)=5
       exch2_mydnx(17)=32
       exch2_mydny(17)=32
       exch2_nNeighbours(17)=4

       exch2_neighbourId(1,17)=19
       exch2_pi(1,1,17)=1
       exch2_pi(2,1,17)=0
       exch2_pj(1,1,17)=0
       exch2_pj(2,1,17)=1
       exch2_oi(1,17)=0
       exch2_oi_f(1,17)=0
       exch2_oj(1,17)=0
       exch2_oj_f(1,17)=0
       exch2_itlo_c(1,17)=0
       exch2_ithi_c(1,17)=17
       exch2_jtlo_c(1,17)=0
       exch2_jthi_c(1,17)=0
       exch2_opposingSend_record(1,17)=2

       exch2_neighbourId(2,17)=15
       exch2_pi(1,2,17)=1
       exch2_pi(2,2,17)=0
       exch2_pj(1,2,17)=0
       exch2_pj(2,2,17)=1
       exch2_oi(2,17)=0
       exch2_oi_f(2,17)=0
       exch2_oj(2,17)=-32
       exch2_oj_f(2,17)=-32
       exch2_itlo_c(2,17)=0
       exch2_ithi_c(2,17)=17
       exch2_jtlo_c(2,17)=17
       exch2_jthi_c(2,17)=17
       exch2_opposingSend_record(2,17)=1

       exch2_neighbourId(3,17)=18
       exch2_pi(1,3,17)=1
       exch2_pi(2,3,17)=0
       exch2_pj(1,3,17)=0
       exch2_pj(2,3,17)=1
       exch2_oi(3,17)=0
       exch2_oi_f(3,17)=0
       exch2_oj(3,17)=0
       exch2_oj_f(3,17)=0
       exch2_itlo_c(3,17)=0
       exch2_ithi_c(3,17)=0
       exch2_jtlo_c(3,17)=0
       exch2_jthi_c(3,17)=17
       exch2_opposingSend_record(3,17)=4

       exch2_neighbourId(4,17)=12
       exch2_pi(1,4,17)=0
       exch2_pi(2,4,17)=1
       exch2_pj(1,4,17)=-1
       exch2_pj(2,4,17)=0
       exch2_oi(4,17)=-32
       exch2_oi_f(4,17)=-32
       exch2_oj(4,17)=33
       exch2_oj_f(4,17)=34
       exch2_itlo_c(4,17)=0
       exch2_ithi_c(4,17)=17
       exch2_jtlo_c(4,17)=17
       exch2_jthi_c(4,17)=17
       exch2_opposingSend_record(4,17)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_18
C      Topology for tile 18
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(18)=16
       exch2_tny(18)=16
       exch2_tbasex(18)=16
       exch2_tbasey(18)=0
       exch2_txglobalo(18)=145
       exch2_tyglobalo(18)=1
       exch2_isWedge(18)=0
       exch2_isNedge(18)=0
       exch2_isEedge(18)=1
       exch2_isSedge(18)=1
       exch2_myFace(18)=5
       exch2_mydnx(18)=32
       exch2_mydny(18)=32
       exch2_nNeighbours(18)=4

       exch2_neighbourId(1,18)=20
       exch2_pi(1,1,18)=1
       exch2_pi(2,1,18)=0
       exch2_pj(1,1,18)=0
       exch2_pj(2,1,18)=1
       exch2_oi(1,18)=0
       exch2_oi_f(1,18)=0
       exch2_oj(1,18)=0
       exch2_oj_f(1,18)=0
       exch2_itlo_c(1,18)=0
       exch2_ithi_c(1,18)=17
       exch2_jtlo_c(1,18)=0
       exch2_jthi_c(1,18)=0
       exch2_opposingSend_record(1,18)=2

       exch2_neighbourId(2,18)=16
       exch2_pi(1,2,18)=1
       exch2_pi(2,2,18)=0
       exch2_pj(1,2,18)=0
       exch2_pj(2,2,18)=1
       exch2_oi(2,18)=0
       exch2_oi_f(2,18)=0
       exch2_oj(2,18)=-32
       exch2_oj_f(2,18)=-32
       exch2_itlo_c(2,18)=0
       exch2_ithi_c(2,18)=17
       exch2_jtlo_c(2,18)=17
       exch2_jthi_c(2,18)=17
       exch2_opposingSend_record(2,18)=1

       exch2_neighbourId(3,18)=21
       exch2_pi(1,3,18)=1
       exch2_pi(2,3,18)=0
       exch2_pj(1,3,18)=0
       exch2_pj(2,3,18)=1
       exch2_oi(3,18)=32
       exch2_oi_f(3,18)=32
       exch2_oj(3,18)=0
       exch2_oj_f(3,18)=0
       exch2_itlo_c(3,18)=0
       exch2_ithi_c(3,18)=0
       exch2_jtlo_c(3,18)=0
       exch2_jthi_c(3,18)=17
       exch2_opposingSend_record(3,18)=4

       exch2_neighbourId(4,18)=17
       exch2_pi(1,4,18)=1
       exch2_pi(2,4,18)=0
       exch2_pj(1,4,18)=0
       exch2_pj(2,4,18)=1
       exch2_oi(4,18)=0
       exch2_oi_f(4,18)=0
       exch2_oj(4,18)=0
       exch2_oj_f(4,18)=0
       exch2_itlo_c(4,18)=17
       exch2_ithi_c(4,18)=17
       exch2_jtlo_c(4,18)=0
       exch2_jthi_c(4,18)=17
       exch2_opposingSend_record(4,18)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_19
C      Topology for tile 19
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(19)=16
       exch2_tny(19)=16
       exch2_tbasex(19)=0
       exch2_tbasey(19)=16
       exch2_txglobalo(19)=129
       exch2_tyglobalo(19)=17
       exch2_isWedge(19)=1
       exch2_isNedge(19)=1
       exch2_isEedge(19)=0
       exch2_isSedge(19)=0
       exch2_myFace(19)=5
       exch2_mydnx(19)=32
       exch2_mydny(19)=32
       exch2_nNeighbours(19)=4

       exch2_neighbourId(1,19)=3
       exch2_pi(1,1,19)=0
       exch2_pi(2,1,19)=-1
       exch2_pj(1,1,19)=1
       exch2_pj(2,1,19)=0
       exch2_oi(1,19)=33
       exch2_oi_f(1,19)=34
       exch2_oj(1,19)=32
       exch2_oj_f(1,19)=32
       exch2_itlo_c(1,19)=0
       exch2_ithi_c(1,19)=0
       exch2_jtlo_c(1,19)=0
       exch2_jthi_c(1,19)=17
       exch2_opposingSend_record(1,19)=4

       exch2_neighbourId(2,19)=17
       exch2_pi(1,2,19)=1
       exch2_pi(2,2,19)=0
       exch2_pj(1,2,19)=0
       exch2_pj(2,2,19)=1
       exch2_oi(2,19)=0
       exch2_oi_f(2,19)=0
       exch2_oj(2,19)=0
       exch2_oj_f(2,19)=0
       exch2_itlo_c(2,19)=1
       exch2_ithi_c(2,19)=16
       exch2_jtlo_c(2,19)=17
       exch2_jthi_c(2,19)=17
       exch2_opposingSend_record(2,19)=1

       exch2_neighbourId(3,19)=20
       exch2_pi(1,3,19)=1
       exch2_pi(2,3,19)=0
       exch2_pj(1,3,19)=0
       exch2_pj(2,3,19)=1
       exch2_oi(3,19)=0
       exch2_oi_f(3,19)=0
       exch2_oj(3,19)=0
       exch2_oj_f(3,19)=0
       exch2_itlo_c(3,19)=0
       exch2_ithi_c(3,19)=0
       exch2_jtlo_c(3,19)=0
       exch2_jthi_c(3,19)=17
       exch2_opposingSend_record(3,19)=4

       exch2_neighbourId(4,19)=11
       exch2_pi(1,4,19)=0
       exch2_pi(2,4,19)=1
       exch2_pj(1,4,19)=-1
       exch2_pj(2,4,19)=0
       exch2_oi(4,19)=-32
       exch2_oi_f(4,19)=-32
       exch2_oj(4,19)=33
       exch2_oj_f(4,19)=34
       exch2_itlo_c(4,19)=0
       exch2_ithi_c(4,19)=17
       exch2_jtlo_c(4,19)=17
       exch2_jthi_c(4,19)=17
       exch2_opposingSend_record(4,19)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_20
C      Topology for tile 20
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(20)=16
       exch2_tny(20)=16
       exch2_tbasex(20)=16
       exch2_tbasey(20)=16
       exch2_txglobalo(20)=145
       exch2_tyglobalo(20)=17
       exch2_isWedge(20)=0
       exch2_isNedge(20)=1
       exch2_isEedge(20)=1
       exch2_isSedge(20)=0
       exch2_myFace(20)=5
       exch2_mydnx(20)=32
       exch2_mydny(20)=32
       exch2_nNeighbours(20)=4

       exch2_neighbourId(1,20)=1
       exch2_pi(1,1,20)=0
       exch2_pi(2,1,20)=-1
       exch2_pj(1,1,20)=1
       exch2_pj(2,1,20)=0
       exch2_oi(1,20)=33
       exch2_oi_f(1,20)=34
       exch2_oj(1,20)=32
       exch2_oj_f(1,20)=32
       exch2_itlo_c(1,20)=0
       exch2_ithi_c(1,20)=0
       exch2_jtlo_c(1,20)=0
       exch2_jthi_c(1,20)=17
       exch2_opposingSend_record(1,20)=4

       exch2_neighbourId(2,20)=18
       exch2_pi(1,2,20)=1
       exch2_pi(2,2,20)=0
       exch2_pj(1,2,20)=0
       exch2_pj(2,2,20)=1
       exch2_oi(2,20)=0
       exch2_oi_f(2,20)=0
       exch2_oj(2,20)=0
       exch2_oj_f(2,20)=0
       exch2_itlo_c(2,20)=1
       exch2_ithi_c(2,20)=16
       exch2_jtlo_c(2,20)=17
       exch2_jthi_c(2,20)=17
       exch2_opposingSend_record(2,20)=1

       exch2_neighbourId(3,20)=23
       exch2_pi(1,3,20)=1
       exch2_pi(2,3,20)=0
       exch2_pj(1,3,20)=0
       exch2_pj(2,3,20)=1
       exch2_oi(3,20)=32
       exch2_oi_f(3,20)=32
       exch2_oj(3,20)=0
       exch2_oj_f(3,20)=0
       exch2_itlo_c(3,20)=0
       exch2_ithi_c(3,20)=0
       exch2_jtlo_c(3,20)=0
       exch2_jthi_c(3,20)=17
       exch2_opposingSend_record(3,20)=4

       exch2_neighbourId(4,20)=19
       exch2_pi(1,4,20)=1
       exch2_pi(2,4,20)=0
       exch2_pj(1,4,20)=0
       exch2_pj(2,4,20)=1
       exch2_oi(4,20)=0
       exch2_oi_f(4,20)=0
       exch2_oj(4,20)=0
       exch2_oj_f(4,20)=0
       exch2_itlo_c(4,20)=17
       exch2_ithi_c(4,20)=17
       exch2_jtlo_c(4,20)=0
       exch2_jthi_c(4,20)=17
       exch2_opposingSend_record(4,20)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_21
C      Topology for tile 21
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(21)=16
       exch2_tny(21)=16
       exch2_tbasex(21)=0
       exch2_tbasey(21)=0
       exch2_txglobalo(21)=161
       exch2_tyglobalo(21)=1
       exch2_isWedge(21)=1
       exch2_isNedge(21)=0
       exch2_isEedge(21)=0
       exch2_isSedge(21)=1
       exch2_myFace(21)=6
       exch2_mydnx(21)=32
       exch2_mydny(21)=32
       exch2_nNeighbours(21)=4

       exch2_neighbourId(1,21)=23
       exch2_pi(1,1,21)=1
       exch2_pi(2,1,21)=0
       exch2_pj(1,1,21)=0
       exch2_pj(2,1,21)=1
       exch2_oi(1,21)=0
       exch2_oi_f(1,21)=0
       exch2_oj(1,21)=0
       exch2_oj_f(1,21)=0
       exch2_itlo_c(1,21)=0
       exch2_ithi_c(1,21)=17
       exch2_jtlo_c(1,21)=0
       exch2_jthi_c(1,21)=0
       exch2_opposingSend_record(1,21)=2

       exch2_neighbourId(2,21)=16
       exch2_pi(1,2,21)=0
       exch2_pi(2,2,21)=-1
       exch2_pj(1,2,21)=1
       exch2_pj(2,2,21)=0
       exch2_oi(2,21)=33
       exch2_oi_f(2,21)=34
       exch2_oj(2,21)=-32
       exch2_oj_f(2,21)=-32
       exch2_itlo_c(2,21)=17
       exch2_ithi_c(2,21)=17
       exch2_jtlo_c(2,21)=0
       exch2_jthi_c(2,21)=17
       exch2_opposingSend_record(2,21)=3

       exch2_neighbourId(3,21)=22
       exch2_pi(1,3,21)=1
       exch2_pi(2,3,21)=0
       exch2_pj(1,3,21)=0
       exch2_pj(2,3,21)=1
       exch2_oi(3,21)=0
       exch2_oi_f(3,21)=0
       exch2_oj(3,21)=0
       exch2_oj_f(3,21)=0
       exch2_itlo_c(3,21)=0
       exch2_ithi_c(3,21)=0
       exch2_jtlo_c(3,21)=0
       exch2_jthi_c(3,21)=17
       exch2_opposingSend_record(3,21)=4

       exch2_neighbourId(4,21)=18
       exch2_pi(1,4,21)=1
       exch2_pi(2,4,21)=0
       exch2_pj(1,4,21)=0
       exch2_pj(2,4,21)=1
       exch2_oi(4,21)=-32
       exch2_oi_f(4,21)=-32
       exch2_oj(4,21)=0
       exch2_oj_f(4,21)=0
       exch2_itlo_c(4,21)=17
       exch2_ithi_c(4,21)=17
       exch2_jtlo_c(4,21)=0
       exch2_jthi_c(4,21)=17
       exch2_opposingSend_record(4,21)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_22
C      Topology for tile 22
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(22)=16
       exch2_tny(22)=16
       exch2_tbasex(22)=16
       exch2_tbasey(22)=0
       exch2_txglobalo(22)=177
       exch2_tyglobalo(22)=1
       exch2_isWedge(22)=0
       exch2_isNedge(22)=0
       exch2_isEedge(22)=1
       exch2_isSedge(22)=1
       exch2_myFace(22)=6
       exch2_mydnx(22)=32
       exch2_mydny(22)=32
       exch2_nNeighbours(22)=4

       exch2_neighbourId(1,22)=24
       exch2_pi(1,1,22)=1
       exch2_pi(2,1,22)=0
       exch2_pj(1,1,22)=0
       exch2_pj(2,1,22)=1
       exch2_oi(1,22)=0
       exch2_oi_f(1,22)=0
       exch2_oj(1,22)=0
       exch2_oj_f(1,22)=0
       exch2_itlo_c(1,22)=0
       exch2_ithi_c(1,22)=17
       exch2_jtlo_c(1,22)=0
       exch2_jthi_c(1,22)=0
       exch2_opposingSend_record(1,22)=2

       exch2_neighbourId(2,22)=14
       exch2_pi(1,2,22)=0
       exch2_pi(2,2,22)=-1
       exch2_pj(1,2,22)=1
       exch2_pj(2,2,22)=0
       exch2_oi(2,22)=33
       exch2_oi_f(2,22)=34
       exch2_oj(2,22)=-32
       exch2_oj_f(2,22)=-32
       exch2_itlo_c(2,22)=17
       exch2_ithi_c(2,22)=17
       exch2_jtlo_c(2,22)=0
       exch2_jthi_c(2,22)=17
       exch2_opposingSend_record(2,22)=3

       exch2_neighbourId(3,22)=6
       exch2_pi(1,3,22)=0
       exch2_pi(2,3,22)=1
       exch2_pj(1,3,22)=-1
       exch2_pj(2,3,22)=0
       exch2_oi(3,22)=32
       exch2_oi_f(3,22)=32
       exch2_oj(3,22)=33
       exch2_oj_f(3,22)=34
       exch2_itlo_c(3,22)=0
       exch2_ithi_c(3,22)=17
       exch2_jtlo_c(3,22)=0
       exch2_jthi_c(3,22)=0
       exch2_opposingSend_record(3,22)=2

       exch2_neighbourId(4,22)=21
       exch2_pi(1,4,22)=1
       exch2_pi(2,4,22)=0
       exch2_pj(1,4,22)=0
       exch2_pj(2,4,22)=1
       exch2_oi(4,22)=0
       exch2_oi_f(4,22)=0
       exch2_oj(4,22)=0
       exch2_oj_f(4,22)=0
       exch2_itlo_c(4,22)=17
       exch2_ithi_c(4,22)=17
       exch2_jtlo_c(4,22)=0
       exch2_jthi_c(4,22)=17
       exch2_opposingSend_record(4,22)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_23
C      Topology for tile 23
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(23)=16
       exch2_tny(23)=16
       exch2_tbasex(23)=0
       exch2_tbasey(23)=16
       exch2_txglobalo(23)=161
       exch2_tyglobalo(23)=17
       exch2_isWedge(23)=1
       exch2_isNedge(23)=1
       exch2_isEedge(23)=0
       exch2_isSedge(23)=0
       exch2_myFace(23)=6
       exch2_mydnx(23)=32
       exch2_mydny(23)=32
       exch2_nNeighbours(23)=4

       exch2_neighbourId(1,23)=1
       exch2_pi(1,1,23)=1
       exch2_pi(2,1,23)=0
       exch2_pj(1,1,23)=0
       exch2_pj(2,1,23)=1
       exch2_oi(1,23)=0
       exch2_oi_f(1,23)=0
       exch2_oj(1,23)=32
       exch2_oj_f(1,23)=32
       exch2_itlo_c(1,23)=0
       exch2_ithi_c(1,23)=17
       exch2_jtlo_c(1,23)=0
       exch2_jthi_c(1,23)=0
       exch2_opposingSend_record(1,23)=2

       exch2_neighbourId(2,23)=21
       exch2_pi(1,2,23)=1
       exch2_pi(2,2,23)=0
       exch2_pj(1,2,23)=0
       exch2_pj(2,2,23)=1
       exch2_oi(2,23)=0
       exch2_oi_f(2,23)=0
       exch2_oj(2,23)=0
       exch2_oj_f(2,23)=0
       exch2_itlo_c(2,23)=1
       exch2_ithi_c(2,23)=16
       exch2_jtlo_c(2,23)=17
       exch2_jthi_c(2,23)=17
       exch2_opposingSend_record(2,23)=1

       exch2_neighbourId(3,23)=24
       exch2_pi(1,3,23)=1
       exch2_pi(2,3,23)=0
       exch2_pj(1,3,23)=0
       exch2_pj(2,3,23)=1
       exch2_oi(3,23)=0
       exch2_oi_f(3,23)=0
       exch2_oj(3,23)=0
       exch2_oj_f(3,23)=0
       exch2_itlo_c(3,23)=0
       exch2_ithi_c(3,23)=0
       exch2_jtlo_c(3,23)=0
       exch2_jthi_c(3,23)=17
       exch2_opposingSend_record(3,23)=4

       exch2_neighbourId(4,23)=20
       exch2_pi(1,4,23)=1
       exch2_pi(2,4,23)=0
       exch2_pj(1,4,23)=0
       exch2_pj(2,4,23)=1
       exch2_oi(4,23)=-32
       exch2_oi_f(4,23)=-32
       exch2_oj(4,23)=0
       exch2_oj_f(4,23)=0
       exch2_itlo_c(4,23)=17
       exch2_ithi_c(4,23)=17
       exch2_jtlo_c(4,23)=0
       exch2_jthi_c(4,23)=17
       exch2_opposingSend_record(4,23)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_24
C      Topology for tile 24
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(24)=16
       exch2_tny(24)=16
       exch2_tbasex(24)=16
       exch2_tbasey(24)=16
       exch2_txglobalo(24)=177
       exch2_tyglobalo(24)=17
       exch2_isWedge(24)=0
       exch2_isNedge(24)=1
       exch2_isEedge(24)=1
       exch2_isSedge(24)=0
       exch2_myFace(24)=6
       exch2_mydnx(24)=32
       exch2_mydny(24)=32
       exch2_nNeighbours(24)=4

       exch2_neighbourId(1,24)=2
       exch2_pi(1,1,24)=1
       exch2_pi(2,1,24)=0
       exch2_pj(1,1,24)=0
       exch2_pj(2,1,24)=1
       exch2_oi(1,24)=0
       exch2_oi_f(1,24)=0
       exch2_oj(1,24)=32
       exch2_oj_f(1,24)=32
       exch2_itlo_c(1,24)=0
       exch2_ithi_c(1,24)=17
       exch2_jtlo_c(1,24)=0
       exch2_jthi_c(1,24)=0
       exch2_opposingSend_record(1,24)=2

       exch2_neighbourId(2,24)=22
       exch2_pi(1,2,24)=1
       exch2_pi(2,2,24)=0
       exch2_pj(1,2,24)=0
       exch2_pj(2,2,24)=1
       exch2_oi(2,24)=0
       exch2_oi_f(2,24)=0
       exch2_oj(2,24)=0
       exch2_oj_f(2,24)=0
       exch2_itlo_c(2,24)=1
       exch2_ithi_c(2,24)=16
       exch2_jtlo_c(2,24)=17
       exch2_jthi_c(2,24)=17
       exch2_opposingSend_record(2,24)=1

       exch2_neighbourId(3,24)=5
       exch2_pi(1,3,24)=0
       exch2_pi(2,3,24)=1
       exch2_pj(1,3,24)=-1
       exch2_pj(2,3,24)=0
       exch2_oi(3,24)=32
       exch2_oi_f(3,24)=32
       exch2_oj(3,24)=33
       exch2_oj_f(3,24)=34
       exch2_itlo_c(3,24)=0
       exch2_ithi_c(3,24)=17
       exch2_jtlo_c(3,24)=0
       exch2_jthi_c(3,24)=0
       exch2_opposingSend_record(3,24)=2

       exch2_neighbourId(4,24)=23
       exch2_pi(1,4,24)=1
       exch2_pi(2,4,24)=0
       exch2_pj(1,4,24)=0
       exch2_pj(2,4,24)=1
       exch2_oi(4,24)=0
       exch2_oi_f(4,24)=0
       exch2_oj(4,24)=0
       exch2_oj_f(4,24)=0
       exch2_itlo_c(4,24)=17
       exch2_ithi_c(4,24)=17
       exch2_jtlo_c(4,24)=0
       exch2_jthi_c(4,24)=17
       exch2_opposingSend_record(4,24)=3
       RETURN
       END
                                                  
