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
       RETURN
       END
       SUBROUTINE W2_E2SETUP_1
C      Topology for tile 1
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(1)=32
       exch2_tny(1)=16
       exch2_tbasex(1)=0
       exch2_tbasey(1)=0
       exch2_txglobalo(1)=1
       exch2_tyglobalo(1)=1
       exch2_isWedge(1)=1
       exch2_isNedge(1)=0
       exch2_isEedge(1)=1
       exch2_isSedge(1)=1
       exch2_myFace(1)=1
       exch2_mydnx(1)=32
       exch2_mydny(1)=32
       exch2_nNeighbours(1)=4

       exch2_neighbourId(1,1)=2
       exch2_pi(1,1,1)=1
       exch2_pi(2,1,1)=0
       exch2_pj(1,1,1)=0
       exch2_pj(2,1,1)=1
       exch2_oi(1,1)=0
       exch2_oi_f(1,1)=0
       exch2_oj(1,1)=0
       exch2_oj_f(1,1)=0
       exch2_itlo_c(1,1)=0
       exch2_ithi_c(1,1)=33
       exch2_jtlo_c(1,1)=0
       exch2_jthi_c(1,1)=0
       exch2_opposingSend_record(1,1)=3

       exch2_neighbourId(2,1)=12
       exch2_pi(1,2,1)=1
       exch2_pi(2,2,1)=0
       exch2_pj(1,2,1)=0
       exch2_pj(2,2,1)=1
       exch2_oi(2,1)=0
       exch2_oi_f(2,1)=0
       exch2_oj(2,1)=-32
       exch2_oj_f(2,1)=-32
       exch2_itlo_c(2,1)=0
       exch2_ithi_c(2,1)=33
       exch2_jtlo_c(2,1)=17
       exch2_jthi_c(2,1)=17
       exch2_opposingSend_record(2,1)=1

       exch2_neighbourId(3,1)=3
       exch2_pi(1,3,1)=1
       exch2_pi(2,3,1)=0
       exch2_pj(1,3,1)=0
       exch2_pj(2,3,1)=1
       exch2_oi(3,1)=32
       exch2_oi_f(3,1)=32
       exch2_oj(3,1)=0
       exch2_oj_f(3,1)=0
       exch2_itlo_c(3,1)=0
       exch2_ithi_c(3,1)=0
       exch2_jtlo_c(3,1)=0
       exch2_jthi_c(3,1)=17
       exch2_opposingSend_record(3,1)=5

       exch2_neighbourId(4,1)=10
       exch2_pi(1,4,1)=0
       exch2_pi(2,4,1)=1
       exch2_pj(1,4,1)=-1
       exch2_pj(2,4,1)=0
       exch2_oi(4,1)=-32
       exch2_oi_f(4,1)=-32
       exch2_oj(4,1)=33
       exch2_oj_f(4,1)=34
       exch2_itlo_c(4,1)=16
       exch2_ithi_c(4,1)=33
       exch2_jtlo_c(4,1)=17
       exch2_jthi_c(4,1)=17
       exch2_opposingSend_record(4,1)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_2
C      Topology for tile 2
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(2)=32
       exch2_tny(2)=16
       exch2_tbasex(2)=0
       exch2_tbasey(2)=16
       exch2_txglobalo(2)=1
       exch2_tyglobalo(2)=17
       exch2_isWedge(2)=1
       exch2_isNedge(2)=1
       exch2_isEedge(2)=1
       exch2_isSedge(2)=0
       exch2_myFace(2)=1
       exch2_mydnx(2)=32
       exch2_mydny(2)=32
       exch2_nNeighbours(2)=5

       exch2_neighbourId(1,2)=5
       exch2_pi(1,1,2)=0
       exch2_pi(2,1,2)=-1
       exch2_pj(1,1,2)=1
       exch2_pj(2,1,2)=0
       exch2_oi(1,2)=33
       exch2_oi_f(1,2)=34
       exch2_oj(1,2)=32
       exch2_oj_f(1,2)=32
       exch2_itlo_c(1,2)=0
       exch2_ithi_c(1,2)=0
       exch2_jtlo_c(1,2)=0
       exch2_jthi_c(1,2)=17
       exch2_opposingSend_record(1,2)=4

       exch2_neighbourId(2,2)=6
       exch2_pi(1,2,2)=0
       exch2_pi(2,2,2)=-1
       exch2_pj(1,2,2)=1
       exch2_pj(2,2,2)=0
       exch2_oi(2,2)=33
       exch2_oi_f(2,2)=34
       exch2_oj(2,2)=32
       exch2_oj_f(2,2)=32
       exch2_itlo_c(2,2)=0
       exch2_ithi_c(2,2)=0
       exch2_jtlo_c(2,2)=0
       exch2_jthi_c(2,2)=17
       exch2_opposingSend_record(2,2)=5

       exch2_neighbourId(3,2)=1
       exch2_pi(1,3,2)=1
       exch2_pi(2,3,2)=0
       exch2_pj(1,3,2)=0
       exch2_pj(2,3,2)=1
       exch2_oi(3,2)=0
       exch2_oi_f(3,2)=0
       exch2_oj(3,2)=0
       exch2_oj_f(3,2)=0
       exch2_itlo_c(3,2)=1
       exch2_ithi_c(3,2)=32
       exch2_jtlo_c(3,2)=17
       exch2_jthi_c(3,2)=17
       exch2_opposingSend_record(3,2)=1

       exch2_neighbourId(4,2)=4
       exch2_pi(1,4,2)=1
       exch2_pi(2,4,2)=0
       exch2_pj(1,4,2)=0
       exch2_pj(2,4,2)=1
       exch2_oi(4,2)=32
       exch2_oi_f(4,2)=32
       exch2_oj(4,2)=0
       exch2_oj_f(4,2)=0
       exch2_itlo_c(4,2)=0
       exch2_ithi_c(4,2)=0
       exch2_jtlo_c(4,2)=0
       exch2_jthi_c(4,2)=17
       exch2_opposingSend_record(4,2)=4

       exch2_neighbourId(5,2)=10
       exch2_pi(1,5,2)=0
       exch2_pi(2,5,2)=1
       exch2_pj(1,5,2)=-1
       exch2_pj(2,5,2)=0
       exch2_oi(5,2)=-32
       exch2_oi_f(5,2)=-32
       exch2_oj(5,2)=33
       exch2_oj_f(5,2)=34
       exch2_itlo_c(5,2)=0
       exch2_ithi_c(5,2)=17
       exch2_jtlo_c(5,2)=17
       exch2_jthi_c(5,2)=17
       exch2_opposingSend_record(5,2)=2
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_3
C      Topology for tile 3
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(3)=32
       exch2_tny(3)=16
       exch2_tbasex(3)=0
       exch2_tbasey(3)=0
       exch2_txglobalo(3)=33
       exch2_tyglobalo(3)=1
       exch2_isWedge(3)=1
       exch2_isNedge(3)=0
       exch2_isEedge(3)=1
       exch2_isSedge(3)=1
       exch2_myFace(3)=2
       exch2_mydnx(3)=32
       exch2_mydny(3)=32
       exch2_nNeighbours(3)=5

       exch2_neighbourId(1,3)=4
       exch2_pi(1,1,3)=1
       exch2_pi(2,1,3)=0
       exch2_pj(1,1,3)=0
       exch2_pj(2,1,3)=1
       exch2_oi(1,3)=0
       exch2_oi_f(1,3)=0
       exch2_oj(1,3)=0
       exch2_oj_f(1,3)=0
       exch2_itlo_c(1,3)=0
       exch2_ithi_c(1,3)=33
       exch2_jtlo_c(1,3)=0
       exch2_jthi_c(1,3)=0
       exch2_opposingSend_record(1,3)=2

       exch2_neighbourId(2,3)=11
       exch2_pi(1,2,3)=0
       exch2_pi(2,2,3)=-1
       exch2_pj(1,2,3)=1
       exch2_pj(2,2,3)=0
       exch2_oi(2,3)=33
       exch2_oi_f(2,3)=34
       exch2_oj(2,3)=-32
       exch2_oj_f(2,3)=-32
       exch2_itlo_c(2,3)=33
       exch2_ithi_c(2,3)=33
       exch2_jtlo_c(2,3)=0
       exch2_jthi_c(2,3)=17
       exch2_opposingSend_record(2,3)=4

       exch2_neighbourId(3,3)=12
       exch2_pi(1,3,3)=0
       exch2_pi(2,3,3)=-1
       exch2_pj(1,3,3)=1
       exch2_pj(2,3,3)=0
       exch2_oi(3,3)=33
       exch2_oi_f(3,3)=34
       exch2_oj(3,3)=-32
       exch2_oj_f(3,3)=-32
       exch2_itlo_c(3,3)=33
       exch2_ithi_c(3,3)=33
       exch2_jtlo_c(3,3)=0
       exch2_jthi_c(3,3)=17
       exch2_opposingSend_record(3,3)=3

       exch2_neighbourId(4,3)=7
       exch2_pi(1,4,3)=0
       exch2_pi(2,4,3)=1
       exch2_pj(1,4,3)=-1
       exch2_pj(2,4,3)=0
       exch2_oi(4,3)=32
       exch2_oi_f(4,3)=32
       exch2_oj(4,3)=33
       exch2_oj_f(4,3)=34
       exch2_itlo_c(4,3)=16
       exch2_ithi_c(4,3)=33
       exch2_jtlo_c(4,3)=0
       exch2_jthi_c(4,3)=0
       exch2_opposingSend_record(4,3)=2

       exch2_neighbourId(5,3)=1
       exch2_pi(1,5,3)=1
       exch2_pi(2,5,3)=0
       exch2_pj(1,5,3)=0
       exch2_pj(2,5,3)=1
       exch2_oi(5,3)=-32
       exch2_oi_f(5,3)=-32
       exch2_oj(5,3)=0
       exch2_oj_f(5,3)=0
       exch2_itlo_c(5,3)=33
       exch2_ithi_c(5,3)=33
       exch2_jtlo_c(5,3)=0
       exch2_jthi_c(5,3)=17
       exch2_opposingSend_record(5,3)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_4
C      Topology for tile 4
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(4)=32
       exch2_tny(4)=16
       exch2_tbasex(4)=0
       exch2_tbasey(4)=16
       exch2_txglobalo(4)=33
       exch2_tyglobalo(4)=17
       exch2_isWedge(4)=1
       exch2_isNedge(4)=1
       exch2_isEedge(4)=1
       exch2_isSedge(4)=0
       exch2_myFace(4)=2
       exch2_mydnx(4)=32
       exch2_mydny(4)=32
       exch2_nNeighbours(4)=4

       exch2_neighbourId(1,4)=5
       exch2_pi(1,1,4)=1
       exch2_pi(2,1,4)=0
       exch2_pj(1,1,4)=0
       exch2_pj(2,1,4)=1
       exch2_oi(1,4)=0
       exch2_oi_f(1,4)=0
       exch2_oj(1,4)=32
       exch2_oj_f(1,4)=32
       exch2_itlo_c(1,4)=0
       exch2_ithi_c(1,4)=33
       exch2_jtlo_c(1,4)=0
       exch2_jthi_c(1,4)=0
       exch2_opposingSend_record(1,4)=2

       exch2_neighbourId(2,4)=3
       exch2_pi(1,2,4)=1
       exch2_pi(2,2,4)=0
       exch2_pj(1,2,4)=0
       exch2_pj(2,2,4)=1
       exch2_oi(2,4)=0
       exch2_oi_f(2,4)=0
       exch2_oj(2,4)=0
       exch2_oj_f(2,4)=0
       exch2_itlo_c(2,4)=1
       exch2_ithi_c(2,4)=32
       exch2_jtlo_c(2,4)=17
       exch2_jthi_c(2,4)=17
       exch2_opposingSend_record(2,4)=1

       exch2_neighbourId(3,4)=7
       exch2_pi(1,3,4)=0
       exch2_pi(2,3,4)=1
       exch2_pj(1,3,4)=-1
       exch2_pj(2,3,4)=0
       exch2_oi(3,4)=32
       exch2_oi_f(3,4)=32
       exch2_oj(3,4)=33
       exch2_oj_f(3,4)=34
       exch2_itlo_c(3,4)=0
       exch2_ithi_c(3,4)=17
       exch2_jtlo_c(3,4)=0
       exch2_jthi_c(3,4)=0
       exch2_opposingSend_record(3,4)=3

       exch2_neighbourId(4,4)=2
       exch2_pi(1,4,4)=1
       exch2_pi(2,4,4)=0
       exch2_pj(1,4,4)=0
       exch2_pj(2,4,4)=1
       exch2_oi(4,4)=-32
       exch2_oi_f(4,4)=-32
       exch2_oj(4,4)=0
       exch2_oj_f(4,4)=0
       exch2_itlo_c(4,4)=33
       exch2_ithi_c(4,4)=33
       exch2_jtlo_c(4,4)=0
       exch2_jthi_c(4,4)=17
       exch2_opposingSend_record(4,4)=4
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_5
C      Topology for tile 5
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(5)=32
       exch2_tny(5)=16
       exch2_tbasex(5)=0
       exch2_tbasey(5)=0
       exch2_txglobalo(5)=65
       exch2_tyglobalo(5)=1
       exch2_isWedge(5)=1
       exch2_isNedge(5)=0
       exch2_isEedge(5)=1
       exch2_isSedge(5)=1
       exch2_myFace(5)=3
       exch2_mydnx(5)=32
       exch2_mydny(5)=32
       exch2_nNeighbours(5)=4

       exch2_neighbourId(1,5)=6
       exch2_pi(1,1,5)=1
       exch2_pi(2,1,5)=0
       exch2_pj(1,1,5)=0
       exch2_pj(2,1,5)=1
       exch2_oi(1,5)=0
       exch2_oi_f(1,5)=0
       exch2_oj(1,5)=0
       exch2_oj_f(1,5)=0
       exch2_itlo_c(1,5)=0
       exch2_ithi_c(1,5)=33
       exch2_jtlo_c(1,5)=0
       exch2_jthi_c(1,5)=0
       exch2_opposingSend_record(1,5)=3

       exch2_neighbourId(2,5)=4
       exch2_pi(1,2,5)=1
       exch2_pi(2,2,5)=0
       exch2_pj(1,2,5)=0
       exch2_pj(2,2,5)=1
       exch2_oi(2,5)=0
       exch2_oi_f(2,5)=0
       exch2_oj(2,5)=-32
       exch2_oj_f(2,5)=-32
       exch2_itlo_c(2,5)=0
       exch2_ithi_c(2,5)=33
       exch2_jtlo_c(2,5)=17
       exch2_jthi_c(2,5)=17
       exch2_opposingSend_record(2,5)=1

       exch2_neighbourId(3,5)=7
       exch2_pi(1,3,5)=1
       exch2_pi(2,3,5)=0
       exch2_pj(1,3,5)=0
       exch2_pj(2,3,5)=1
       exch2_oi(3,5)=32
       exch2_oi_f(3,5)=32
       exch2_oj(3,5)=0
       exch2_oj_f(3,5)=0
       exch2_itlo_c(3,5)=0
       exch2_ithi_c(3,5)=0
       exch2_jtlo_c(3,5)=0
       exch2_jthi_c(3,5)=17
       exch2_opposingSend_record(3,5)=5

       exch2_neighbourId(4,5)=2
       exch2_pi(1,4,5)=0
       exch2_pi(2,4,5)=1
       exch2_pj(1,4,5)=-1
       exch2_pj(2,4,5)=0
       exch2_oi(4,5)=-32
       exch2_oi_f(4,5)=-32
       exch2_oj(4,5)=33
       exch2_oj_f(4,5)=34
       exch2_itlo_c(4,5)=16
       exch2_ithi_c(4,5)=33
       exch2_jtlo_c(4,5)=17
       exch2_jthi_c(4,5)=17
       exch2_opposingSend_record(4,5)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_6
C      Topology for tile 6
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(6)=32
       exch2_tny(6)=16
       exch2_tbasex(6)=0
       exch2_tbasey(6)=16
       exch2_txglobalo(6)=65
       exch2_tyglobalo(6)=17
       exch2_isWedge(6)=1
       exch2_isNedge(6)=1
       exch2_isEedge(6)=1
       exch2_isSedge(6)=0
       exch2_myFace(6)=3
       exch2_mydnx(6)=32
       exch2_mydny(6)=32
       exch2_nNeighbours(6)=5

       exch2_neighbourId(1,6)=9
       exch2_pi(1,1,6)=0
       exch2_pi(2,1,6)=-1
       exch2_pj(1,1,6)=1
       exch2_pj(2,1,6)=0
       exch2_oi(1,6)=33
       exch2_oi_f(1,6)=34
       exch2_oj(1,6)=32
       exch2_oj_f(1,6)=32
       exch2_itlo_c(1,6)=0
       exch2_ithi_c(1,6)=0
       exch2_jtlo_c(1,6)=0
       exch2_jthi_c(1,6)=17
       exch2_opposingSend_record(1,6)=4

       exch2_neighbourId(2,6)=10
       exch2_pi(1,2,6)=0
       exch2_pi(2,2,6)=-1
       exch2_pj(1,2,6)=1
       exch2_pj(2,2,6)=0
       exch2_oi(2,6)=33
       exch2_oi_f(2,6)=34
       exch2_oj(2,6)=32
       exch2_oj_f(2,6)=32
       exch2_itlo_c(2,6)=0
       exch2_ithi_c(2,6)=0
       exch2_jtlo_c(2,6)=0
       exch2_jthi_c(2,6)=17
       exch2_opposingSend_record(2,6)=5

       exch2_neighbourId(3,6)=5
       exch2_pi(1,3,6)=1
       exch2_pi(2,3,6)=0
       exch2_pj(1,3,6)=0
       exch2_pj(2,3,6)=1
       exch2_oi(3,6)=0
       exch2_oi_f(3,6)=0
       exch2_oj(3,6)=0
       exch2_oj_f(3,6)=0
       exch2_itlo_c(3,6)=1
       exch2_ithi_c(3,6)=32
       exch2_jtlo_c(3,6)=17
       exch2_jthi_c(3,6)=17
       exch2_opposingSend_record(3,6)=1

       exch2_neighbourId(4,6)=8
       exch2_pi(1,4,6)=1
       exch2_pi(2,4,6)=0
       exch2_pj(1,4,6)=0
       exch2_pj(2,4,6)=1
       exch2_oi(4,6)=32
       exch2_oi_f(4,6)=32
       exch2_oj(4,6)=0
       exch2_oj_f(4,6)=0
       exch2_itlo_c(4,6)=0
       exch2_ithi_c(4,6)=0
       exch2_jtlo_c(4,6)=0
       exch2_jthi_c(4,6)=17
       exch2_opposingSend_record(4,6)=4

       exch2_neighbourId(5,6)=2
       exch2_pi(1,5,6)=0
       exch2_pi(2,5,6)=1
       exch2_pj(1,5,6)=-1
       exch2_pj(2,5,6)=0
       exch2_oi(5,6)=-32
       exch2_oi_f(5,6)=-32
       exch2_oj(5,6)=33
       exch2_oj_f(5,6)=34
       exch2_itlo_c(5,6)=0
       exch2_ithi_c(5,6)=17
       exch2_jtlo_c(5,6)=17
       exch2_jthi_c(5,6)=17
       exch2_opposingSend_record(5,6)=2
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_7
C      Topology for tile 7
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(7)=32
       exch2_tny(7)=16
       exch2_tbasex(7)=0
       exch2_tbasey(7)=0
       exch2_txglobalo(7)=97
       exch2_tyglobalo(7)=1
       exch2_isWedge(7)=1
       exch2_isNedge(7)=0
       exch2_isEedge(7)=1
       exch2_isSedge(7)=1
       exch2_myFace(7)=4
       exch2_mydnx(7)=32
       exch2_mydny(7)=32
       exch2_nNeighbours(7)=5

       exch2_neighbourId(1,7)=8
       exch2_pi(1,1,7)=1
       exch2_pi(2,1,7)=0
       exch2_pj(1,1,7)=0
       exch2_pj(2,1,7)=1
       exch2_oi(1,7)=0
       exch2_oi_f(1,7)=0
       exch2_oj(1,7)=0
       exch2_oj_f(1,7)=0
       exch2_itlo_c(1,7)=0
       exch2_ithi_c(1,7)=33
       exch2_jtlo_c(1,7)=0
       exch2_jthi_c(1,7)=0
       exch2_opposingSend_record(1,7)=2

       exch2_neighbourId(2,7)=3
       exch2_pi(1,2,7)=0
       exch2_pi(2,2,7)=-1
       exch2_pj(1,2,7)=1
       exch2_pj(2,2,7)=0
       exch2_oi(2,7)=33
       exch2_oi_f(2,7)=34
       exch2_oj(2,7)=-32
       exch2_oj_f(2,7)=-32
       exch2_itlo_c(2,7)=33
       exch2_ithi_c(2,7)=33
       exch2_jtlo_c(2,7)=0
       exch2_jthi_c(2,7)=17
       exch2_opposingSend_record(2,7)=4

       exch2_neighbourId(3,7)=4
       exch2_pi(1,3,7)=0
       exch2_pi(2,3,7)=-1
       exch2_pj(1,3,7)=1
       exch2_pj(2,3,7)=0
       exch2_oi(3,7)=33
       exch2_oi_f(3,7)=34
       exch2_oj(3,7)=-32
       exch2_oj_f(3,7)=-32
       exch2_itlo_c(3,7)=33
       exch2_ithi_c(3,7)=33
       exch2_jtlo_c(3,7)=0
       exch2_jthi_c(3,7)=17
       exch2_opposingSend_record(3,7)=3

       exch2_neighbourId(4,7)=11
       exch2_pi(1,4,7)=0
       exch2_pi(2,4,7)=1
       exch2_pj(1,4,7)=-1
       exch2_pj(2,4,7)=0
       exch2_oi(4,7)=32
       exch2_oi_f(4,7)=32
       exch2_oj(4,7)=33
       exch2_oj_f(4,7)=34
       exch2_itlo_c(4,7)=16
       exch2_ithi_c(4,7)=33
       exch2_jtlo_c(4,7)=0
       exch2_jthi_c(4,7)=0
       exch2_opposingSend_record(4,7)=2

       exch2_neighbourId(5,7)=5
       exch2_pi(1,5,7)=1
       exch2_pi(2,5,7)=0
       exch2_pj(1,5,7)=0
       exch2_pj(2,5,7)=1
       exch2_oi(5,7)=-32
       exch2_oi_f(5,7)=-32
       exch2_oj(5,7)=0
       exch2_oj_f(5,7)=0
       exch2_itlo_c(5,7)=33
       exch2_ithi_c(5,7)=33
       exch2_jtlo_c(5,7)=0
       exch2_jthi_c(5,7)=17
       exch2_opposingSend_record(5,7)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_8
C      Topology for tile 8
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(8)=32
       exch2_tny(8)=16
       exch2_tbasex(8)=0
       exch2_tbasey(8)=16
       exch2_txglobalo(8)=97
       exch2_tyglobalo(8)=17
       exch2_isWedge(8)=1
       exch2_isNedge(8)=1
       exch2_isEedge(8)=1
       exch2_isSedge(8)=0
       exch2_myFace(8)=4
       exch2_mydnx(8)=32
       exch2_mydny(8)=32
       exch2_nNeighbours(8)=4

       exch2_neighbourId(1,8)=9
       exch2_pi(1,1,8)=1
       exch2_pi(2,1,8)=0
       exch2_pj(1,1,8)=0
       exch2_pj(2,1,8)=1
       exch2_oi(1,8)=0
       exch2_oi_f(1,8)=0
       exch2_oj(1,8)=32
       exch2_oj_f(1,8)=32
       exch2_itlo_c(1,8)=0
       exch2_ithi_c(1,8)=33
       exch2_jtlo_c(1,8)=0
       exch2_jthi_c(1,8)=0
       exch2_opposingSend_record(1,8)=2

       exch2_neighbourId(2,8)=7
       exch2_pi(1,2,8)=1
       exch2_pi(2,2,8)=0
       exch2_pj(1,2,8)=0
       exch2_pj(2,2,8)=1
       exch2_oi(2,8)=0
       exch2_oi_f(2,8)=0
       exch2_oj(2,8)=0
       exch2_oj_f(2,8)=0
       exch2_itlo_c(2,8)=1
       exch2_ithi_c(2,8)=32
       exch2_jtlo_c(2,8)=17
       exch2_jthi_c(2,8)=17
       exch2_opposingSend_record(2,8)=1

       exch2_neighbourId(3,8)=11
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
       exch2_opposingSend_record(3,8)=3

       exch2_neighbourId(4,8)=6
       exch2_pi(1,4,8)=1
       exch2_pi(2,4,8)=0
       exch2_pj(1,4,8)=0
       exch2_pj(2,4,8)=1
       exch2_oi(4,8)=-32
       exch2_oi_f(4,8)=-32
       exch2_oj(4,8)=0
       exch2_oj_f(4,8)=0
       exch2_itlo_c(4,8)=33
       exch2_ithi_c(4,8)=33
       exch2_jtlo_c(4,8)=0
       exch2_jthi_c(4,8)=17
       exch2_opposingSend_record(4,8)=4
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_9
C      Topology for tile 9
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(9)=32
       exch2_tny(9)=16
       exch2_tbasex(9)=0
       exch2_tbasey(9)=0
       exch2_txglobalo(9)=129
       exch2_tyglobalo(9)=1
       exch2_isWedge(9)=1
       exch2_isNedge(9)=0
       exch2_isEedge(9)=1
       exch2_isSedge(9)=1
       exch2_myFace(9)=5
       exch2_mydnx(9)=32
       exch2_mydny(9)=32
       exch2_nNeighbours(9)=4

       exch2_neighbourId(1,9)=10
       exch2_pi(1,1,9)=1
       exch2_pi(2,1,9)=0
       exch2_pj(1,1,9)=0
       exch2_pj(2,1,9)=1
       exch2_oi(1,9)=0
       exch2_oi_f(1,9)=0
       exch2_oj(1,9)=0
       exch2_oj_f(1,9)=0
       exch2_itlo_c(1,9)=0
       exch2_ithi_c(1,9)=33
       exch2_jtlo_c(1,9)=0
       exch2_jthi_c(1,9)=0
       exch2_opposingSend_record(1,9)=3

       exch2_neighbourId(2,9)=8
       exch2_pi(1,2,9)=1
       exch2_pi(2,2,9)=0
       exch2_pj(1,2,9)=0
       exch2_pj(2,2,9)=1
       exch2_oi(2,9)=0
       exch2_oi_f(2,9)=0
       exch2_oj(2,9)=-32
       exch2_oj_f(2,9)=-32
       exch2_itlo_c(2,9)=0
       exch2_ithi_c(2,9)=33
       exch2_jtlo_c(2,9)=17
       exch2_jthi_c(2,9)=17
       exch2_opposingSend_record(2,9)=1

       exch2_neighbourId(3,9)=11
       exch2_pi(1,3,9)=1
       exch2_pi(2,3,9)=0
       exch2_pj(1,3,9)=0
       exch2_pj(2,3,9)=1
       exch2_oi(3,9)=32
       exch2_oi_f(3,9)=32
       exch2_oj(3,9)=0
       exch2_oj_f(3,9)=0
       exch2_itlo_c(3,9)=0
       exch2_ithi_c(3,9)=0
       exch2_jtlo_c(3,9)=0
       exch2_jthi_c(3,9)=17
       exch2_opposingSend_record(3,9)=5

       exch2_neighbourId(4,9)=6
       exch2_pi(1,4,9)=0
       exch2_pi(2,4,9)=1
       exch2_pj(1,4,9)=-1
       exch2_pj(2,4,9)=0
       exch2_oi(4,9)=-32
       exch2_oi_f(4,9)=-32
       exch2_oj(4,9)=33
       exch2_oj_f(4,9)=34
       exch2_itlo_c(4,9)=16
       exch2_ithi_c(4,9)=33
       exch2_jtlo_c(4,9)=17
       exch2_jthi_c(4,9)=17
       exch2_opposingSend_record(4,9)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_10
C      Topology for tile 10
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(10)=32
       exch2_tny(10)=16
       exch2_tbasex(10)=0
       exch2_tbasey(10)=16
       exch2_txglobalo(10)=129
       exch2_tyglobalo(10)=17
       exch2_isWedge(10)=1
       exch2_isNedge(10)=1
       exch2_isEedge(10)=1
       exch2_isSedge(10)=0
       exch2_myFace(10)=5
       exch2_mydnx(10)=32
       exch2_mydny(10)=32
       exch2_nNeighbours(10)=5

       exch2_neighbourId(1,10)=1
       exch2_pi(1,1,10)=0
       exch2_pi(2,1,10)=-1
       exch2_pj(1,1,10)=1
       exch2_pj(2,1,10)=0
       exch2_oi(1,10)=33
       exch2_oi_f(1,10)=34
       exch2_oj(1,10)=32
       exch2_oj_f(1,10)=32
       exch2_itlo_c(1,10)=0
       exch2_ithi_c(1,10)=0
       exch2_jtlo_c(1,10)=0
       exch2_jthi_c(1,10)=17
       exch2_opposingSend_record(1,10)=4

       exch2_neighbourId(2,10)=2
       exch2_pi(1,2,10)=0
       exch2_pi(2,2,10)=-1
       exch2_pj(1,2,10)=1
       exch2_pj(2,2,10)=0
       exch2_oi(2,10)=33
       exch2_oi_f(2,10)=34
       exch2_oj(2,10)=32
       exch2_oj_f(2,10)=32
       exch2_itlo_c(2,10)=0
       exch2_ithi_c(2,10)=0
       exch2_jtlo_c(2,10)=0
       exch2_jthi_c(2,10)=17
       exch2_opposingSend_record(2,10)=5

       exch2_neighbourId(3,10)=9
       exch2_pi(1,3,10)=1
       exch2_pi(2,3,10)=0
       exch2_pj(1,3,10)=0
       exch2_pj(2,3,10)=1
       exch2_oi(3,10)=0
       exch2_oi_f(3,10)=0
       exch2_oj(3,10)=0
       exch2_oj_f(3,10)=0
       exch2_itlo_c(3,10)=1
       exch2_ithi_c(3,10)=32
       exch2_jtlo_c(3,10)=17
       exch2_jthi_c(3,10)=17
       exch2_opposingSend_record(3,10)=1

       exch2_neighbourId(4,10)=12
       exch2_pi(1,4,10)=1
       exch2_pi(2,4,10)=0
       exch2_pj(1,4,10)=0
       exch2_pj(2,4,10)=1
       exch2_oi(4,10)=32
       exch2_oi_f(4,10)=32
       exch2_oj(4,10)=0
       exch2_oj_f(4,10)=0
       exch2_itlo_c(4,10)=0
       exch2_ithi_c(4,10)=0
       exch2_jtlo_c(4,10)=0
       exch2_jthi_c(4,10)=17
       exch2_opposingSend_record(4,10)=4

       exch2_neighbourId(5,10)=6
       exch2_pi(1,5,10)=0
       exch2_pi(2,5,10)=1
       exch2_pj(1,5,10)=-1
       exch2_pj(2,5,10)=0
       exch2_oi(5,10)=-32
       exch2_oi_f(5,10)=-32
       exch2_oj(5,10)=33
       exch2_oj_f(5,10)=34
       exch2_itlo_c(5,10)=0
       exch2_ithi_c(5,10)=17
       exch2_jtlo_c(5,10)=17
       exch2_jthi_c(5,10)=17
       exch2_opposingSend_record(5,10)=2
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_11
C      Topology for tile 11
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(11)=32
       exch2_tny(11)=16
       exch2_tbasex(11)=0
       exch2_tbasey(11)=0
       exch2_txglobalo(11)=161
       exch2_tyglobalo(11)=1
       exch2_isWedge(11)=1
       exch2_isNedge(11)=0
       exch2_isEedge(11)=1
       exch2_isSedge(11)=1
       exch2_myFace(11)=6
       exch2_mydnx(11)=32
       exch2_mydny(11)=32
       exch2_nNeighbours(11)=5

       exch2_neighbourId(1,11)=12
       exch2_pi(1,1,11)=1
       exch2_pi(2,1,11)=0
       exch2_pj(1,1,11)=0
       exch2_pj(2,1,11)=1
       exch2_oi(1,11)=0
       exch2_oi_f(1,11)=0
       exch2_oj(1,11)=0
       exch2_oj_f(1,11)=0
       exch2_itlo_c(1,11)=0
       exch2_ithi_c(1,11)=33
       exch2_jtlo_c(1,11)=0
       exch2_jthi_c(1,11)=0
       exch2_opposingSend_record(1,11)=2

       exch2_neighbourId(2,11)=7
       exch2_pi(1,2,11)=0
       exch2_pi(2,2,11)=-1
       exch2_pj(1,2,11)=1
       exch2_pj(2,2,11)=0
       exch2_oi(2,11)=33
       exch2_oi_f(2,11)=34
       exch2_oj(2,11)=-32
       exch2_oj_f(2,11)=-32
       exch2_itlo_c(2,11)=33
       exch2_ithi_c(2,11)=33
       exch2_jtlo_c(2,11)=0
       exch2_jthi_c(2,11)=17
       exch2_opposingSend_record(2,11)=4

       exch2_neighbourId(3,11)=8
       exch2_pi(1,3,11)=0
       exch2_pi(2,3,11)=-1
       exch2_pj(1,3,11)=1
       exch2_pj(2,3,11)=0
       exch2_oi(3,11)=33
       exch2_oi_f(3,11)=34
       exch2_oj(3,11)=-32
       exch2_oj_f(3,11)=-32
       exch2_itlo_c(3,11)=33
       exch2_ithi_c(3,11)=33
       exch2_jtlo_c(3,11)=0
       exch2_jthi_c(3,11)=17
       exch2_opposingSend_record(3,11)=3

       exch2_neighbourId(4,11)=3
       exch2_pi(1,4,11)=0
       exch2_pi(2,4,11)=1
       exch2_pj(1,4,11)=-1
       exch2_pj(2,4,11)=0
       exch2_oi(4,11)=32
       exch2_oi_f(4,11)=32
       exch2_oj(4,11)=33
       exch2_oj_f(4,11)=34
       exch2_itlo_c(4,11)=16
       exch2_ithi_c(4,11)=33
       exch2_jtlo_c(4,11)=0
       exch2_jthi_c(4,11)=0
       exch2_opposingSend_record(4,11)=2

       exch2_neighbourId(5,11)=9
       exch2_pi(1,5,11)=1
       exch2_pi(2,5,11)=0
       exch2_pj(1,5,11)=0
       exch2_pj(2,5,11)=1
       exch2_oi(5,11)=-32
       exch2_oi_f(5,11)=-32
       exch2_oj(5,11)=0
       exch2_oj_f(5,11)=0
       exch2_itlo_c(5,11)=33
       exch2_ithi_c(5,11)=33
       exch2_jtlo_c(5,11)=0
       exch2_jthi_c(5,11)=17
       exch2_opposingSend_record(5,11)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_12
C      Topology for tile 12
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(12)=32
       exch2_tny(12)=16
       exch2_tbasex(12)=0
       exch2_tbasey(12)=16
       exch2_txglobalo(12)=161
       exch2_tyglobalo(12)=17
       exch2_isWedge(12)=1
       exch2_isNedge(12)=1
       exch2_isEedge(12)=1
       exch2_isSedge(12)=0
       exch2_myFace(12)=6
       exch2_mydnx(12)=32
       exch2_mydny(12)=32
       exch2_nNeighbours(12)=4

       exch2_neighbourId(1,12)=1
       exch2_pi(1,1,12)=1
       exch2_pi(2,1,12)=0
       exch2_pj(1,1,12)=0
       exch2_pj(2,1,12)=1
       exch2_oi(1,12)=0
       exch2_oi_f(1,12)=0
       exch2_oj(1,12)=32
       exch2_oj_f(1,12)=32
       exch2_itlo_c(1,12)=0
       exch2_ithi_c(1,12)=33
       exch2_jtlo_c(1,12)=0
       exch2_jthi_c(1,12)=0
       exch2_opposingSend_record(1,12)=2

       exch2_neighbourId(2,12)=11
       exch2_pi(1,2,12)=1
       exch2_pi(2,2,12)=0
       exch2_pj(1,2,12)=0
       exch2_pj(2,2,12)=1
       exch2_oi(2,12)=0
       exch2_oi_f(2,12)=0
       exch2_oj(2,12)=0
       exch2_oj_f(2,12)=0
       exch2_itlo_c(2,12)=1
       exch2_ithi_c(2,12)=32
       exch2_jtlo_c(2,12)=17
       exch2_jthi_c(2,12)=17
       exch2_opposingSend_record(2,12)=1

       exch2_neighbourId(3,12)=3
       exch2_pi(1,3,12)=0
       exch2_pi(2,3,12)=1
       exch2_pj(1,3,12)=-1
       exch2_pj(2,3,12)=0
       exch2_oi(3,12)=32
       exch2_oi_f(3,12)=32
       exch2_oj(3,12)=33
       exch2_oj_f(3,12)=34
       exch2_itlo_c(3,12)=0
       exch2_ithi_c(3,12)=17
       exch2_jtlo_c(3,12)=0
       exch2_jthi_c(3,12)=0
       exch2_opposingSend_record(3,12)=3

       exch2_neighbourId(4,12)=10
       exch2_pi(1,4,12)=1
       exch2_pi(2,4,12)=0
       exch2_pj(1,4,12)=0
       exch2_pj(2,4,12)=1
       exch2_oi(4,12)=-32
       exch2_oi_f(4,12)=-32
       exch2_oj(4,12)=0
       exch2_oj_f(4,12)=0
       exch2_itlo_c(4,12)=33
       exch2_ithi_c(4,12)=33
       exch2_jtlo_c(4,12)=0
       exch2_jthi_c(4,12)=17
       exch2_opposingSend_record(4,12)=4
       RETURN
       END
                                                  
