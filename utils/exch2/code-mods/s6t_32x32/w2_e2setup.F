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
       RETURN
       END
       SUBROUTINE W2_E2SETUP_1
C      Topology for tile 1
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(1)=32
       exch2_tny(1)=32
       exch2_tbasex(1)=0
       exch2_tbasey(1)=0
       exch2_txglobalo(1)=1
       exch2_tyglobalo(1)=1
       exch2_isWedge(1)=1
       exch2_isNedge(1)=1
       exch2_isEedge(1)=1
       exch2_isSedge(1)=1
       exch2_myFace(1)=1
       exch2_mydnx(1)=32
       exch2_mydny(1)=32
       exch2_nNeighbours(1)=4

       exch2_neighbourId(1,1)=3
       exch2_pi(1,1,1)=0
       exch2_pi(2,1,1)=-1
       exch2_pj(1,1,1)=1
       exch2_pj(2,1,1)=0
       exch2_oi(1,1)=33
       exch2_oi_f(1,1)=34
       exch2_oj(1,1)=32
       exch2_oj_f(1,1)=32
       exch2_itlo_c(1,1)=0
       exch2_ithi_c(1,1)=0
       exch2_jtlo_c(1,1)=0
       exch2_jthi_c(1,1)=33
       exch2_opposingSend_record(1,1)=4

       exch2_neighbourId(2,1)=6
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
       exch2_jtlo_c(2,1)=33
       exch2_jthi_c(2,1)=33
       exch2_opposingSend_record(2,1)=1

       exch2_neighbourId(3,1)=2
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
       exch2_jthi_c(3,1)=33
       exch2_opposingSend_record(3,1)=4

       exch2_neighbourId(4,1)=5
       exch2_pi(1,4,1)=0
       exch2_pi(2,4,1)=1
       exch2_pj(1,4,1)=-1
       exch2_pj(2,4,1)=0
       exch2_oi(4,1)=-32
       exch2_oi_f(4,1)=-32
       exch2_oj(4,1)=33
       exch2_oj_f(4,1)=34
       exch2_itlo_c(4,1)=0
       exch2_ithi_c(4,1)=33
       exch2_jtlo_c(4,1)=33
       exch2_jthi_c(4,1)=33
       exch2_opposingSend_record(4,1)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_2
C      Topology for tile 2
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(2)=32
       exch2_tny(2)=32
       exch2_tbasex(2)=0
       exch2_tbasey(2)=0
       exch2_txglobalo(2)=33
       exch2_tyglobalo(2)=1
       exch2_isWedge(2)=1
       exch2_isNedge(2)=1
       exch2_isEedge(2)=1
       exch2_isSedge(2)=1
       exch2_myFace(2)=2
       exch2_mydnx(2)=32
       exch2_mydny(2)=32
       exch2_nNeighbours(2)=4

       exch2_neighbourId(1,2)=3
       exch2_pi(1,1,2)=1
       exch2_pi(2,1,2)=0
       exch2_pj(1,1,2)=0
       exch2_pj(2,1,2)=1
       exch2_oi(1,2)=0
       exch2_oi_f(1,2)=0
       exch2_oj(1,2)=32
       exch2_oj_f(1,2)=32
       exch2_itlo_c(1,2)=0
       exch2_ithi_c(1,2)=33
       exch2_jtlo_c(1,2)=0
       exch2_jthi_c(1,2)=0
       exch2_opposingSend_record(1,2)=2

       exch2_neighbourId(2,2)=6
       exch2_pi(1,2,2)=0
       exch2_pi(2,2,2)=-1
       exch2_pj(1,2,2)=1
       exch2_pj(2,2,2)=0
       exch2_oi(2,2)=33
       exch2_oi_f(2,2)=34
       exch2_oj(2,2)=-32
       exch2_oj_f(2,2)=-32
       exch2_itlo_c(2,2)=33
       exch2_ithi_c(2,2)=33
       exch2_jtlo_c(2,2)=0
       exch2_jthi_c(2,2)=33
       exch2_opposingSend_record(2,2)=3

       exch2_neighbourId(3,2)=4
       exch2_pi(1,3,2)=0
       exch2_pi(2,3,2)=1
       exch2_pj(1,3,2)=-1
       exch2_pj(2,3,2)=0
       exch2_oi(3,2)=32
       exch2_oi_f(3,2)=32
       exch2_oj(3,2)=33
       exch2_oj_f(3,2)=34
       exch2_itlo_c(3,2)=0
       exch2_ithi_c(3,2)=33
       exch2_jtlo_c(3,2)=0
       exch2_jthi_c(3,2)=0
       exch2_opposingSend_record(3,2)=2

       exch2_neighbourId(4,2)=1
       exch2_pi(1,4,2)=1
       exch2_pi(2,4,2)=0
       exch2_pj(1,4,2)=0
       exch2_pj(2,4,2)=1
       exch2_oi(4,2)=-32
       exch2_oi_f(4,2)=-32
       exch2_oj(4,2)=0
       exch2_oj_f(4,2)=0
       exch2_itlo_c(4,2)=33
       exch2_ithi_c(4,2)=33
       exch2_jtlo_c(4,2)=0
       exch2_jthi_c(4,2)=33
       exch2_opposingSend_record(4,2)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_3
C      Topology for tile 3
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(3)=32
       exch2_tny(3)=32
       exch2_tbasex(3)=0
       exch2_tbasey(3)=0
       exch2_txglobalo(3)=65
       exch2_tyglobalo(3)=1
       exch2_isWedge(3)=1
       exch2_isNedge(3)=1
       exch2_isEedge(3)=1
       exch2_isSedge(3)=1
       exch2_myFace(3)=3
       exch2_mydnx(3)=32
       exch2_mydny(3)=32
       exch2_nNeighbours(3)=4

       exch2_neighbourId(1,3)=5
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
       exch2_jthi_c(1,3)=33
       exch2_opposingSend_record(1,3)=4

       exch2_neighbourId(2,3)=2
       exch2_pi(1,2,3)=1
       exch2_pi(2,2,3)=0
       exch2_pj(1,2,3)=0
       exch2_pj(2,2,3)=1
       exch2_oi(2,3)=0
       exch2_oi_f(2,3)=0
       exch2_oj(2,3)=-32
       exch2_oj_f(2,3)=-32
       exch2_itlo_c(2,3)=0
       exch2_ithi_c(2,3)=33
       exch2_jtlo_c(2,3)=33
       exch2_jthi_c(2,3)=33
       exch2_opposingSend_record(2,3)=1

       exch2_neighbourId(3,3)=4
       exch2_pi(1,3,3)=1
       exch2_pi(2,3,3)=0
       exch2_pj(1,3,3)=0
       exch2_pj(2,3,3)=1
       exch2_oi(3,3)=32
       exch2_oi_f(3,3)=32
       exch2_oj(3,3)=0
       exch2_oj_f(3,3)=0
       exch2_itlo_c(3,3)=0
       exch2_ithi_c(3,3)=0
       exch2_jtlo_c(3,3)=0
       exch2_jthi_c(3,3)=33
       exch2_opposingSend_record(3,3)=4

       exch2_neighbourId(4,3)=1
       exch2_pi(1,4,3)=0
       exch2_pi(2,4,3)=1
       exch2_pj(1,4,3)=-1
       exch2_pj(2,4,3)=0
       exch2_oi(4,3)=-32
       exch2_oi_f(4,3)=-32
       exch2_oj(4,3)=33
       exch2_oj_f(4,3)=34
       exch2_itlo_c(4,3)=0
       exch2_ithi_c(4,3)=33
       exch2_jtlo_c(4,3)=33
       exch2_jthi_c(4,3)=33
       exch2_opposingSend_record(4,3)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_4
C      Topology for tile 4
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(4)=32
       exch2_tny(4)=32
       exch2_tbasex(4)=0
       exch2_tbasey(4)=0
       exch2_txglobalo(4)=97
       exch2_tyglobalo(4)=1
       exch2_isWedge(4)=1
       exch2_isNedge(4)=1
       exch2_isEedge(4)=1
       exch2_isSedge(4)=1
       exch2_myFace(4)=4
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

       exch2_neighbourId(2,4)=2
       exch2_pi(1,2,4)=0
       exch2_pi(2,2,4)=-1
       exch2_pj(1,2,4)=1
       exch2_pj(2,2,4)=0
       exch2_oi(2,4)=33
       exch2_oi_f(2,4)=34
       exch2_oj(2,4)=-32
       exch2_oj_f(2,4)=-32
       exch2_itlo_c(2,4)=33
       exch2_ithi_c(2,4)=33
       exch2_jtlo_c(2,4)=0
       exch2_jthi_c(2,4)=33
       exch2_opposingSend_record(2,4)=3

       exch2_neighbourId(3,4)=6
       exch2_pi(1,3,4)=0
       exch2_pi(2,3,4)=1
       exch2_pj(1,3,4)=-1
       exch2_pj(2,3,4)=0
       exch2_oi(3,4)=32
       exch2_oi_f(3,4)=32
       exch2_oj(3,4)=33
       exch2_oj_f(3,4)=34
       exch2_itlo_c(3,4)=0
       exch2_ithi_c(3,4)=33
       exch2_jtlo_c(3,4)=0
       exch2_jthi_c(3,4)=0
       exch2_opposingSend_record(3,4)=2

       exch2_neighbourId(4,4)=3
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
       exch2_jthi_c(4,4)=33
       exch2_opposingSend_record(4,4)=3
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_5
C      Topology for tile 5
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(5)=32
       exch2_tny(5)=32
       exch2_tbasex(5)=0
       exch2_tbasey(5)=0
       exch2_txglobalo(5)=129
       exch2_tyglobalo(5)=1
       exch2_isWedge(5)=1
       exch2_isNedge(5)=1
       exch2_isEedge(5)=1
       exch2_isSedge(5)=1
       exch2_myFace(5)=5
       exch2_mydnx(5)=32
       exch2_mydny(5)=32
       exch2_nNeighbours(5)=4

       exch2_neighbourId(1,5)=1
       exch2_pi(1,1,5)=0
       exch2_pi(2,1,5)=-1
       exch2_pj(1,1,5)=1
       exch2_pj(2,1,5)=0
       exch2_oi(1,5)=33
       exch2_oi_f(1,5)=34
       exch2_oj(1,5)=32
       exch2_oj_f(1,5)=32
       exch2_itlo_c(1,5)=0
       exch2_ithi_c(1,5)=0
       exch2_jtlo_c(1,5)=0
       exch2_jthi_c(1,5)=33
       exch2_opposingSend_record(1,5)=4

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
       exch2_jtlo_c(2,5)=33
       exch2_jthi_c(2,5)=33
       exch2_opposingSend_record(2,5)=1

       exch2_neighbourId(3,5)=6
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
       exch2_jthi_c(3,5)=33
       exch2_opposingSend_record(3,5)=4

       exch2_neighbourId(4,5)=3
       exch2_pi(1,4,5)=0
       exch2_pi(2,4,5)=1
       exch2_pj(1,4,5)=-1
       exch2_pj(2,4,5)=0
       exch2_oi(4,5)=-32
       exch2_oi_f(4,5)=-32
       exch2_oj(4,5)=33
       exch2_oj_f(4,5)=34
       exch2_itlo_c(4,5)=0
       exch2_ithi_c(4,5)=33
       exch2_jtlo_c(4,5)=33
       exch2_jthi_c(4,5)=33
       exch2_opposingSend_record(4,5)=1
       RETURN
       END
                                                  
       SUBROUTINE W2_E2SETUP_6
C      Topology for tile 6
#include "W2_EXCH2_TOPOLOGY.h"
       exch2_tnx(6)=32
       exch2_tny(6)=32
       exch2_tbasex(6)=0
       exch2_tbasey(6)=0
       exch2_txglobalo(6)=161
       exch2_tyglobalo(6)=1
       exch2_isWedge(6)=1
       exch2_isNedge(6)=1
       exch2_isEedge(6)=1
       exch2_isSedge(6)=1
       exch2_myFace(6)=6
       exch2_mydnx(6)=32
       exch2_mydny(6)=32
       exch2_nNeighbours(6)=4

       exch2_neighbourId(1,6)=1
       exch2_pi(1,1,6)=1
       exch2_pi(2,1,6)=0
       exch2_pj(1,1,6)=0
       exch2_pj(2,1,6)=1
       exch2_oi(1,6)=0
       exch2_oi_f(1,6)=0
       exch2_oj(1,6)=32
       exch2_oj_f(1,6)=32
       exch2_itlo_c(1,6)=0
       exch2_ithi_c(1,6)=33
       exch2_jtlo_c(1,6)=0
       exch2_jthi_c(1,6)=0
       exch2_opposingSend_record(1,6)=2

       exch2_neighbourId(2,6)=4
       exch2_pi(1,2,6)=0
       exch2_pi(2,2,6)=-1
       exch2_pj(1,2,6)=1
       exch2_pj(2,2,6)=0
       exch2_oi(2,6)=33
       exch2_oi_f(2,6)=34
       exch2_oj(2,6)=-32
       exch2_oj_f(2,6)=-32
       exch2_itlo_c(2,6)=33
       exch2_ithi_c(2,6)=33
       exch2_jtlo_c(2,6)=0
       exch2_jthi_c(2,6)=33
       exch2_opposingSend_record(2,6)=3

       exch2_neighbourId(3,6)=2
       exch2_pi(1,3,6)=0
       exch2_pi(2,3,6)=1
       exch2_pj(1,3,6)=-1
       exch2_pj(2,3,6)=0
       exch2_oi(3,6)=32
       exch2_oi_f(3,6)=32
       exch2_oj(3,6)=33
       exch2_oj_f(3,6)=34
       exch2_itlo_c(3,6)=0
       exch2_ithi_c(3,6)=33
       exch2_jtlo_c(3,6)=0
       exch2_jthi_c(3,6)=0
       exch2_opposingSend_record(3,6)=2

       exch2_neighbourId(4,6)=5
       exch2_pi(1,4,6)=1
       exch2_pi(2,4,6)=0
       exch2_pj(1,4,6)=0
       exch2_pj(2,4,6)=1
       exch2_oi(4,6)=-32
       exch2_oi_f(4,6)=-32
       exch2_oj(4,6)=0
       exch2_oj_f(4,6)=0
       exch2_itlo_c(4,6)=33
       exch2_ithi_c(4,6)=33
       exch2_jtlo_c(4,6)=0
       exch2_jthi_c(4,6)=33
       exch2_opposingSend_record(4,6)=3
       RETURN
       END
                                                  
