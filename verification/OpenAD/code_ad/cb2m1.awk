BEGIN { incommon = 0; fstart = 0} 
{if ( fstart == 0 ) { fstart=1 }   }
/^      .*/ {incommon = 0 }
/^[^ ]/ {incommon = 0 }
/^      COMMON/ || /^      common/ { incommon = 1 }
/      &/ { if (incommon != 1) {print} }
{if (incommon ==0 ) print}
END { 
}


