BEGIN { commonDecl = 0} 
/^      .*/ {commonDecl = 0 }
/^[^ ]/ {commonDecl = 0 }
/^       *COMMON/ || /^       *common/ { commonDecl = 1 }
/      &/ { if (commonDecl != 1) {print} }
{if (commonDecl ==0 ) print }
END {}
