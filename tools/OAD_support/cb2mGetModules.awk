BEGIN { commonDecl = 0} 
/^      .*/ {commonDecl = 0 }
/^[^ ]/ && !/^# *ifdef / && !/^# *endif/ && !/^# *if / && !/^# *else/ && !/^c.*/ && !/^C.*/ {commonDecl = 0 }
/^       *COMMON/ || /^       *common/ { commonDecl = 1 }
/^# *ifdef / || /^# *endif/ || /^# *if / || /^# *else/ { if (commonDecl == 1) {print} }
{if (commonDecl ==0 ) print }
END {}
