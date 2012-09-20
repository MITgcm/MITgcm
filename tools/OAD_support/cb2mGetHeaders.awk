#extract CPP directives with continuation lines from the input. 
BEGIN { ppContd = 0} 
/^# *ifdef .*\\$/ || /^# *ifndef .*\\$/ || /^# *endif.*\\$/ || /^# *if .*\\$/ || /^# *else.*\\$/ || /^# *define.*\\$/ { ppContd=1 }
!/^.*\\$/ { if (ppContd ==1) {print; ppContd=0} }
/^# *ifdef .*/ || /^# *ifndef .*/ || /^# *endif.*/ || /^# *if .*/ || /^# *else.*/ || /^# *define.*/ || /^# *include .*/ { if (ppContd != 1) {print} }
{if (ppContd == 1 ) print }
END {}
