@echo off
title Tapenade

IF NOT DEFINED TAPENADE_HOME set TAPENADE_HOME=..
IF NOT DEFINED JAVA_HOME set JAVA_HOME="C:\Progra~1\Java\jdk1.8.0_18"
IF NOT DEFINED JAVA_BIN set JAVA_BIN="%JAVA_HOME%\bin\java.exe"

set HEAP_SIZE=-mx256m
set CLASSPATH="%TAPENADE_HOME%\build\libs\tapenade-3.16.jar"
set BROWSER="C:\Program Files\Internet Explorer\iexplore.exe"

"%JAVA_BIN%" %HEAP_SIZE% -classpath %CLASSPATH% -Djava_home="%JAVA_HOME%" -Dtapenade_home=%TAPENADE_HOME% -Dbrowser=%BROWSER% fr.inria.tapenade.toplevel.Tapenade %*
