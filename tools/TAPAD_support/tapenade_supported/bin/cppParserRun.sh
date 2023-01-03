#!/usr/bin/env bash

if [ "$(uname)" = "Linux" ]; then
  TAPENADE_HOME="$(dirname -- "$(readlink -f -- "$0")")"/..
  CPPPARSER=$TAPENADE_HOME/bin/linux/cppParser
else
  TAPENADE_HOME="$(cd "$(dirname "$0")" && cd ../ && pwd)"
  CPPPARSER=$TAPENADE_HOME/bin/mac/cppParser
fi

if [ -f $CPPPARSER ]; then
  $CPPPARSER -version >/dev/null 2>&1
  RUN=$?
else
  RUN=9
fi

if [ ! $RUN = 0 ]; then
  which docker >/dev/null 2>&1
  RUNDOCKER=$?
  if [ ! $RUNDOCKER = 0 ]; then
    RUN=999
  fi
fi

case $RUN in
0)
  $CPPPARSER $*
  ;;
9)
  docker pull -q registry.gitlab.inria.fr/tapenade/tapenade/cppparser >/dev/null
  docker run --rm -v "$PWD":"$PWD" -v "$TAPENADE_HOME":"$TAPENADE_HOME" -w "$PWD" registry.gitlab.inria.fr/tapenade/tapenade/cppparser "$@"
  ;;
999)
  echo "C++ Parser not found" >&2
  ;;
*)
  docker pull -q registry.gitlab.inria.fr/tapenade/distrib >/dev/null
  docker run --rm -v "$TAPENADE_HOME/bin/linux":/usr/tapenade/bin/linux -v "$PWD":"$PWD" -w "$PWD" registry.gitlab.inria.fr/tapenade/distrib /usr/tapenade/bin/linux/cppParser "$@"
  ;;
esac
