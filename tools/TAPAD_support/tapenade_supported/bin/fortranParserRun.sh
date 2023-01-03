#!/usr/bin/env bash

if [ "$(uname)" = "Linux" ]; then
  TAPENADE_HOME="$(dirname -- "$(readlink -f -- "$0")")"/..
  FORTRANPARSER=$TAPENADE_HOME/bin/linux/fortranParser
else
  TAPENADE_HOME="$(cd "$(dirname "$0")" && cd ../ && pwd)"
  FORTRANPARSER=$TAPENADE_HOME/bin/mac/fortranParser
fi

$FORTRANPARSER -version >/dev/null 2>&1
RUN=$?

case $RUN in
0)
  $FORTRANPARSER "$@"
  ;;
*)
  GITTAG=`tapenade -version | grep Tag | awk 'BEGIN{FS=":      "} {print $2}'`
  docker pull -q registry.gitlab.inria.fr/tapenade/tapenade/fortranparser:$GITTAG >/dev/null
  docker run --rm -v "$PWD":"$PWD" -v "$TAPENADE_HOME":"$TAPENADE_HOME" -w "$PWD" registry.gitlab.inria.fr/tapenade/tapenade/fortranparser:$GITTAG "$@"
  ;;
esac
