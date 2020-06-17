#!/bin/sh
debug=no
die () {
  echo "configure.sh: error: $*" 1>&2
  exit 1
}
usage () {
cat <<EOF
usage: configure.sh [-h | -g ]
EOF
exit 0
}
while [ $# -gt 0 ]
do
  case "$1" in
    -h) usage;;
    -g) debug=yes;;
    *) die "invalid option '$1' (try '-h')";;
  esac
  shift
done
COMPILE="gcc -Wall"
if [ $debug = yes ]
then
  COMPILE="$COMPILE -ggdb3"
else
  COMPILE="$COMPILE -O3 -DNDEBUG"
fi
echo "$COMPILE"
rm -f makefile
sed -e "s,@COMPILE@,$COMPILE," makefile.in > makefile
