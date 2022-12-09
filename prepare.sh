#!/bin/bash
#
# WARNING! Not tested with GNU sed, it probably won't work. Use BSD sed
# or test/fix stuff.
#

DAYNUM=`printf "%02d" $1`
STUB_FILE="DayXX.hs.stub"
INPUT_FILE="data/input$DAYNUM.txt"
INPUT_URL="https://adventofcode.com/2022/day/$1/input"
HS_FILE="src/Day$DAYNUM.hs"
MAIN_FILE="app/Main.hs"
SESSION_FILE="data/session.txt"
SESSION=`cat $SESSION_FILE`
IMPORT="import           Day$DAYNUM"
ENTRY="  , ($1, Day$DAYNUM.solve)"

if [ $# -ne 1 ]
then
  echo "Illegal number of arguments ($#, should be 1)."
  exit 1
fi

if [ $1 -lt 1 ] || [ $1 -gt 25 ]
then
  echo "Illegal day $1, must be 1-25."
  exit 1
fi

if [ ! -f "$STUB_FILE" ]
then
  echo "$STUB_FILE doesn't exist, are you in the right directory?"
  exit 1
fi

if [ ! -f "$MAIN_FILE" ]
then
  echo "$MAIN_FILE doesn't exist, are you in the right directory?"
  exit 1
fi

if [ ! -f "$SESSION_FILE" ]
then
  echo "$SESSION_FILE doesn't exist."
  exit 1
fi

if [ -f "$INPUT_FILE" ]
then 
  echo "$INPUT_FILE already exists."
  exit 1
fi

if [ -f "$HS_FILE" ]
then
  echo "$HS_FILE already exists."
  exit 1
fi

if grep -q "import.*Day$DAYNUM" $MAIN_FILE
then
  echo "$MAIN_FILE already updated."
  exit 1
fi

echo "Getting $INPUT_FILE..."
curl $INPUT_URL --cookie "session=$SESSION" > $INPUT_FILE.new 2> /dev/null
if [ $? -ne 0 ]
then
  echo "Couldn't get input."
  exit 1
fi
mv $INPUT_FILE.new $INPUT_FILE

echo "Setting up $HS_FILE..."
sed "s/\#\#DAY\#\#/$DAYNUM/g" $STUB_FILE > $HS_FILE

echo "Updating $MAIN_FILE..."
sed -e "/{-- insert import here --}/i\\
$IMPORT" -e "/{-- insert map entry here --}/i\\
$ENTRY" $MAIN_FILE > $MAIN_FILE.new
mv $MAIN_FILE $MAIN_FILE.bak
mv $MAIN_FILE.new $MAIN_FILE

