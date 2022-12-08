#!/bin/bash

DAYNUM=`printf "%02d" $1`
INPUT_FILE="data/input$DAYNUM.txt"
STUB_FILE="DayXX.hs.stub"
HS_FILE="src/Day$DAYNUM.hs"
SESSION=`cat data/session.txt`

if [ ! -f "$STUB_FILE" ]
then
  echo "$STUB_FILE doesn't exist, are you in the right directory?"
  exit 1
fi

if [ ! -f "$INPUT_FILE" ]
then
  echo "Getting $INPUT_FILE..."
  curl https://adventofcode.com/2022/day/$1/input --cookie "$SESSION" > $INPUT_FILE
else
  echo "$INPUT_FILE already exists."
fi

if [ ! -f "$HS_FILE" ]
then
  echo "Setting up $HS_FILE..."
  sed "s/\#\#DAY\#\#/$DAYNUM/g" $STUB_FILE > $HS_FILE
else
  echo "$HS_FILE already exists."
fi

