#!/bin/zsh
unset HOME
BASE_DIR=`pwd`
CHECKOUT_DIR=$BASE_DIR/checked-out
EVAL_DIR=$BASE_DIR
LOGS_DIR=$EVAL_DIR/logs
TESTS_DIR=$EVAL_DIR/auto-tests
GROUP_RATE=$(ls auto-tests/*-ok.til | sed -e 's=auto-tests/==g' | cut -d- -f1 | sort -u | wc -l)
LANG=til

#--------------------------------------------------------------------------
PROJDIR=$CHECKOUT_DIR/$1
COMPILER_CMD=./$LANG

#--------------------------------------------------------------------------
# per-group results
# $1 must be in the form NNN (three-digit number)
#--------------------------------------------------------------------------
LOG_FILE=$LOGS_DIR/$1.log.html
RESULTS_FILE=$LOGS_DIR/$1.res.html

###########################################################################
#

function check_bad_functions() {
  location="$1"
  badfound=0

  ### initialize bad functions (list from string.h, selected stdlib.h and stdio.h)
  typeset -A BADFUNCS
  set -A BADFUNCS `for i in basename bcmp bcopy bzero __bzero ffs ffsl ffsll memccpy memchr memcmp memcpy memfrob memmem memmove mempcpy __mempcpy memrchr memset rawmemchr __REDIRECT_NTH stpcpy __stpcpy stpncpy __stpncpy strcasecmp strcasecmp_l strcasestr strcat strchr strchrnul strcmp strcoll strcoll_l strcpy strcspn strdup strerror strerror_l strerror_r strfry strlen strncasecmp strncasecmp_l strncat strncmp strncpy strndup strnlen strpbrk strrchr strsep strsignal strspn strstr strtok __strtok_r strtok_r strverscmp strxfrm strxfrm_l __xpg_strerror_r calloc malloc free realloc printf sprintf fprintf scanf fscanf sscanf; do echo "$i 1 "; done `

  for i in ${(k)BADFUNCS}
  do
    egrep -r "\b$i\b" "$location" > /dev/null 2>&1
    BADFUNCS[$i]=$?
    if [ $BADFUNCS[$i] -eq 0 ]; then badfound=1; fi
  done

  if [ $badfound -eq 1 ];
  then
    echo >> $RESULTS_FILE
    echo "!! USES BAD FUNCTIONS (some of these uses are penalized -- index may not exist): " >> $RESULTS_FILE
    for i in ${(k)BADFUNCS}
    do
      if [ $BADFUNCS[$i] -eq 0 ]; then echo " * $i" >> $RESULTS_FILE; fi
    done
    echo >> $RESULTS_FILE
    echo "More information: Secção [Projecto] (Fénix) " >> $RESULTS_FILE
  fi
}


###########################################################################
#

function compile_project() {

  #------------------------------------------------------------------------

  echo "<pre>" >> $RESULTS_FILE
  echo "#################### PROJECT STRUCTURE ####################" >> $RESULTS_FILE
  echo >> $RESULTS_FILE

  echo "Checking project structure..."
  cd $PROJDIR
  make ROOT= clean > /dev/null

  echo >> $RESULTS_FILE
  echo "====================     N O D E S     ====================" >> $RESULTS_FILE
  echo >> $RESULTS_FILE

  # check for core lib
  if [ ! -d $PROJDIR/ast ]; then
    echo "!! 'ast' does not exist." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
    echo
    exit
  else
    echo "-- Directory 'ast' found." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
  fi

  echo -n "Number of nodes: " >> $RESULTS_FILE
  find ast -type f -name \*.h | wc -l >> $RESULTS_FILE
  #PRIVACY find ast -type f -name \*.h -print | sed -e 's/\.h/ /g' -e 's=^.*/= * =g' >> $RESULTS_FILE

  check_bad_functions ast

  echo >> $RESULTS_FILE
  echo "====================     SEMANTICS     ====================" >> $RESULTS_FILE
  echo >> $RESULTS_FILE

  if [ ! -d $PROJDIR/targets ]; then
    echo "!! 'targets' does not exist." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
    echo
    exit
  else
    echo "-- Directory 'targets' found." >> $RESULTS_FILE
  fi

  check_bad_functions targets

  echo >> $RESULTS_FILE
  echo "====================     SCANNER       ====================" >> $RESULTS_FILE
  echo >> $RESULTS_FILE

  if [ ! -f ${LANG}_scanner.l ]; then
    echo "!! Scanner specification does not exist." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
    echo
    exit
  else
    echo "-- Scanner specification found." >> $RESULTS_FILE
  fi

  check_bad_functions ${LANG}_scanner.l

  echo >> $RESULTS_FILE
  echo "====================      PARSER       ====================" >> $RESULTS_FILE
  echo >> $RESULTS_FILE

  if [ ! -f ${LANG}_parser.y ]; then
    echo "!! Parser specification does not exist." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
    echo
    exit
  else
    echo "-- Parser specification found." >> $RESULTS_FILE
  fi

  # first try with name ${LANG}_parser.y
  make ROOT= ${LANG}_parser.tab.h > /dev/null 2>&1
  #byacc -v ${LANG}_parser.y > /dev/null 2>&1
  if [ ! -f ${LANG}_parser.output ]; then
    echo "!! could not find a valid grammar file to create LALR(1) description." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
    echo
    exit
  fi

  # parse LALR(1) statistics
  tail -2 ${LANG}_parser.output >> $RESULTS_FILE
  egrep '(shift\/reduce|reduce\/reduce)' ${LANG}_parser.output> /dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "!! GRAMMAR HAS CONFLICTS" >> $RESULTS_FILE
  else
    echo "-- grammar has no conflics" >> $RESULTS_FILE
  fi

  grep -v YYSTYPE ${LANG}_parser.y | egrep '{[ \t]*}' > /dev/null 2>&1
  if [ $? -eq 0 ]; then echo "!! PARSER CONTAINS EMPTY RULES" >> $RESULTS_FILE; fi

  check_bad_functions ${LANG}_parser.y

  echo >> $RESULTS_FILE
  echo "====================     COMPILING     ====================" >> $RESULTS_FILE
  echo >> $RESULTS_FILE

  make ROOT= clean > /dev/null
  make ROOT= > /dev/null # trying 'make'

  if [ ! -f ${LANG} ]; then
    echo "!! Makefile could not produce '${LANG}'." >> $RESULTS_FILE
    echo >> $RESULTS_FILE
    echo
    exit
  else
    echo "-- successfully generated '${LANG}'."  >> $RESULTS_FILE
  fi

  #------------------------------------------------------------------------

  echo >> $RESULTS_FILE

  #------------------------------------------------------------------------

  cd $BASE_DIR # just to be more generic
}

###########################################################################
#
# $1 - must be 'compNNN'. Replace 'NNN' with group number
#
###########################################################################
function copy_tests() {
  \cp $TESTS_DIR/*.${LANG} $PROJDIR
}

###########################################################################
#
# $1 - return value of compiler execution
# $2 - test type: "ok or "err"
# This function must be run in the $PROJ_DIR directory!
# returns 0 if passed, else 1
#
# in the future it will test for both positive and negative tests (tests
# that are supposed to fail). For now it assumes that all tests are ok!
#
# (actual return / expected return)
#
###########################################################################
function check_compiler_return() {
  echo -n "Compiler: "
  if [ $2 = "ok" ]; then
    if [ $1 -ne 0 ]; then
      echo -n "failed ($1/0). "
      return 1;
    else
      echo -n "ok ($1/0). "
      return 0;
    fi
  else # expecting != 0
    if [ $1 -eq 0 ]; then
      echo -n "failed ($1/!0). "
      return 1;
    else
      echo -n "ok ($1/!0). "
      return 0;
    fi
  fi
}

###########################################################################
#
# $1 - a group number in the form NNN
# $2 - a source file to be tested
#
###########################################################################
function test_prog() {
  TEST_FILE=$2:t
  TEST_NAME=$TEST_FILE:r
  ASM_FILE=$TEST_NAME.asm

  # test characteristics
  TEST_CLASS=`echo "$TEST_NAME" | cut -d- -f 1`  # which test class it belongs
  TEST_POINTS=`echo "$TEST_NAME" | cut -d- -f 2` # value of this test
  TEST_IDX=`echo "$TEST_NAME" | cut -d- -f 3`    # index
  TEST_SNAME=`echo "$TEST_NAME" | cut -d- -f 4`  # short name
  TEST_TYPE=`echo "$TEST_NAME" | cut -d- -f 5`   # ok or err

  echo "<font color='white' style='background: blue;'>---------------------------"$TEST_FILE"---------------------------</font>"
  #echo "[1m### Testing "$TEST_FILE" ###[0m"

  echo "" >> $RESULTS_FILE

  PRETTY_PRINT=$TEST_NAME
  while [ ${#PRETTY_PRINT} -lt 30 ]; do
    PRETTY_PRINT=`echo $PRETTY_PRINT"."`
  done
  echo -n $PRETTY_PRINT >> $RESULTS_FILE

  # try to compile the program
  (ulimit -f $((10*2*1024)); ulimit -t 5; $COMPILER_CMD $TEST_FILE)
  # check for the correct return value
  check_compiler_return $? $TEST_TYPE
  if [ $? -eq 0 ]; then
    echo -n " OK" >> $RESULTS_FILE # compiler returned the expected value
  else
    echo -n " KO" >> $RESULTS_FILE # compiler did not return the expected value
  fi

  if [ ! -f $ASM_FILE ]; then
    echo "Compiler did not generate .asm file"
    return
  fi
  if [ ! -s $ASM_FILE ]; then
    echo "Generated empty asm file"
    return
  fi
  echo -n " 1" >> $RESULTS_FILE # .${LANG} => .asm ok

  # try to compile the assembly
  echo -n "YASM: "
  yasm -mx86 -felf $ASM_FILE
  if [ $? -ne 0 ]; then
    echo "'yasm' failed"
    return
  fi
  echo -n "ok. "
  echo -n " 2" >> $RESULTS_FILE # yasm ok

  # try to link
  echo -n "LD: "
  ld -melf_i386 -o $TEST_NAME $TEST_NAME.o -L. -L$DEFAULT_DIR -lprts -L$ROOT/usr/lib -lrts
  if [ $? -ne 0 ]; then
    echo "'ld' failed"
    return
  fi
  echo -n "ok. "
  echo -n " 3" >> $RESULTS_FILE # ld ok

  # try to run
  echo -n "Running: "
  (ulimit -f $((1*2*1024)); ulimit -t 5; ./$TEST_NAME) > $LOGS_DIR/$1_$TEST_NAME.out
  if [ $? -ne 0 ]; then
    echo "execution failed"
    echo -n "  " >> $RESULTS_FILE
    #return
  else
    echo -n " 4" >> $RESULTS_FILE # execution ok
  fi

  # check output
  if [ -f $LOGS_DIR/$1_$TEST_NAME.out ]; then

    # compare the output ignoring new lines

    # fully compare the output
    diff -iwc $TESTS_DIR/expected/$TEST_NAME.out $LOGS_DIR/$1_$TEST_NAME.out | sed -e "s=$BASE_DIR/==g" -e 's=auto-tests/==g' | awk 'BEGIN {zone=1} /^\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*/ { print "<font style=\"background: brown;\" color=\"white\">----- ZONE ", zone++, " ----</font>"; next } /^--- [0-9]/ { printf("<font color=\"brown\">%s</font>\n", $0); next } /^\*\*\* [0-9]/ { printf("<font color=\"brown\">%s</font>\n", $0); next } {print}'
    diff -iwub =(tr -d '\n' < $TESTS_DIR/expected/$TEST_NAME.out) =(tr -d '\n' < $LOGS_DIR/$1_$TEST_NAME.out) > /dev/null

    if [ $? -ne 0 ]; then
      echo "<font color='brown'>output differs from expected</font>"
      echo -n "  " >> $RESULTS_FILE # diff not ok
      return
    else
      echo "<font color='green'>TEST PASSED!</font>"
      echo -n " 5" >> $RESULTS_FILE # diff ok (no diffs)
    fi

  else

    echo "<font color='brown'>program did not produce any output</font>"
    return

  fi

}


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

# MAIN starts here
# $1 group to test (NNN)
#

###########################################################################
#
# SANITY CHECK: logs dir exists?
#
if [ ! -d $LOGS_DIR ]; then
  echo "Creating logs dir"
  mkdir $LOGS_DIR
fi

###########################################################################
#
# DAVID: already checked out
# SANITY CHECK: checkout dir exists?
#
# if [ ! -d $CHECKOUT_DIR ]; then
#   echo "Creating checkout dir"
#   mkdir $CHECKOUT_DIR
# fi

###########################################################################
#
# initialisations...
# check $1 was given
#
if [ -z $1 ]; then
  echo
  echo "$0 <group name>"
  echo "Please provide group name as first argument"
  echo
  exit
fi

###########################################################################
#
# checking existence of group to test.
# For now looking for the module in the configuration dir is enough.
#
if [ ! -d $PROJDIR ]; then
  echo "I know nothing about '$1'. Cannot proceed."
  exit
fi

###########################################################################
#
# per group log file
#
\rm -f $LOG_FILE
# per group results file
\rm -f $LOGS_DIR/$1* 2>/dev/null

# backup stdout
exec 6>&1
# replace stdout
exec > $LOG_FILE
# Send stderr to the same place
exec 2>&1

###########################################################################

echo "<pre>"

#GITLAB echo -n "[20D[0K"$1" starting" > /dev/fd/6

###########################################################################
#
# test compilation (make)
#

#GITLAB echo -n "[20D[0K"$1" compiling" > /dev/fd/6
compile_project $1

###########################################################################
#
# now, for each test file test the following:
# * compile  test.src => test.asm
# * assemble test.asm => test.o
# * link test.o + librts > test executable
# * run test executable
# * check output diff
#
copy_tests $1
cd $PROJDIR
echo "##################      TEST RESULTS      ###################" >> $RESULTS_FILE
echo "     OK - compiler runs; KO - compiler (partially) fails     " >> $RESULTS_FILE
echo "  1 - compiler produces ASM file from ${LANG} program            " >> $RESULTS_FILE
echo "  2 - yasm is able to produce object file (.o) from ASM file " >> $RESULTS_FILE
echo "  3 - linker is able to produce executable program           " >> $RESULTS_FILE
echo "  4 - program runs and returns 0 on sucessful exit           " >> $RESULTS_FILE
echo "  5 - program output matches expected output                 " >> $RESULTS_FILE
echo "       (tab, space, and newline chars are ignored)           " >> $RESULTS_FILE
echo >> $RESULTS_FILE
echo "                                  C   L   D" >> $RESULTS_FILE
echo "                                  O A I R I" >> $RESULTS_FILE
echo "                                  M S N U F" >> $RESULTS_FILE
echo "                                  P M K N F" >> $RESULTS_FILE
for i in $TESTS_DIR/*.${LANG}; do
  #GITLAB echo -n "[50D[0K"$1" "testing $i:t:r" " >> /dev/fd/6
  test_prog $1 $i
done
echo "" >> $RESULTS_FILE
cd $BASE_DIR # just to be more generic

###########################################################################
#
# Now parse the results!
#
echo  >> $RESULTS_FILE
echo "#####################     SUMMARY     #####################" >> $RESULTS_FILE
echo >> $RESULTS_FILE
echo  "Group: Points (Point/Test)">> $RESULTS_FILE

TEST_CLASSES_LIST=`find $TESTS_DIR -type f -name \*.${LANG} -exec basename {} \\; |  cut -d- -f1 | sort -u`

TEST_GROUPS=`echo $TEST_CLASSES_LIST | wc -w`
PER_GROUP_VALUE=$[$GROUP_RATE*1.0/$TEST_GROUPS]

at_least_one_test_ok=0
total_points=0.0
## without 'echo' the variable would be taken as a whole
for i in $(echo $TEST_CLASSES_LIST); do
  TESTS_IN_CLASS=`find $TESTS_DIR -type f -name $i-\?\?-\*.${LANG} -exec basename {} \\; |  wc -l`
  PER_TEST_VALUE=`echo "scale=50; $PER_GROUP_VALUE/$TESTS_IN_CLASS" | bc`

  tests_ok=`grep ^$i $RESULTS_FILE | grep ^$i\\- | grep "OK 1 2 3 [4 ] 5" | wc -l`
  if [ $tests_ok -ne 0 ]; then
    at_least_one_test_ok=1
  fi

  class_points=$[$tests_ok * $PER_TEST_VALUE]
  total_points=$[$total_points + $class_points]
  echo -n "$i: "$class_points >> $RESULTS_FILE
  echo " ("$PER_TEST_VALUE")" >> $RESULTS_FILE
done

total_points=$[100.0*$total_points/$TEST_GROUPS]

###########################################################################

if [ $at_least_one_test_ok -ne 0 ]; then
  echo "Total: "$total_points" (max. 100%)" >> $RESULTS_FILE
else
  echo "Total: 0" >> $RESULTS_FILE
fi

echo >> $RESULTS_FILE
echo "Number of at least \"OK\" tests: "`fgrep ". OK" $RESULTS_FILE | wc -l`"/"`find $TESTS_DIR -type f -name \*.${LANG} | wc -l` >> $RESULTS_FILE
echo >> $RESULTS_FILE

#GITLAB echo "[50D[0K"$1" "$total_points" %" > /dev/fd/6

echo "</pre>"

exit

###########################################################################
####                          T H E     E N D                          ####
###########################################################################
