#!/bin/bash

export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"
export MODE=$1

if [[ $MODE = 'Compile' ]] ; then
  #this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile test:compile #gentests has .dependsOn(scalatest  % "test->test"), so it is common
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'RegularTests1' ]] ; then
  echo "Doing 'sbt genRegularTests1/test'"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genRegularTests1/test
  rc=$?
  echo first try, exitcode $rc      
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests1/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests2' ]] ; then
  echo "Doing 'sbt genRegularTests2/test'"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genRegularTests2/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests2/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests3' ]] ; then
  echo "Doing 'sbt genRegularTests3/test'"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genRegularTests3/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests3/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests4' ]] ; then
  echo "Doing 'sbt genRegularTests4/test'"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genRegularTests4/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests4/testQuick
    rc=$?
    echo second try, exitcode $rc
    if [[ $rc != 0 ]] ; then
      sbt ++$TRAVIS_SCALA_VERSION genRegularTests4/testQuick
      rc=$?
      echo third try, exitcode $rc
    fi
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests5' ]] ; then
  echo "Doing 'sbt genRegularTests5/test'"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genRegularTests5/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests5/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'ScalacticTests' ]] ; then
  echo "Doing 'sbt scalactic/test'"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION scalactic/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION scalactic/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'genMustMatchersTests1' ]] ; then
  echo "Doing 'sbt genMustMatchersTests1/test'"
  export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"
  
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests1/test
  rc=$?
  kill %1  
  exit $rc
fi

if [[ $MODE = 'genMustMatchersTests2' ]] ; then
  echo "Doing 'sbt genMustMatchersTests2/test'"
  export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"
  
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests2/test
  rc=$?
  kill %1  
  exit $rc
fi

if [[ $MODE = 'genMustMatchersTests3' ]] ; then
  echo "Doing 'sbt genMustMatchersTests3/test'"
  export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests3/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genMustMatchersTests4' ]] ; then
  echo "Doing 'sbt genMustMatchersTests4/test'"
  export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests4/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genGenTests' ]] ; then
  echo "Doing 'sbt genGenTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genGenTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genTablesTests' ]] ; then
  echo "Doing 'sbt genTablesTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genTablesTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genInspectorsTests' ]] ; then
  echo "Doing 'sbt genInspectorsTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genInspectorsShorthandsTests1' ]] ; then
  echo "Doing 'sbt genInspectorsShorthandsTests1/test'"
  export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsShorthandsTests1/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genInspectorsShorthandsTests2' ]] ; then
  echo "Doing 'sbt genInspectorsShorthandsTests2/test'"
  export SBT_OPTS="-server -Xms2G -Xmx2G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsShorthandsTests2/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genTheyTests' ]] ; then
  echo "Doing 'sbt genTheyTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx3G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genTheyTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genContainTests1' ]] ; then
  echo "Doing 'sbt genContainTests1/test'"
  export SBT_OPTS="-server -Xms1G -Xmx3G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genContainTests1/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genContainTests2' ]] ; then
  echo "Doing 'sbt genContainTests2/test'"
  export SBT_OPTS="-server -Xms1G -Xmx3G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genContainTests2/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genSortedTests' ]] ; then
  echo "Doing 'sbt genSortedTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx3G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genSortedTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genLoneElementTests' ]] ; then
  echo "Doing 'sbt genLoneElementTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx3G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genLoneElementTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genEmptyTests' ]] ; then
  echo "Doing 'sbt genEmptyTests/test'"
  export SBT_OPTS="-server -Xms1G -Xmx3G -Xss10M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION genEmptyTests/test
  rc=$?
  kill %1
  exit $rc
fi

#if [[ $MODE = 'genSafeStyleTests' ]] ; then
#  echo "Doing 'sbt genSafeStyleTests/test'"
#  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"
#
#  while true; do echo "..."; sleep 60; done &
#  sbt ++$TRAVIS_SCALA_VERSION genSafeStyleTests/test
#  rc=$?
#  kill %1
#  exit $rc
#fi

if [[ $MODE = 'examples' ]] ; then
  #this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
  while true; do echo "..."; sleep 60; done &
  project examples
  sbt ++$TRAVIS_SCALA_VERSION examples/compile examples/test:compile
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'examplesJS' ]] ; then
  #this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION examplesJS/compile examplesJS/test:compile
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'scalacticTestsJS1' ]] ; then

  #this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
  while true; do echo "..."; sleep 60; done &
  echo "Doing 'sbt scalacticTestJS/test:compile'"
  sbt ++$TRAVIS_SCALA_VERSION scalacticTestJS/test:compile
  echo "Doing 'sbt scalacticTestJS/test-only org.scalactic.A*" "scalacticTestJS/test-only org.scalactic.B*" "scalacticTestJS/test-only org.scalactic.C*" "scalacticTestJS/test-only org.scalactic.D*" "scalacticTestJS/test-only org.scalactic.E*'"
  sbt ++$TRAVIS_SCALA_VERSION "scalacticTestJS/test-only org.scalactic.A*" "scalacticTestJS/test-only org.scalactic.B*" "scalacticTestJS/test-only org.scalactic.C*" "scalacticTestJS/test-only org.scalactic.D*" "scalacticTestJS/test-only org.scalactic.E*"
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'scalacticTestsJS2' ]] ; then

  #this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
  while true; do echo "..."; sleep 60; done &
  echo "Doing 'sbt scalacticTestJS/test:compile'"
  sbt ++$TRAVIS_SCALA_VERSION scalacticTestJS/test:compile
  echo "Doing 'sbt scalacticTestJS/test-only org.scalactic.F*" "scalacticTestJS/test-only org.scalactic.G*" "scalacticTestJS/test-only org.scalactic.H*" "scalacticTestJS/test-only org.scalactic.I*" "scalacticTestJS/test-only org.scalactic.J*'"
  sbt ++$TRAVIS_SCALA_VERSION "scalacticTestJS/test-only org.scalactic.F*" "scalacticTestJS/test-only org.scalactic.G*" "scalacticTestJS/test-only org.scalactic.H*" "scalacticTestJS/test-only org.scalactic.I*" "scalacticTestJS/test-only org.scalactic.J*"
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'Publish' ]] ; then
  sbt ++$TRAVIS_SCALA_VERSION publishSigned
  sbt ++$TRAVIS_SCALA_VERSION scalactic/publishSigned
fi
