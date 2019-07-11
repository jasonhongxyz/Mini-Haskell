module Main where

import Test.Tasty
import System.Environment

import CheckTest
import EvalTest
import ParserTest
import ExecTest

main =
    do
        setEnv "TASTY_TIMEOUT" "120s"
        setEnv "TASTY_QUICKCHECK_TESTS" "2"
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain testSuite
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"


testSuite =
  testGroup
    "allTests"
    [
    CheckTest.tests,
    EvalTest.tests,
    ParserTest.tests,
    CheckTest.tests,
    ExecTest.tests
    ]
