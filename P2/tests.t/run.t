  $ ../main.exe test.f
  Int
  Int
  $TESTCASE_ROOT/test.f:3.3:
  Mismatched types: Not an integer
  
  
  Int
  Int

  $ ../main.exe test1.f
  Int->Int->Int
  Bool
  (Int->Bool->Unit)->Int->Bool->Unit
  Bool
  52
  43
  Int
  1
  2
  6
  3628800
  840
  71

  $ ../main.exe test2.f
  30
  false
  5
  -4
  true
  false
  3
  5
  10
  3
  84
