  $ ../main.exe typeof.f
  Int
  Int
  $TESTCASE_ROOT/typeof.f:3.10:
  Mismatched types: Not an integer
  
  
  Int->Int->Int
  Bool
  (Int->Bool->Unit)->Int->Bool->Unit
  Bool
  Int

  $ ../main.exe eval1.f
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

  $ ../main.exe eval2.f
  Int->Int->Int
  Bool
  (Int->Bool->Unit)->Int->Bool->Unit
  Bool
  42
  $TESTCASE_ROOT/eval2.f:10.57:
  Exception type lookup failure: Exception arithexc not found in context
  
  
  43
  Int
  1
  2
  6
  3628800
  840
  71
  18
