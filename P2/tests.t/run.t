  $ ../main.exe test.f
  Type Checking: 5+6
  Int
  Type Checking: 5+(if false then 0 else 5)
  Int
  Evaluating: 5+(lambda x:Int.lambda y:Int.x+y)
  $TESTCASE_ROOT/test.f:3.3:
  Mismatched types: Not an integer
  
  
  [1]

  $ ../main.exe test1.f
  Type Checking: lambda x:Int.lambda y:Int.x+y
  Int->Int->Int
  Type Checking: (1=2)||(6=(((3*2)-5)-(6*1)))
  Bool
  Type Checking: lambda x:Int->Bool->Unit.x
  (Int->Bool->Unit)->Int->Bool->Unit
  Type Checking: let x=5=5 in x
  Bool
  Evaluating: exception arithexc of Unit in let div=lambda x:Int.lambda y:Int.if y=0 then throw arithexc unit as Int else x/y in try div 2 0 catch {arithexc c => 52}{arithexc _ => 42}
  52
  Evaluating: exception ex1 of Unit in try throw ex1 unit as Int catch {ex1 _ => 43}
  43
  Type Checking: (lambda x:Int.5*x) 5
  Int
  Evaluating: fix (lambda f:Int->Int.lambda n:Int.if n=0 then 1 else n*(f (n-1))) 0
  1
  Evaluating: fix (lambda f:Int->Int.lambda n:Int.if n=0 then 1 else n*(f (n-1))) 2
  2
  Evaluating: fix (lambda f:Int->Int.lambda n:Int.if n=0 then 1 else n*(f (n-1))) 3
  6
  Evaluating: fix (lambda f:Int->Int.lambda n:Int.if n=0 then 1 else n*(f (n-1))) 10
  3628800

  $ ../main.exe test2.f
  Evaluating: ((((((((2+2)+5)+2)+3)+5)+2)+3)+4)+2
  30
  Evaluating: (2+5)=(3+3)
  false
  Evaluating: if (5+5)=10 then 5 else 10
  5
  Evaluating: ((2*5)-6)-((2*8)/2)
  -4
  Evaluating: (2=5)||(10=(2*5))
  true
  Evaluating: (2=5)&&(10=(2*5))
  false
  Evaluating: (lambda x:Int.x+1) 2
  3
  Evaluating: (lambda x:Int.lambda y:Int.x+y) 2 3
  5
  Evaluating: (lambda x:Int.(lambda x:Int.lambda y:Int.x+y) (x+1)) 4 5
  10
  Evaluating: (lambda x:Int.lambda x:Int.x) 2 3
  3
  Evaluating: let x=42 in let y=x in x+y
  84
