  $ ../main.exe test1.f
  Unit -> (Unit -> Unit) x Unit
  Error: Unification failed: Can't apply constraint: Unit = v0 + v1 (Unit = v0 + v1)
  Unit
  Unit
  Error: Unification failed: Infinitely unifiable term
  Error: Unification failed: Can't apply constraint: Unit x Unit = Unit (Unit x Unit = Unit)
  Void -> Unit
  Void -> Unit
  Unit
  Unit -> Unit
  Unit -> Unit
  Unit
  $TESTCASE_ROOT/test1.f:15.7:
  Error: Variable type lookup failure: Variable x not found in context
  Error: Unification failed: Can't apply constraint: Unit = Void (Unit = Void)
  Error: Unification failed: Can't apply constraint: v0 = Void (Unit + v2 = Void)
  (Unit x (Unit -> Unit -> Unit)) -> (Unit x Unit) -> (Unit x Unit) -> Unit

  $ ../main.exe -v test1.f
  
  Type Checking: lambda x.<lambda x.x,unit>
  Before unification:
  x : CType v1
  lambda x.x : CType v1 -> v1
  unit : CType Unit
  <lambda x.x,unit> : CType (v1 -> v1) x Unit
  lambda x.<lambda x.x,unit> : CType v0 -> (v1 -> v1) x Unit
  Constraints:
  After unification:
  x : Type Unit
  lambda x.x : Type Unit -> Unit
  unit : CType Unit
  <lambda x.x,unit> : Type (Unit -> Unit) x Unit
  lambda x.<lambda x.x,unit> : Type Unit -> (Unit -> Unit) x Unit
  Unit -> (Unit -> Unit) x Unit
  
  Type Checking: case unit of in1 x => unit | in2 x => unit
  Before unification:
  unit : CType Unit
  unit : CType Unit
  unit : CType Unit
  case unit of in1 x => unit | in2 x => unit : CType Unit
  Constraints:
  Unit = v0 + v1
  Unit = Unit
  After unification:
  Error: Unification failed: Can't apply constraint: Unit = v0 + v1 (Unit = v0 + v1)
  
  Type Checking: (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,unit>
  Before unification:
  x : CType v0
  y : CType v1
  p1 y : CType v3
  y : CType v2
  p2 y : CType v6
  case x of in1 y => p1 y | in2 y => p2 y : CType v3
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : CType v0 -> v3
  unit : CType Unit
  unit : CType Unit
  <unit,unit> : CType Unit x Unit
  in1 <unit,unit> : CType (Unit x Unit) + v7
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,unit> : CType v8
  Constraints:
  ((Unit x Unit) + v7) -> v8 = v0 -> v3
  v0 = v1 + v2
  v3 = v6
  v1 = v3 x v4
  v2 = v5 x v6
  After unification:
  x : Type (Unit x Unit) + (Unit x Unit)
  y : Type Unit x Unit
  p1 y : Type Unit
  y : Type Unit x Unit
  p2 y : Type Unit
  case x of in1 y => p1 y | in2 y => p2 y : Type Unit
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : Type ((Unit x Unit) + (Unit x Unit)) -> Unit
  unit : CType Unit
  unit : CType Unit
  <unit,unit> : Type Unit x Unit
  in1 <unit,unit> : Type (Unit x Unit) + (Unit x Unit)
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,unit> : Type Unit
  Unit
  
  Type Checking: (lambda x.x) unit
  Before unification:
  x : CType v0
  lambda x.x : CType v0 -> v0
  unit : CType Unit
  (lambda x.x) unit : CType v1
  Constraints:
  Unit -> v1 = v0 -> v0
  After unification:
  x : Type Unit
  lambda x.x : Type Unit -> Unit
  unit : CType Unit
  (lambda x.x) unit : Type Unit
  Unit
  
  Type Checking: lambda f.(lambda x.f (x x)) (lambda x.f (x x))
  Before unification:
  f : CType v0
  x : CType v1
  x : CType v1
  x x : CType v2
  f (x x) : CType v3
  lambda x.f (x x) : CType v1 -> v3
  f : CType v0
  x : CType v4
  x : CType v4
  x x : CType v5
  f (x x) : CType v6
  lambda x.f (x x) : CType v4 -> v6
  (lambda x.f (x x)) (lambda x.f (x x)) : CType v7
  lambda f.(lambda x.f (x x)) (lambda x.f (x x)) : CType v0 -> v7
  Constraints:
  (v4 -> v6) -> v7 = v1 -> v3
  v2 -> v3 = v0
  v1 -> v2 = v1
  v5 -> v6 = v0
  v4 -> v5 = v4
  After unification:
  Error: Unification failed: Infinitely unifiable term
  
  Type Checking: (lambda f.lambda a.lambda b.<f a,f b>) (lambda x.x) unit <unit,unit>
  Before unification:
  f : CType v0
  a : CType v1
  f a : CType v3
  f : CType v0
  b : CType v2
  f b : CType v4
  <f a,f b> : CType v3 x v4
  lambda b.<f a,f b> : CType v2 -> v3 x v4
  lambda a.lambda b.<f a,f b> : CType v1 -> v2 -> v3 x v4
  lambda f.lambda a.lambda b.<f a,f b> : CType v0 -> v1 -> v2 -> v3 x v4
  x : CType v5
  lambda x.x : CType v5 -> v5
  (lambda f.lambda a.lambda b.<f a,f b>) (lambda x.x) : CType v6
  unit : CType Unit
  (lambda f.lambda a.lambda b.<f a,f b>) (lambda x.x) unit : CType v7
  unit : CType Unit
  unit : CType Unit
  <unit,unit> : CType Unit x Unit
  (lambda f.lambda a.lambda b.<f a,f b>) (lambda x.x) unit <unit,unit> : CType v8
  Constraints:
  (Unit x Unit) -> v8 = v7
  Unit -> v7 = v6
  (v5 -> v5) -> v6 = v0 -> v1 -> v2 -> v3 x v4
  v1 -> v3 = v0
  v2 -> v4 = v0
  After unification:
  Error: Unification failed: Can't apply constraint: Unit x Unit = Unit (Unit x Unit = Unit)
  
  Type Checking: lambda y.abort ((lambda x.y) unit)
  Before unification:
  y : CType v0
  lambda x.y : CType v1 -> v0
  unit : CType Unit
  (lambda x.y) unit : CType v2
  abort ((lambda x.y) unit) : CType v3
  lambda y.abort ((lambda x.y) unit) : CType v0 -> v3
  Constraints:
  v2 = Void
  Unit -> v2 = v1 -> v0
  After unification:
  y : Type Void
  lambda x.y : Type Unit -> Void
  unit : CType Unit
  (lambda x.y) unit : Type Void
  abort ((lambda x.y) unit) : Type Unit
  lambda y.abort ((lambda x.y) unit) : Type Void -> Unit
  Void -> Unit
  
  Type Checking: lambda x.abort x
  Before unification:
  x : CType v0
  abort x : CType v1
  lambda x.abort x : CType v0 -> v1
  Constraints:
  v0 = Void
  After unification:
  x : Type Void
  abort x : Type Unit
  lambda x.abort x : Type Void -> Unit
  Void -> Unit
  
  Type Checking: (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,lambda x.unit>
  Before unification:
  x : CType v0
  y : CType v1
  p1 y : CType v3
  y : CType v2
  p2 y : CType v6
  case x of in1 y => p1 y | in2 y => p2 y : CType v3
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : CType v0 -> v3
  unit : CType Unit
  unit : CType Unit
  lambda x.unit : CType v7 -> Unit
  <unit,lambda x.unit> : CType Unit x (v7 -> Unit)
  in1 <unit,lambda x.unit> : CType (Unit x (v7 -> Unit)) + v8
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,lambda x.unit> : CType v9
  Constraints:
  ((Unit x (v7 -> Unit)) + v8) -> v9 = v0 -> v3
  v0 = v1 + v2
  v3 = v6
  v1 = v3 x v4
  v2 = v5 x v6
  After unification:
  x : Type (Unit x (Unit -> Unit)) + (Unit x Unit)
  y : Type Unit x (Unit -> Unit)
  p1 y : Type Unit
  y : Type Unit x Unit
  p2 y : Type Unit
  case x of in1 y => p1 y | in2 y => p2 y : Type Unit
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : Type ((Unit x (Unit -> Unit)) + (Unit x Unit)) -> Unit
  unit : CType Unit
  unit : CType Unit
  lambda x.unit : Type Unit -> Unit
  <unit,lambda x.unit> : Type Unit x (Unit -> Unit)
  in1 <unit,lambda x.unit> : Type (Unit x (Unit -> Unit)) + (Unit x Unit)
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,lambda x.unit> : Type Unit
  Unit
  
  Type Checking: (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in2 <unit,lambda x.unit>
  Before unification:
  x : CType v0
  y : CType v1
  p1 y : CType v3
  y : CType v2
  p2 y : CType v6
  case x of in1 y => p1 y | in2 y => p2 y : CType v3
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : CType v0 -> v3
  unit : CType Unit
  unit : CType Unit
  lambda x.unit : CType v7 -> Unit
  <unit,lambda x.unit> : CType Unit x (v7 -> Unit)
  in2 <unit,lambda x.unit> : CType v8 + (Unit x (v7 -> Unit))
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in2 <unit,lambda x.unit> : CType v9
  Constraints:
  (v8 + (Unit x (v7 -> Unit))) -> v9 = v0 -> v3
  v0 = v1 + v2
  v3 = v6
  v1 = v3 x v4
  v2 = v5 x v6
  After unification:
  x : Type ((Unit -> Unit) x Unit) + (Unit x (Unit -> Unit))
  y : Type (Unit -> Unit) x Unit
  p1 y : Type Unit -> Unit
  y : Type Unit x (Unit -> Unit)
  p2 y : Type Unit -> Unit
  case x of in1 y => p1 y | in2 y => p2 y : Type Unit -> Unit
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : Type (((Unit -> Unit) x Unit) + (Unit x (Unit -> Unit))) -> Unit -> Unit
  unit : CType Unit
  unit : CType Unit
  lambda x.unit : Type Unit -> Unit
  <unit,lambda x.unit> : Type Unit x (Unit -> Unit)
  in2 <unit,lambda x.unit> : Type ((Unit -> Unit) x Unit) + (Unit x (Unit -> Unit))
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in2 <unit,lambda x.unit> : Type Unit -> Unit
  Unit -> Unit
  
  Type Checking: p1 <lambda x.x,unit>
  Before unification:
  x : CType v2
  lambda x.x : CType v2 -> v2
  unit : CType Unit
  <lambda x.x,unit> : CType (v2 -> v2) x Unit
  p1 <lambda x.x,unit> : CType v0
  Constraints:
  (v2 -> v2) x Unit = v0 x v1
  After unification:
  x : Type Unit
  lambda x.x : Type Unit -> Unit
  unit : CType Unit
  <lambda x.x,unit> : Type (Unit -> Unit) x Unit
  p1 <lambda x.x,unit> : Type Unit -> Unit
  Unit -> Unit
  
  Type Checking: (lambda z.(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z) <unit,lambda x.unit>
  Before unification:
  x : CType v1
  y : CType v2
  p1 y : CType v4
  y : CType v3
  p2 y : CType v7
  case x of in1 y => p1 y | in2 y => p2 y : CType v4
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : CType v1 -> v4
  z : CType v0
  in1 z : CType v0 + v8
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z : CType v9
  lambda z.(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z : CType v0 -> v9
  unit : CType Unit
  unit : CType Unit
  lambda x.unit : CType v10 -> Unit
  <unit,lambda x.unit> : CType Unit x (v10 -> Unit)
  (lambda z.(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z) <unit,lambda x.unit> : CType v11
  Constraints:
  (Unit x (v10 -> Unit)) -> v11 = v0 -> v9
  (v0 + v8) -> v9 = v1 -> v4
  v1 = v2 + v3
  v4 = v7
  v2 = v4 x v5
  v3 = v6 x v7
  After unification:
  x : Type (Unit x (Unit -> Unit)) + (Unit x Unit)
  y : Type Unit x (Unit -> Unit)
  p1 y : Type Unit
  y : Type Unit x Unit
  p2 y : Type Unit
  case x of in1 y => p1 y | in2 y => p2 y : Type Unit
  lambda x.case x of in1 y => p1 y | in2 y => p2 y : Type ((Unit x (Unit -> Unit)) + (Unit x Unit)) -> Unit
  z : Type Unit x (Unit -> Unit)
  in1 z : Type (Unit x (Unit -> Unit)) + (Unit x Unit)
  (lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z : Type Unit
  lambda z.(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z : Type (Unit x (Unit -> Unit)) -> Unit
  unit : CType Unit
  unit : CType Unit
  lambda x.unit : Type Unit -> Unit
  <unit,lambda x.unit> : Type Unit x (Unit -> Unit)
  (lambda z.(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 z) <unit,lambda x.unit> : Type Unit
  Unit
  
  Type Checking: abort x
  $TESTCASE_ROOT/test1.f:15.7:
  Error: Variable type lookup failure: Variable x not found in context
  
  Type Checking: abort unit
  Before unification:
  unit : CType Unit
  abort unit : CType v0
  Constraints:
  Unit = Void
  After unification:
  Error: Unification failed: Can't apply constraint: Unit = Void (Unit = Void)
  
  Type Checking: (lambda x.abort x) in1 unit
  Before unification:
  x : CType v0
  abort x : CType v1
  lambda x.abort x : CType v0 -> v1
  unit : CType Unit
  in1 unit : CType Unit + v2
  (lambda x.abort x) in1 unit : CType v3
  Constraints:
  (Unit + v2) -> v3 = v0 -> v1
  v0 = Void
  After unification:
  Error: Unification failed: Can't apply constraint: v0 = Void (Unit + v2 = Void)
  
  Type Checking: lambda x.lambda y.lambda z.p2 x p2 y p1 z
  Before unification:
  x : CType v0
  p2 x : CType v4
  y : CType v1
  p2 y : CType v6
  p2 x p2 y : CType v7
  z : CType v2
  p1 z : CType v8
  p2 x p2 y p1 z : CType v10
  lambda z.p2 x p2 y p1 z : CType v2 -> v10
  lambda y.lambda z.p2 x p2 y p1 z : CType v1 -> v2 -> v10
  lambda x.lambda y.lambda z.p2 x p2 y p1 z : CType v0 -> v1 -> v2 -> v10
  Constraints:
  v8 -> v10 = v7
  v6 -> v7 = v4
  v0 = v3 x v4
  v1 = v5 x v6
  v2 = v8 x v9
  After unification:
  x : Type Unit x (Unit -> Unit -> Unit)
  p2 x : Type Unit -> Unit -> Unit
  y : Type Unit x Unit
  p2 y : Type Unit
  p2 x p2 y : Type Unit -> Unit
  z : Type Unit x Unit
  p1 z : Type Unit
  p2 x p2 y p1 z : Type Unit
  lambda z.p2 x p2 y p1 z : Type (Unit x Unit) -> Unit
  lambda y.lambda z.p2 x p2 y p1 z : Type (Unit x Unit) -> (Unit x Unit) -> Unit
  lambda x.lambda y.lambda z.p2 x p2 y p1 z : Type (Unit x (Unit -> Unit -> Unit)) -> (Unit x Unit) -> (Unit x Unit) -> Unit
  (Unit x (Unit -> Unit -> Unit)) -> (Unit x Unit) -> (Unit x Unit) -> Unit
