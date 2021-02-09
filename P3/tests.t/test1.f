lambda x. <lambda x.x, unit>;
case unit of in1 x => unit | in2 x => unit;
(lambda x. case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,unit>;
(lambda x.x) unit;
lambda f. (lambda x. f (x x)) (lambda x. f (x x));
(lambda f. lambda a. lambda b. <f a, f b>) (lambda x.x) unit <unit,unit>;
lambda y.abort ((lambda x.y) unit);
lambda x. abort x;
(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in1 <unit,lambda x.unit>;
(lambda x.case x of in1 y => p1 y | in2 y => p2 y) in2 <unit,lambda x.unit>;
p1 <lambda x.x,unit>;
(lambda z. 
    (lambda x.case x of in1 y => p1 y | in2 y => p2 y) 
    in1 z) <unit,lambda x.unit>;
abort x;
abort unit;
(lambda x. abort x) (in1 unit);
lambda x. lambda y. lambda z. (p2 x) (p2 y) (p1 z);