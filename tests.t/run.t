  $ ../main.exe eval.f
  a b y
  a b
  lambda x. lambda z. z
  lambda x. x
  lambda x. x x
  x y
  lambda x. lambda x'. x' z

  $ ../main.exe sugar.f
  lambda s. lambda z. s (s (s (s (s z))))
  lambda s. lambda z. z
  lambda s. lambda z. s (s (s (s (s (s (s (s (s (s z)))))))))
  lambda t. lambda f. t
  lambda t. lambda f. f
  lambda s. lambda z. z
  lambda s. lambda z. s (s (s (s (s z))))
  lambda s. lambda z. s (s (s (s (s (s (s z))))))
  lambda x. lambda s. lambda z. y s (s (s (s (s (s z)))))
  lambda s. lambda z. s (s (s (s (s (s (s (s z)))))))
  lambda s. lambda z. s (s (s (s (s z))))
  y
  lambda b. b x y
  lambda s. lambda z. z
  y
  lambda b. b (lambda t. lambda f.t) (lambda t. lambda f.t)
  lambda t. lambda f. t
  lambda t. lambda f. t
  lambda t. lambda f. t
  lambda b.
    b (lambda t. lambda f.f)
    (lambda b'.
       b' (lambda s. lambda z.z)
       (lambda b''.
          b'' (lambda t. lambda f.t) (lambda t. lambda f.t)))
  lambda t. lambda f. f
  lambda s. lambda z. s z
  lambda b.
    b (lambda t. lambda f.f)
    (lambda b'.
       b' (lambda s. lambda z. s z)
       (lambda b''.
          b'' (lambda t. lambda f.t) (lambda t. lambda f.t)))
  x
  lambda t. lambda f. f
  lambda t. lambda f. t
  lambda s. lambda z. s z
  lambda s. lambda z. s (s (s (s (s (s z)))))
  lambda t. lambda f. t
  lambda t. lambda f. f
  lambda t. lambda f. t
  lambda t. lambda f. t
  lambda t. lambda f. f

  $ ../main.exe equal.f
  false
  true
  true
  true
  true
  true
  true
  false
  true
