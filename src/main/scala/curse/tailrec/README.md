 # 执行程 
  用FP的替换原则理解Demo3中run(factorial(3))的factorial和run的交替执行过程
  假设 f = ((i: BigDecimal) => factorial(i - 1).map(j => i * j))
  
  ```
  run(factorial(3))
  ---  factorial 控制 ----------------------------------------------
  = run(
     FlatMap(Return(3), f)
    )
  ---  run 控制 ---------------------------------------------------
  = run(
        factorial(3 - 1).map(j => 3 * j)
    )
  = run(
       FlatMap(Return(2), f).map(j => 3 * j)
    )
  = run(
       FlatMap(Return(2), f).flatMap(j => Return (3 * j) )
    )
  = run(
       FlatMap(
           FlatMap( Return(2), f ), (j => Return (3 * j) )
    )
  ---  run 控制 ---------------------------------------------------
  = run(
         Return(2).flatMap( i => f(i).flatMap(j => Return(3 * j) )
    )
  = run(
         FlatMap(
                 Return(2),
                 ( i => f(i).flatMap( j => Return(3 * j) ))
         )
    )
  
  --第一次 FlatMap(FlatMap(x, f), g) 转换成 FlatMap(x, f flatMap g)--
  
  = run(
       f(2).flatMap( j => Return(3 * j) )
    )
  = run(
       factorial(2 - 1).map(j => 2 * j).flatMap( j => Return(3 * j) )
    )
  ---  factorial 控制 ----------------------------------------------
  = run(
       FlatMap( Return(1), f ).map(j => 2 * j).flatMap(j => Return(3 * j) )
    )
  = run(
       FlatMap( Return(1), f).flatMap(j => Return( 2 * j ))
                             .flatMap(j => Return( 3 * j ))
   )
  = run(
      FlatMap(
         FlatMap(Return(1), f), (j => Return ( 2 * j ))
      ).flatMap(j => Return( 3 * j ))
   )
  = run(
      FlatMap(
         FlatMap(FlatMap( Return(1), f), (j => Return ( 2 * j ))),
         (j => Return ( 3 * j ))
      )
    )
  = run(
     FlatMap( Return(1), f )
       .flatMap(j =>
           Return ( 2 * j ).flatMap( j => Return ( 3 * j ) )
        )
  = run(
     FlatMap( FlatMap( Return(1), f ),
              (j => Return ( 2 * j ).flatMap( j => Return ( 3 * j ))
            )
   )
  ---  run 控制 ---------------------------------------------------
  = run(
     Return(1).flatMap(i => f(i).flatMap(j => Return ( 2 * j ).flatMap( j => Return ( 3 * j ))
   )
  = run(
     FlatMap(Return(1),
             i => f(i).flatMap(j => Return ( 2 * j ).flatMap( j => Return ( 3 * j ))
    )
  = run(
     f(1).flatMap(j => Return(2 * j).flatMap(j => Return(3*j))
  )
  = run(
     factorial(0).map(j => 1 * j).flatMap(j => Return(2 * j).flatMap(j => Return(3 * j))
   )
  ---  factorial 控制 ----------------------------------------------
  = run(
      Return(1).map(j => 1 * j).flatMap(j => Return(2 * j).flatMap(j => Return(3 * j) )
   )
  = run(
      Return(1).flatMap(j => Return(1 * j)).flatMap(j => Return(2 * j).flatMap(j=>Return(3*j))
   )
  = run(
      FlatMap(Return(1),
      j => Return(1 * j).flatMap(j => Return(2 * j).flatMap(j=>Return(3*j)
      )
    )
  --- run 控制 -----------------------------------------------------
  = run(
     (j => Return(1 * j).flatMap(j => Return(2 * j).flatMap(j=>Return(3*j))(1)
   )
  = run(
     Return(1 * 1).flatMap(j => Return(2 * j).flatMap(j => Return(3 * j))
   )
  = run(
    FlatMap( Return(1), (j => Return(2 * j).flatMap(j => Return(3 * j))
   )
  = run(
    Return(2 * 1).flatMap(j => Return(3 * j))
   )
  = run(
    FlatMap( Return(2), (j => Return(3 * j) )
   )
  = run(
    Return(3 * 2)
   )
  = 6
  ```
