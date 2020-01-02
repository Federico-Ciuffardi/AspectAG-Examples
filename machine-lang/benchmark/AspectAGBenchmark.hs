import Criterion.Main
import MachineLangInterpreter

main = defaultMain [
  bgroup "Interpreter"
    [ bench "test0" $ nfIO test0
    , bench "test1" $ nfIO test1_1
    , bench "test2" $ nfIO test2
    , bench "test3" $ nfIO test3_1
    , bench "test4" $ nfIO test4
    , bench "test5" $ nfIO test5
    , bench "test6" $ nfIO test6
    , bench "test7" $ nfIO test7
    , bench "test8" $ nfIO test8
    , bench "test9" $ nfIO test9_1
    , bench "test10" $ nfIO test10
    , bench "test14" $ nfIO test14
    , bench "test15" $ nfIO test15
    , bench "test16" $ nfIO test16
    , bench "test17" $ nfIO test17_2

    ]
  ]
iterations_bench = defaultMain [
  bgroup "Interpreter"
    [ bench "i = 1" $ nfIO (test_iterate 1)
    , bench "i = 10" $ nfIO (test_iterate 10)
    , bench "i = 100" $ nfIO (test_iterate 100)
    , bench "i = 1000" $ nfIO (test_iterate 1000)
    , bench "i = 10000" $ nfIO (test_iterate 10000)
    , bench "i = 100000" $ nfIO (test_iterate 100000)
    ]
  ]