import Test.Tasty
import SrcMapTests
import InvocationTests

main = defaultMain $
  testGroup "Tests" [srcMapTests, invocationTests]
