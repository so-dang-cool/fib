import System.Exit
import System.Process
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "[stdin] fib" $ do
    context "given 0" $ do
      let run = fibCli [] "0"
      it "prints 0" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "0\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given [0 .. 10] from stdin" $ do
      let run = fibCli [] (unlines $ map show [0 .. 10])
      it "prints sequence as lines" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given nothing from stdin" $ do
      let run = fibCli [] ""
      it "prints nothing" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` ""
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given non-digits from stdin" $ do
      let run = fibCli [] "abc"
      it "prints \"ERROR: ...\" to stdout" $ do
        (_, stdout, _) <- run
        stdout `shouldSatisfy` ((== "ERROR:") . take 6)
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given non-digits from stdin (some lines ok)" $ do
      let run = fibCli [] "1\nabc\n4"
      it "prints \"ERROR: ...\" to stdout (only second line)" $ do
        (_, stdout, _) <- run
        lines stdout `shouldSatisfy` \[one, fail, three] ->
          one == "1"
            && ((== "ERROR:") . take 6 $ fail)
            && three == "3"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

  describe "[args] fib <N>..." $ do

    context "given 0 from args" $ do
      let run = fibCli ["0"] ""
      it "prints 0" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "0\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given [0 .. 10] from args" $ do
      let run = fibCli (map show [0 .. 10]) ""
      it "prints sequence as lines" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given 1001 (the final precalculated number) from args" $ do
      let run = fibCli ["1001"] ""
      it "prints the correct number" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given 1002 (the first calculated number) from args" $ do
      let run = fibCli ["1002"] ""
      it "prints the correct number" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "113796925398360272257523782552224175572745930353730513145086634176691092536145985470146129334641866902783673042322088625863396052888690096969577173696370562180400527049497109023054114771394568040040412172632376\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

  describe "[seq] fib --until N" $ do

    context "given \"--until 10\"" $ do
      let run = fibCli ["--until", "10"] ""
      it "prints sequence as lines" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given \"--until 1001\" (the final precalculated number)" $ do
      let run = fibCli ["--until", "1001"] ""
      it "ends with the correct number" $ do
        (_, stdout, _) <- run
        (last . lines $ stdout) `shouldBe` "70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given \"--until 1002\" (the first calculated number)" $ do
      let run = fibCli ["--until", "1002"] ""
      it "ends with the correct number" $ do
        (_, stdout, _) <- run
        (last . lines $ stdout) `shouldBe` "113796925398360272257523782552224175572745930353730513145086634176691092536145985470146129334641866902783673042322088625863396052888690096969577173696370562180400527049497109023054114771394568040040412172632376"
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

    context "given \"--until\" (missing argument to --until)" $ do
      let run = fibCli ["--until"] ""
      it "prints nothing to stdout" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` ""
      it "exits unsuccessfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` (ExitFailure 1)
      it "prints \"USAGE: ...\" to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldSatisfy` ((== "USAGE:") . take 6)

    context "given \"--until 1 2\" (too many arguments to --until)" $ do
      let run = fibCli ["--until", "1", "2"] ""
      it "prints nothing to stdout" $ do
        (_, stdout, _) <- run
        stdout `shouldBe` ""
      it "exits unsuccessfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` (ExitFailure 1)
      it "prints \"USAGE: ...\" to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldSatisfy` ((== "USAGE:") . take 6)

  describe "[help] fib --help" $ do

    context "given \"--help\"" $ do
      let run = fibCli ["--help"] ""
      it "prints name and version to stdout" $ do
        (_, stdout, _) <- run
        stdout `shouldSatisfy` (== "fib v1.1.2") . head . lines
      it "exits successfully" $ do
        (exitCode, _, _) <- run
        exitCode `shouldBe` ExitSuccess
      it "prints nothing to stderr" $ do
        (_, _, stderr) <- run
        stderr `shouldBe` ""

-- Helpers

type Args = [String]

type StdIn = String

fibCli :: Args -> StdIn -> IO (ExitCode, String, String)
fibCli = readProcessWithExitCode "./test/run.sh"
