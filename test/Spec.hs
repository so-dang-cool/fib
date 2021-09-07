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
        stdout `shouldSatisfy` (== "fib v1.1") . head . lines
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
