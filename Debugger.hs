-- replace head and tail with pattern matching to keep functions total
module Debugger where
import System.IO (hFlush, stdout)

import Types (Program, Env, Command)
import Interpreter (execute, initialEnv)
import Parser (commandParser, runParser, resolveError)

-- remaining program, executed program, env, output, input
type ProgramState = (Program, Program, Env, [String], [String])

debug :: Program -> [String] -> IO ()
debug program stdinStream = debugHelper [(program, [], initialEnv, [], stdinStream)]

debugHelper :: [ProgramState] -> IO ()
debugHelper allStates@(state:_) = do
  putStr "\27[34m> "
  hFlush stdout
  option <- getLine
  putStr "\27[0m"
  case option of
    "f" -> debugHelper (stepForward allStates)
    "b" -> debugHelper (stepBackward allStates)
    ('r':num) -> do
      newLine <- getLine
      let command = (resolveError . runParser commandParser) newLine
      let offset = if (null num) then 0 else (read num)
      debugHelper (map (uncurry $ replaceLine command) (zip (map (+ offset) [0..]) allStates))
    "p" -> do
      putStr (showCurrentPosition state)
      debugHelper allStates
    "io" -> do
      putStr (showCurrentIO state)
      debugHelper allStates
    _ -> do
      putStrLn "???"
      debugHelper allStates
debugHelper rest = error . show $ rest

stepForward :: [ProgramState] -> [ProgramState]
stepForward [] = []
stepForward allStates@(([], _, _, _, _):_) = allStates
stepForward allStates@(((next:rest), executed, env, stdoutStream, stdinStream):_) = (rest, next:executed, newEnv, newStdoutStream, newStdinStream):allStates
  where
  (newEnv, newStdoutStream, newStdinStream) = execute (env, stdoutStream, stdinStream) next

stepBackward :: [ProgramState] -> [ProgramState]
stepBackward [] = []
stepBackward (_:rest) = rest

replaceLine :: Command -> Int -> ProgramState -> ProgramState
replaceLine _ _ oldState@([], _, _, _, _) = oldState
replaceLine newCommand 0 ((_:rest), executed, env, stdoutStream, stdinStream) = ((newCommand:rest), executed, env, stdoutStream, stdinStream)
replaceLine newCommand count ((nextLine:rest), executed, env, stdoutStream, stdinStream) = ((nextLine:newProgram), executed, env, stdoutStream, stdinStream)
  where
  (newProgram, _, _, _, _) = replaceLine newCommand (count - 1) (rest, executed, env, stdoutStream, stdinStream)

showCurrentPosition :: ProgramState -> String
showCurrentPosition (remaining, executed, _, _, _) = ((unlines . map show . reverse) executed) ++ "------------\n" ++ ((unlines . map show) remaining)

showCurrentIO :: ProgramState -> String
showCurrentIO (_, _, _, stdoutStream, stdinStream) = unlines ["stdin: ", show stdinStream, "stdout: ", (show . reverse) stdoutStream]
