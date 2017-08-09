import System.Process
import Data.String.Utils
import Control.Monad
import System.IO
import System.Exit (ExitCode(ExitSuccess))

dockerbin = "/usr/local/bin/docker"
herokubin = "/usr/local/heroku/bin/heroku"
awkbin = "/usr/bin/awk"
datebin = "/usr/local/opt/coreutils/libexec/gnubin/date"

main :: IO ()
main = do
    p <- fmap lines $ readProcess dockerbin ["ps", "-a", "-q"] []
    (_, dlog1, delog1) <- readProcessWithExitCode dockerbin ("rm":p) []
    putStrLn dlog1
    putStrLn delog1
    (_, Just hout, _, _) <- createProcess (proc dockerbin ["images"]){std_out = CreatePipe}
    (Just hin, Just hout2, _, _) <- createProcess
        (proc awkbin ["/^<none>/ { print $3 }"]){std_in = CreatePipe, std_out = CreatePipe}
    hout' <- hGetContents hout
    hPutStr hin hout'
    q <- fmap lines $ hGetContents hout2
    (_, dlog2, delog2) <- readProcessWithExitCode dockerbin ("rmi":q) []
    putStrLn dlog2
    putStrLn delog2

    startday <- fmap strip $ readProcess datebin ["+%Y/%m/%d"] []
    starttim <- fmap strip $ readProcess datebin ["+%H:%M:%S"] []

    (hexcode, hlog, helog) <- readProcessWithExitCode herokubin ["container:push", "worker"] []
    putStrLn hlog

    endday <- fmap strip $ readProcess datebin ["+%Y/%m/%d"] []
    endtim <- fmap strip $ readProcess datebin ["+%H:%M:%S"] []

    putStrLn $
        "Build started: " ++ startday ++ " " ++ starttim
        ++ " -> Finished: " ++ endday ++ " " ++ endtim

