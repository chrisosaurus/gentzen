module Proof.Run
(
    run,
)
where

import Sequent.Data.Sequent (Sequent)
import System.Apply (apply)
import Proof.Data.Theorem
import Proof.Data.SequentTree
import System.Data.System

run :: System -> Theorem -> Either String ([String], SequentTree)
run sy th = do
    sy_name    <- return $ system_name sy
    th_sy_name <- return $ system th
    ()         <- if sy_name == th_sy_name
                    then Right ()
                    else Left $
                        "Wrong system supplied: wanted '" ++ th_sy_name ++ "', but got '" ++ sy_name ++"'"
    steps     <- return $ steps th
    seq       <- return $ sequent th
    -- TODO FIXME this pattern match is ugly
    -- the first step after root has to return a single sequenttree
    -- and the sequent it returns is just the one we give it
    (str, [(_,st)]) <- run' sy [seq] steps
    st        <- return $ Root seq st
    return (str, st)

run' :: System -> [Sequent] -> [Stmt] -> Either String ([String], [(Sequent, SequentTree)])
--          empty sequents and stms means this subproof is done
run' _      []    []          = Right ([], [])
-- if we have no more statements but still have sequents,
-- then we failed to discharge all our proof obligations
run' _      seqs    []        = Left $
    "Failed to discharge remaining proof obligations: " ++ (show seqs)
run' system seqs (stmt:stmts) = case stmt of
    Branch lstmts rstmts -> do
        (lseq, rseq)<- expect_double seqs
        (lstr, lst) <- run' system [lseq] lstmts
        (rstr, rst) <- run' system [rseq] rstmts
        str         <- return $ lstr ++ rstr
        rest        <- return $ lst  ++ rst
        return (str, rest)
    Apply name args -> do
        seq        <- expect_single seqs
        rule       <- lookup_rule system name
        seqs       <- apply seq rule args
        (str, out) <- run' system seqs stmts
        rest       <- return $ Step name args out
        return (str, [(seq, rest)])
    Expect sexp -> do
        seq       <- expect_single seqs
        ()        <- expect sexp seq
        (str, st) <- run' system seqs stmts
        return (str, st)
    Print -> do
        (str, st) <- run' system seqs stmts
        str       <- return $ (show seqs):str
        return (str, st)
    Abort -> do
        seq       <- expect_single seqs
        str       <- return $ "Aborted: " ++ (show seq)
        return ([str], [(seq, Aborted)])

expect_single :: [Sequent] -> Either String Sequent
expect_single []     = Left "No sequent found"
expect_single (x:[]) = Right x
expect_single _      = Left "More than one sequent found"

expect_double :: [Sequent] -> Either String (Sequent,Sequent)
expect_double []       = Left "No sequents found"
expect_double (x:[])   = Left "Only one sequent found"
expect_double (x:y:[]) = Right (x,y)
expect_double _        = Left "More than two sequents found"


expect :: Sequent -> Sequent -> Either String ()
expect exp got | exp == got = Right ()
expect exp got              = Left $
    "Expectaton failed\n\texpected: " ++ (show exp) ++ "\n\tgot: " ++ (show got)
