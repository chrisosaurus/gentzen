module Proof.Run
(
    run,
    run',
    step,
)
where


import Sequent.Data.Sequent (Sequent)
import System.Apply (apply)
import Proof.Data.Theorem
import qualified Proof.Data.Proof as Proof
import System.Data.System

check_system :: System -> Theorem -> Either String ()
check_system sy th = do
    sy_name    <- return $ system_name sy
    th_sy_name <- return $ system th
    if sy_name == th_sy_name
        then Right ()
        else Left $
            "Wrong system supplied: wanted '" ++ th_sy_name ++ "', but got '" ++ sy_name ++ "'"

run :: System -> Theorem -> Either String ([String], Proof.Proof)
run sy th = do
    ()            <- check_system sy th
    steps         <- return $ steps th
    seq           <- return $ sequent th
    proof         <- return $ Proof.start seq
    (strs, proof) <- run' sy proof steps
    if Proof.finished proof
        then Right ([], proof)
        else Left $
            "Failed to discharge remaining proof obligations: " ++
            (show (Proof.unproven proof)) ++ "\n" ++ (show proof)

run' :: System -> Proof.Proof -> [Stmt] -> Either String ([String], Proof.Proof)
run' _  proof [] = Right ([], proof)
run' sy proof (stmt:stmts) = do
    (strs,  proof) <- step sy proof stmt
    (strs', proof) <- run' sy proof stmts
    return (strs++strs', proof)

step :: System -> Proof.Proof -> Stmt -> Either String ([String], Proof.Proof)
step sy proof stmt = case stmt of
    Branch lstmts rstmts -> do
        (lstrs, proof) <- run' sy proof lstmts
        (rstrs, proof) <- run' sy proof rstmts
        return (lstrs ++ rstrs, proof)

    Apply name args -> do
        id     <- Proof.tip proof
        iseq   <- Proof.get id proof
        rule   <- lookup_rule sy name
        oseqs  <- apply iseq rule args
        (ids, proof)  <- return $ Proof.addSeqs oseqs proof
        step   <- case ids of
            []     -> Right $ Proof.Axiom    id name args
            x:[]   -> Right $ Proof.Straight id name args x
            x:y:[] -> Right $ Proof.Split    id name args x y
            _      -> Left  $ "Error: too many ids, found '" ++ (show (length ids)) ++ "'"
        proof  <- return $ Proof.addStep step proof
        return ([], proof)

    Expect sexp -> do
        id  <- Proof.tip proof
        seq <- Proof.get id proof
        ()  <- expect sexp seq
        return ([], proof)

    Print -> do
        id            <- Proof.tip proof
        seq           <- Proof.get id proof
        str           <- return $ show seq
        return ([str], proof)

    Abort -> do
        id    <- Proof.tip proof
        proof <- return $ Proof.abortSeq id proof
        step  <- return $ Proof.Abort id
        proof <- return $ Proof.addStep step proof
        return ([], proof)

expect :: Sequent -> Sequent -> Either String ()
expect exp got | exp == got = Right ()
expect exp got              = Left $
    "Expectaton failed\n\texpected: " ++ (show exp) ++ "\n\tgot: " ++ (show got)

