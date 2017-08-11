module Src.Modules.Calculater (
    onlyTwoDecimalDigits,
    estimatePert
) where

onlyTwoDecimalDigits :: Float -> Float
onlyTwoDecimalDigits n =
    let nHundred = n * 100
        nRound = round nHundred :: Int
        nString = show nRound
        nFloat = read nString :: Float
    in nFloat / 100

estimatePert:: Float -> Float -> Float -> Float
estimatePert likely worst best =
    let pertMinutes = ((likely * 4) + worst + best) / 6
    in onlyTwoDecimalDigits pertMinutes
