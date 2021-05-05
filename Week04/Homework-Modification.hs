{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Control.Monad.Freer.Extras as Extras
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import GHC.Generics               (Generic)
import Ledger
import Ledger.Ada                 as Ada
import Ledger.Constraints         as Constraints
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet
import Data.Void                  (Void)

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = BlockchainActions .\/ Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

payContractErrorHandler :: Contract () PaySchema Void ()
payContractErrorHandler = Contract.handleError
    (\err -> Contract.logError $ "Caught error: " ++ unpack err)
    payContract

payTrace :: Integer -> [Integer] -> EmulatorTrace ()
payTrace _ []     = void $ Extras.logInfo @String "No more transactions to do"
payTrace n (x:xs) = do
    h <- activateContractWallet (Wallet 1) payContractErrorHandler
    callEndpoint @"pay" h PayParams{ppRecipient=getPKHash n, ppLovelace=x}
    void $ Emulator.waitNSlots 1
    payTrace n xs
  where
    getPKHash :: Integer -> PubKeyHash
    getPKHash i = pubKeyHash $ walletPubKey $ Wallet i

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 2 [1000000]

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 3 [1000000, 1000000]

payTest3 :: IO ()
payTest3 = runEmulatorTraceIO $ payTrace 4 [1000000, 1000000, 2000000]