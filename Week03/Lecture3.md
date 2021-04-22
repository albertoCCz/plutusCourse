# Week03

### Credits

[Alberto Calzada - albertoCCz](https://github.com/albertoCCz)

# Changes due to the Plutus dependencies update

1.	ValidatorCtx -> ScriptContext
2.	When constructing the script address: ScriptAddress -> scriptAddress. Also we don't need anymore the validator hash, so we just pass the validator to the scriptAddress function
3.	Module header doesn't need to be removed when copying code to the playground
4.	Fees are know considered in the playground. They have a fix value (in the playground) of 10 Lovelaces.


# The Context
(Reference: [Contexts.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs))

As we have already seen, the Context, which plays a central role in the _EUTXO model_, is the third argument for the validator function. The data type of this argument has know changed to be `ScriptContext`, which is defined like follows:
```haskell
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```
From the definition we see it has two fields: the `ScriptPurpose` and the `TxInfo`. Let's start by the later and the we explain the most interesting one.

### The `ScriptPurpose`
The `ScriptPurpose` is meant to define, as its name suggests, for which purpose the script is ran. Its definition, which can also be found at the _Contexts.hs_ module we linked previously, is the next one:
```haskell
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```
The most important one for us, at least during this course, will be the `Spending` purpose.
+ `Spending`: to use when the script is run in order to validate spending input for transaction.
+ `Minting`: to use when describing under which circumstances the native token can be minted or burned.

(The other two types where not properly explained, as they are brand new and they won't be used in this course, soon at least)

### The `TxInfo`
The `TxInfo` is what really describes the spending transaction. Again, its definition is in the link, but let us take a look at its definition:
```haskell
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic)
```





