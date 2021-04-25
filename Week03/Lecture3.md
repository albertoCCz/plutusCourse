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
The `TxInfo` is what really describes the pending transaction. Again, its definition is in the link, but let us take a look at it:
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
We see that this data type has many fields, which are global to the transaction. The first and third fields, `txInfoInputs` and `txInfoOutputs` respectively, are particular examples of global fields. They are a list of all the inputs and outputs of the given transaction:
```haskell
txInfoInputs  :: [TxInInfo] -- ^ Transaction inputs
txInfoOutputs :: [TxOut] -- ^ Transaction outputs
```
For those of you who want to go deeper, you can find the definition of the data type `TxInInfo` in the same module `Contexts.hs` and the definiton of the data type `TxOut` at the module [`Tx.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs). In the end, the fields on the definition of `TxInInfo` also depend on data types defined on this last module, so try to check it out if you have time.

Other global fields are the `txInfoFee`, for the fee paid by the transaction and the `txInfoForge`, which is related to the processes of minting and burning tokens, being non-zero in those cases. Then we have the `txInfoValidRange`, which we will be working with extensively during this lecture (so we don't need to explain it now), the `txInfoSignatories`, which is a list of all the signatures attached to the transactions, the `txInfoData`, which is a list of key-value pairs from `DatumHash` to `Datum`, being the second argument optional, and finally the field `TxInfoId`, which is just the hash of the transaction.

# Handling time
One of the characteristics of Plutus' smart contracts is that they behave in a deterministic way. That way, you can know if a transaction will be validated before you submit it to a node from your wallet. Obviously the transaction could still fail because the output was consumed by the moment your transaction has to be validated, but let's leave it aside for now. But it might be difficult to put together this deterministic behaviour with the fact time is always passing. But, why might it be difficult? Well, it could be the case you want to make a transaction that have a deadeline so you try it in your wallet and it validates, because you do it on time, but then you submit it to the node and when it has to be validated the time is beyond the deadline. This can't be allowed if we want a deterministic behaviour, so here is when the `txInfoValidRange` field from the `TxInfo`data type comes into play.
