# Week04

### Credits

[Alberto Calzada - albertoCCz](https://github.com/albertoCCz)

## Summary
In this lecture we are going to take a look at the next topics:
- Values and native tokens
- Minting policies
- NFTs

## 1. Values and native tokens
(References: [Ada.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs), [Contexts.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs), [Value.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs), all in package: [plutus-ledger-api](https://github.com/input-output-hk/plutus/tree/master/plutus-ledger-api))

Let us first talk about Value. Up until now, when talking about the value associated with an UTxO, we treated it as a simple value: 100 Ada, 27 Lovelace, etc. Nevertheless, in the Blockchain a Value is more complex than this. If we look at module _Value.hs_ (look at the references above) we can see what the definition of Value is:
```haskell
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable, NFData)
    deriving newtype (Serialise, PlutusTx.IsData)
    deriving Pretty via (PrettyShow Value)
```
Wee see that it only has one field, called `getValue`, which is a map from `CurrencySymbol` to a map of `TokenName` and an `Integer` value. This way, we have that each native token, including Ada, is completely indentified by two field: its `CurrencySymbol` and its `TokenName`. Both data types are defined at the begining of this same module like this:
```haskell
newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Builtins.ByteString }
-- ignoring deriving statements
.
.
.
-- | ByteString of a name of a token, shown as UTF-8 string when possible
newtype TokenName = TokenName { unTokenName :: Builtins.ByteString }
-- ignoring deriving statements
```
so we see they are just wrappers around the `ByteString` type. We can also wrap these in a useful data type called `AssetClass`:
```haskell
-- | An asset class, identified by currency symbol and token name.
newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
-- ignoring deriving statements
```
which is just a tuple of a `CurrencySymbol` and a `TokenName`. This `AssetClass` also makes possible to understand a `Value` as a map from `AssetClass` to an `Integer`. The integer value of a Value is the number of units of a specific `AssetClass`.

Now that we have seen the basics of Value types, let us play a bit in the repl to get an insight on some useful functions to work with Values. Again, as in the previous weeks, we can start the repl this way:
1. Activate the `nix-shell` inside the plutus repo directory: `__@__:~/plutus$ nix-shell`
2. Move to `plutus-pioneer-program/code/week05/`
3. Access the repl with `cabal repl`, and then
4. Import _Value.hs_ and _Ada.hs_: `import Plutus.V1.Ledger.Value` and `import Plutus.V1.Ledger.Ada`
5. Activate the overload string extension: `:set -XOverloadedStrings`, so we can enter `ByteString`s as literal strings.

Once we have done all this, we are ready to play!

Before nothing, it would be interesting to explore the `Value` of Ada. There is a function in the _Ada.hs_ module called `adaSymbol`, which is of type `CurrencySymbol` that returns, unsurprisingly, the symbol of the ada token. If you run it into the repl,
```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada> adaSymbol

Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada>
```
you see that its symbol is just the empty ByteString. Wow, this is interesting. We will see its importance in a minute, but keep in mind that is something exclusive of the Ada. Let us see what its `TokenName` is by using the function `adaToken`, which is of type `TokenName`, of course.
```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada> adaToken
""
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada>
```
Oh! Again the empty ByteString. Okey, so do we build an Ada `Value` from the map to map we talked before using the empty `ByteString`s or is there a better way? Well, there exists the function `lovelaceValueOf`, whose type is:
```haskell
lovelaceValueOf :: Integer -> Value
```
which allow us to do exactly this. On the repl:
```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada> lovelaceValueOf 100
Value (Map [(,Map [("",100)])])
```
