# Week04

### Credits

[Alberto Calzada - albertoCCz](https://github.com/albertoCCz)

# The Contract Monad
The porpouse of the Contract monad is to define off-chain code that runs in the wallet. It has four parameters:
```haskell
-- Contract w s e a
```
where:
- a:  The overall result of the computation.
- w:  Allows you to write log messages of type `w`. The equivalent of this would be the list of strings we had on the _Writer.hs_ example. 
- s:  Describes the blockchain capabilities aka what contract specific actions this contract can perform. For example: waiting for slots, submiting transactions, finding out your own public key or specific endpoints.
- e:  Describes the type of error messages.

### Example of Contrac Monad
```haskell
myContract1 :: Contract () BlockchainActions Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Hello from the contract!"
```
As you can see, we have chosen `s` to be of type `BlockchainActions`. This data type contains the minimal set of actions for a contract: from the options given above, we will not be able to use specific endpoints. In particular, you can check what actions are avaliable in the _[Contract.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract.hs)_ module if you look for `BlockchainActions`.
