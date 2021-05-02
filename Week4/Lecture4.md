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

### Simple example of Contract Monad
In the next example we will just send a log message from the contract to the console, so we will be ignoring the `a` and `w` types.
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- ^ Add extensions on the top of the module file

myContract1 :: Contract () BlockchainActions Text ()
myContract1 = Contract.logInfo @String "Hello from the contract!"
```
As you can see, we have chosen `s` (which I believe stands for "schema") to be of type `BlockchainActions`. This data type contains the minimal set of actions for a contract: from the options given above, we will not be able to use specific endpoints. In particular, you can check what actions are avaliable in the [Contract.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract.hs) module if you look for `BlockchainActions`.

### The Schema parameter: s
We can define a custom set of contract actions by adding this actions to the `BlockchainActions` type. For example, let us say we want to add and endpoint called 'foo'. We just need to give a pseudonym to the set of action data types like this:
```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- ^ Add extensions on the top of the module file

type MySchema = BlockchainActions .\/ Endpoint "foo" Int
```
In the last line we define the type of the set of actions and we call it `MySchema`. Then we use the operator `.\/`, which acts on types, not on values, to "add" the endpoints that we want, in this case the `foo` endpoint. The first argument to `Endpoint` is a type level string which represents the name of the endpoint, and the second argument is the parameter type (which type of value this endpoint takes).

Once we have define the endpoint we can take the action defined by it using the trace emulator. First, we define our contract:
```haskell
myContract :: Contract () MySchema Text ()
myContract = do
    n <- endpoint @"foo"
    Contract.logInfo n
```
This contract just waits for some wallet to call the `"foo"` endpoint with some `Int` value and then logs it to the console.

Then, we define the trace of the simmulation:
```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) myContract
    callEndpoint @"foo" h 42
```
And finally we define the test funtion that runs this trace simmulation:
```haskell
test :: IO ()
test = runEmulatorTraceIO myTrace
```

### The Writer parameter: w
This type parameter can not be of any type but it must be an instance of the type class `Monoid`. An example of data type which is an instance of this class is `List`. This parameter of the Contract monad is essential because it allows us to bring information back from the contract to the trace and also to the _PAB_, the Plutus Application Backend. We will be able to pass info back from the contract running in the wallet to the outside world. Let us see an example:
```haskell
myContract :: Contract [Int] BlockchainActions Text ()
myContract = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10
```
In the execution of this constract we first wait for 10 Slots, then we pass info back (which has to be of type `[Int]`, as we chose on the contract type declaration) using the `tell` statement, the we again wait for 10 Slots, and so on.

Now we define the trace as follows:
```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) myContract
    
    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs
    
    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys
    
    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
```
With this trace, we are just observating the state communicated by the contract after a number of Slots. In particular, we wait 5 Slots and observe the state using the `observableState` function, to which we pass the handler `h` of the contract associated with the wallet. As the first communication made by the contract happens after Slot 10, we will get an empty* list on the console. 

*_Quick note_: if you are asking how can it returns an emtpy list, just remeber that we impossed that the Writer parameter type had to be an instance of the type class `Monoid`. This type class implements three functions, the first one being `mempty :: a` which just gives you the empty object of your data type instance. In this case, our data type instance is a `List`, so the empty object is `[]`. (Sorry for the terminology, as I am still far from being fluent in Haskell).
