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

To use this contract you first need to define the trace, which substitutes what we previouly did on the Plutus Playground by providing with a list of actions the wallet(s) associated to the contract is going to perfom. The trace function we are going to use is the next one:
```haskell
myTrace :: EmulatorTrace ()
myTrace = void $ activateContractWallet (Wallet 1) myContract
```
This function just activates a wallet or set of wallets (Wallet 1 in this case) by associating it with a contract (myContract in this case) and normally the result, is save in a handler that we can use later. Normally, the code would look like this:
```haskell
myTrace :: EmulatorTrace ()
myTrace = h <- activateContractWallet (Wallet 1) myContract
```
where `h` is the handler. Now, as we are just interested in showing the log message and we will not use the handler, we use the `void` keyword so the compiler does not complaint. Finally, after defining the trace, we can define one or more test(s) to study whether or not the contract works as expected. This time we only define one of these tests because, again, we are just interested in the log message. The test is:
```haskell
myTest :: IO ()
myTest = runEmulatorTraceIO myTrace
```
Be aware of the `IO` at the end of the function `runEmulatorTraceIO`, as it also exists the funtion called `runEmulatorTrace`. The difference between them is that the first one, the one we are using in our example, shows a compact and nicely formatted message on the console when executing (also less information, though) while the second one shows pages and pages of data that needs to be processed to make it readable. 

With all this, we are ready to try our first contract on the repl. To do this, I have found that the simplest way is to:
1. Activate the `nix-shell` inside the plutus repo directory: `__@__:~/plutus$ nix-shell`
2. Move to `plutus-pioneer-program/code/week04/` wherever you placed it.
3. Access the repl: `cabal repl`
4. Load the _Contract.hs_ module inside source: `:l src/Week04/Contract.hs`
5. And executing the test: `myTest`

You will be shown something like this:
```
Prelude Week04.Contract> test1
Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
Slot 00000: SlotAdd Slot 1
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance started
Slot 00001: *** CONTRACT LOG: "Hello from the contract!"
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance stopped (no errors)
Slot 00001: SlotAdd Slot 2
Final balances
Wallet 1:
    {, ""}: 100000000
Wallet 2:
    {, ""}: 100000000
Wallet 3:
    {, ""}: 100000000
Wallet 4:
    {, ""}: 100000000
Wallet 5:
    {, ""}: 100000000
Wallet 6:
    {, ""}: 100000000
Wallet 7:
    {, ""}: 100000000
Wallet 8:
    {, ""}: 100000000
Wallet 9:
    {, ""}: 100000000
Wallet 10:
    {, ""}: 100000000
```
You can observe, at the top of the console output, the contract log message we wrote.

## Throwing vs Caughting errors
When executing a contract, as with any other piece of code, an error can happens. The behaviour of errors inside the contract monad is the expected one: the execution stops and an error message is shown in the console. To explore this a bit and to see the difference with the log message we used in our first example, let us add a line of code to it. The contract code is as follows:
```haskell
myContract1 :: Contract () BlockchainActions Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Hello from the contract!"
```

## The Schema parameter: s
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

## The Writer parameter: w
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
With this trace, we are just observating the state communicated by the contract after a number of Slots. In particular, we wait 5 Slots and observe the state using the `observableState` function, to which we pass the handler `h` of the contract associated with the wallet. Because the first communication made by the contract happens after Slot 10, we will get an empty* list on the console. Then we wait for other 10 Slots and ask againg for the state. Now the contract has already communicated something, as we have passed Slot 15 and the communications happend on Slot 10. In particular, we will observe the list `[1]` on the console. I let you guess what happens when we take a look at the contract state for the third time.

*_Quick note_: if you are asking how can it returns an emtpy list, just remeber that we impossed that the Writer parameter type had to be an instance of the type class `Monoid`. This type class implements three functions, the first one being `mempty :: a` which just gives you the empty object of your data type instance. In this case, our data type instance is a `List`, so the empty object is `[]`. (Sorry for the terminology, as I am still far from being fluent in Haskell).