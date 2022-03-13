# homework-interpreter

{-
(1)

    You are a TA at a university, and you want to evaluate your student’s homework
    without executing their (untrusted) code. You decide to write a small
    web-service that takes bytecode as input, and interprets the results.

    The bytecode language you need to support includes basic arithmetic and
    variables. The bytecode language is stack, rather than register based.
    ByteCode (right) is given for the following pseudo code (left):


    function f() {

        x = 1                   LOAD_VAL 1
                                WRITE_VAR ‘x’

        y = 2                   LOAD_VAL 2
                                WRITE_VAR ‘y’

        return (x + 1) * y      READ_VAR ‘x’
                                LOAD_VAL 1
                                ADD

                                READ_VAR ‘y’
                                MULTIPLY

                                RETURN_VALUE
    }


Add a data type `ByteCode` that can represent bytecode like in the example
above, along with an interpreter for said bytecode. Make sure your code is
total and try to use MTL or an effect library. Moreover, make sure your
bytecode is flat, i.e. not nested.
-}

-- data ByteCode = ...

-- runByteCode :: ...




{-
(2)
    Extend your interpreter with loops. In particular:

    (a) Extend your `ByteCode` datatype with suitable instrucitons to support loops
    (b) Modify your interpreter to support said instructions
    (c) Try it out and see if it works :)
-}



{-
(3)  Suppose we added the following bytecode instructions to our language:

    SEND_CHANNEL:

        Pops the channel and a value from the stack and send the
        value on the channel using a blocking send

    RECV_CHANNEL:

        Pops the channel from the stack, receives a value from the channel
        (this may block), and push the resulting value back onto the stack

    SPAWN:

        Pop two functions from the stack and spawn them as concurrent tasks


Describe in a few sentences how each bytecode instruction could be interpreted,
and how your interpreter or language runtime could deal with the blocking nature
of the send and the receive instructions.
-}



{-
Blockchain Questions (optional, only if you have previous blockchain experience)

(4) explain some of the ways hashing functions enable blockchain technology


(5) briefly explain Bitcoin's UTXO model of transaction validation (separate from POW)


(6) what is the structure of a Block in bitcoin and how does it relate to the 'blockchain' (merkle tree vs merkle list of merkle trees)


(7) what problem/s are POW/POS trying to solve? discuss/compare (byzantine fault tolerance, reaching a single consensus on a p2p network)

-}

* **ANSWERS**

** **Questions 1 and 2**: look at the code in app/Main.hs

** **Question 2**: To implement loops, we need to include 'boolean handling' as well, I've commented out the code for now.

** **Question 3**, here goes:

- SEND_CHANNEL: For this instruction, we would pop two elements from the stack, the first element popped being the channel and the second one being
  the value we want sent to the channel. We'd model the channel with Haskell's MVar, and the blocking send would be implemented by calling `putMVar :: MVar a -> a -> IO ()` on the MVar.

- RECV_CHANNEL: We'd pop the channel from the stack (the channel would be represented as an MVar), then call `takeMVar :: MVar a -> IO a` on it.

- SPAWN: We can use `forkIO` to spawn functions popped from the stack as concurrent tasks

- Note: the send and receive instructions' blocking nature will be handled natively by Haskell's MVars: If we try to put into an already full MVar or take from an empty MVar, it automatically blocks.


** **Question 4**:
- Hashing functions enable blockchain technology by:
    * Making sure the blockchain is tamper-resistant. Once a transaction makes it into a block, it cannot be modified because doing so would invalidate the hashes of subsequent blocks since said subsequent blocks' hashes do contain a hash of the block just modified. Thus, only through a 51% attack can the blockchain be modified, which is very expensive.
    * Helping generate random hashes until one with the correct minimum of zeroes is found (For chains that are based on proof-of-work).
    * Helping with addresses generation and digital signatures (public keys, private keys, addresses, etc...)

** **Question 5**:
- Bitcoin's UTXO model of transaction validation:
Transactions in Bitcoin are made of inputs and outputs. They consume UTXOs and output UTXOs. Whenever a transaction is created, it generates one or many UTXOs (Unspent Transaction Output). Those UTXOs are locked and can only be 'spent' by another transaction that meets the conditions specified by said UTXOs.  Cardano expands upon this model by allowing not only owners of a particular private key to be able to consume a UTXO, but also allowing the specification of custom logic in order to 'unlock' the funds carried by a UTXO.

** **Question 6**:
- A block is a data structure composed of the following fields: block size, block header, transaction count, transactions.
- A block header is composed of: protocol version, previous block hash, merkle root, timestamp, difficulty target, nonce
- The merkle tree summarizes the hashes of all the transactions contained in the block

** **Question 7**:
- POW and POS are mechanisms that help the network to achieve consensus. They allow to verify transactions without needing a central authority.
