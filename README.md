# homework-interpreter

* Questions 1 and 2: look at the code in app/Main.hs

* Question 2: To implement loops, we need to include 'boolean handling' as well, I've commented out the code for now.

* Question 3, here goes:

- SEND_CHANNEL: For this instruction, we would pop two elements from the stack, the first element popped being the channel and the second one being
  the value we want sent to the channel. We'd model the channel with Haskell's MVar, and the blocking send would be implemented by calling `putMVar :: MVar a -> a -> IO ()` on the MVar.

- RECV_CHANNEL: We'd pop the channel from the stack (the channel would be represented as an MVar), then call `takeMVar :: MVar a -> IO a` on it.

- SPAWN: We can use `forkIO` to spawn functions popped from the stack as concurrent tasks

- Note: the send and receive instructions' blocking nature will be handled natively by Haskell's MVars: If we try to put into an already full MVar or take from an empty MVar, it automatically blocks.


* Question 4:
- Hashing functions enable blockchain technology by:
    * Making sure the blockchain is tamper-resistant. Once a transaction makes it into a block, it cannot be modified because doing so would invalidate the hashes of subsequent blocks since said subsequent blocks' hashes do contain a hash of the block just modified. Thus, only through a 51% attack can the blockchain be modified, which is very expensive.
    * Helping generate random hashes until one with the correct minimum of zeroes is found (For chains that are based on proof-of-work).
    * Helping with addresses generation and digital signatures (public keys, private keys, addresses, etc...)

* Question 5:
- Bitcoin's UTXO model of transaction validation:
Transactions in Bitcoin are made of inputs and outputs. They consume UTXOs and output UTXOs. Whenever a transaction is created, it generates one or many UTXOs (Unspent Transaction Output). Those UTXOs are locked and can only be 'spent' by another transaction that meets the conditions specified by said UTXOs.  Cardano expands upon this model by allowing not only owners of a particular private key to be able to consume a UTXO, but also allowing the specification of custom logic in order to 'unlock' the funds carried by a UTXO.

* Question 6:
- A block is a data structure composed of the following fields: block size, block header, transaction count, transactions.
- A block header is composed of: protocol version, previous block hash, merkle root, timestamp, difficulty target, nonce
- The merkle tree summarizes the hashes of all the transactions contained in the block

* Question 7:
- POW and POS are mechanisms that help the network to achieve consensus. They allow to verify transactions without needing a central authority.
