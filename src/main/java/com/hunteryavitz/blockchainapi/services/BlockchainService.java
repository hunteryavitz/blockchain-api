package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.utils.Utils;
import org.springframework.stereotype.Service;

import java.security.NoSuchAlgorithmException;

/**
 * The BlockchainService class is responsible for managing the blockchain.
 */
@Service
public class BlockchainService {

    /**
     * The blockchain is an array of blocks.
     */
    private static Block[] blockchain;

    /**
     * The constructor for the BlockchainService class.
     */
    public void createInitialBlockchain() {

        blockchain = new Block[100];
        Block genesisBlock;

        try {
            long timestamp = System.currentTimeMillis();
            genesisBlock = new Block(0, "0", timestamp, "Genesis Block",
                    Utils.calculateHash(0, "0", timestamp, "Genesis Block"));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        blockchain[0] = genesisBlock;

    }

    /**
     *  The addBlockToBlockchain method is responsible for adding a block to the blockchain.
     */
    public void addBlockToBlockchain() {

        int nextBlockIndex = Utils.getNextBlockIndexFromBlockchain(blockchain);

        Block previousBlock = blockchain[nextBlockIndex - 1];

        Block block;

        try {
            block = new Block(nextBlockIndex, previousBlock.getHash(),
                    System.currentTimeMillis(), "Block " + nextBlockIndex,
                    Utils.calculateHash(nextBlockIndex, previousBlock.getHash(),
                            System.currentTimeMillis(), "Block " + nextBlockIndex));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        blockchain[nextBlockIndex] = block;
    }

    /**
     * The verifyBlockchain method is responsible for verifying the blockchain.
     * @return A boolean value indicating whether the blockchain is valid.
     */
    public boolean verifyBlockchain() {
        return Utils.verifyBlockchain(blockchain);
    }

    /**
     * The getBlockchain method is responsible for returning the blockchain.
     * @return The blockchain.
     */
    public Block[] getBlockchain() {
        return blockchain;
    }

    public Block getBlockById(int id) {

        return blockchain[id];
    }
}
