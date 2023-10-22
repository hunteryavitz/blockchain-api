package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.entities.Transaction;
import com.hunteryavitz.blockchainapi.utils.Utils;
import org.springframework.stereotype.Service;

import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

/**
 * The BlockchainService class is responsible for managing the blockchain.
 */
@Service
public class BlockchainService {

    /**
     * The health metric service.
     */
    private static HealthMetricService healthMetricService;

    /**
     * The blockchain is an array of blocks.
     */
    private static Block[] blockchain;

    /**
     * The liveness array is an array of booleans that indicate whether a block is alive.
     */
    private static Boolean[] liveness;

    /**
     * The blockCount is the number of blocks for the purposes of measuring production.
     */
    private static Integer[] contaminationSpectrum;

    /**
     * The constructor for the BlockchainService class.
     */
    public void createInitialBlockchain() {

        if (healthMetricService == null) {
            healthMetricService = new HealthMetricService();
        }

        liveness = new Boolean[100];

        blockchain = new Block[100];
        Block genesisBlock;

        contaminationSpectrum = new Integer[5];
        Arrays.fill(contaminationSpectrum, 0);

        healthMetricService.resetBlockCount();

        try {
            long timestamp = System.currentTimeMillis();
            genesisBlock = new Block(0, "0", timestamp, "Genesis Block",
                    Utils.calculateHash(0, "0", timestamp, "Genesis Block"));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        blockchain[0] = genesisBlock;
        healthMetricService.incrementBlockCount();
    }

    /**
     * The isAlive method is responsible for returning the liveness of the blockchain.
     * @return The liveness of the blockchain.
     */
    public int isAlive() {
        return Utils.calculateLiveness(liveness);
    }

    /**
     *  The addBlockToBlockchain method is responsible for adding a block to the blockchain.
     */
    public void addBlockToBlockchain() {

        int nextBlockIndex = getNextBlockIndexFromBlockchain();

        Block previousBlock = blockchain[nextBlockIndex - 1];
        Block block;

        try {
            block = new Block(nextBlockIndex, previousBlock.getHash(),
                    System.currentTimeMillis(), "Block " + nextBlockIndex,
                    Utils.calculateHash(nextBlockIndex, previousBlock.getHash(),
                            System.currentTimeMillis(), "Block " + nextBlockIndex));

            incrementBlockCount();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        blockchain[nextBlockIndex] = block;
    }

    /**
     * The addBlockToBlockchain method is responsible for adding a block to the blockchain.
     * @param transactionPool The transaction pool.
     */
    public void addBlockToBlockchain(Transaction[] transactionPool) {

        String payload = Arrays.toString(transactionPool);

        int nextBlockIndex = getNextBlockIndexFromBlockchain();

        Block previousBlock = blockchain[nextBlockIndex - 1];
        Block block;

        try {
            block = new Block(nextBlockIndex, previousBlock.getHash(),
                    System.currentTimeMillis(), payload,
                    Utils.calculateHash(nextBlockIndex, previousBlock.getHash(),
                            System.currentTimeMillis(), payload));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        blockchain[nextBlockIndex] = block;
        healthMetricService.incrementBlockCount();
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

    /**
     * The getBlockById method is responsible for returning a block from the blockchain by its id.
     * @param id The id of the block.
     * @return The block.
     */
    public Block getBlockById(int id) {
        return blockchain[id];
    }

    /**
     * The getNextBlockIndexFromBlockchain method is responsible for returning the next block index from the blockchain.
     * @return The next block index from the blockchain.
     */
    private int getNextBlockIndexFromBlockchain() {
        return Utils.getNextBlockIndexFromBlockchain(blockchain);
    }

    /**
     * The checkReadiness method is responsible for checking the readiness of the blockchain.
     */
    public void checkReadiness() {
        liveness = Utils.updateLiveness(liveness);
    }

    /**
     * The getHealthMetrics method is responsible for returning the health metrics of the blockchain.
     * @return The health metrics of the blockchain.
     */
    public Integer[] getHealthMetrics() {

        // mock health metrics
        contaminationSpectrum[0] = (int) (Math.random() * 100 + 1);
        contaminationSpectrum[1] = (int) (Math.random() * 100 + 1);
        contaminationSpectrum[2] = (int) (Math.random() * 100 + 1);
        contaminationSpectrum[3] = (int) (Math.random() * 100 + 1);
        contaminationSpectrum[4] = (int) (Math.random() * 100 + 1);

        Integer[] healthMetrics = Utils.getContaminationSpectrum(contaminationSpectrum);
        Arrays.fill(contaminationSpectrum, 0);

        return healthMetrics;
    }

    /**
     * The incrementBlockCount method is responsible for incrementing the block count.
     */
    private void incrementBlockCount() {
        contaminationSpectrum[0]++;
    }
}
