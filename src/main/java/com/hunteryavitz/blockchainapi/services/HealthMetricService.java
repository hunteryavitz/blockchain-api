package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.healthmetric.BlockchainProduction;
import com.hunteryavitz.blockchainapi.utils.structures.SlidingWindow;
import lombok.Getter;
import org.springframework.stereotype.Service;

/**
 * The HealthMetricService class.
 */
@Service
public class HealthMetricService {


    /**
     * The block count is the number of blocks for the purposes of measuring production.
     */
//    @Getter
    private static int blockCount;

    /**
     * The transactionCount is the number of transactions for the purposes of measuring production.
     */
//    @Getter
    private static int transactionCount;

    /**
     * The blockchain production.
     */
    private final BlockchainProduction blockchainProduction;

    /**
     * The constructor.
     */
    public HealthMetricService() {
        blockCount = 0;
        transactionCount = 0;
        blockchainProduction = new BlockchainProduction();
    }

    /**
     * Increments the block count.
     */
    public void incrementBlockCount() {
        blockCount++;
    }

    /**
     * Gets the block count.
     * @return The block count.
     */
    public int getBlockCount() {
        return blockCount;
    }

    /**
     * Resets the block count.
     */
    public void resetBlockCount() {
        blockCount = 0;
    }

    /**
     * Increments the transaction count.
     */
    public void incrementTransactionCount() {
        transactionCount++;
    }

    /**
     * Gets the transaction count.
     * @return The transaction count.
     */
    public int getTransactionCount() {
        return transactionCount;
    }

    /**
     * Resets the transaction count.
     */
    public void resetTransactionCount() {
        transactionCount = 0;
    }

    /**
     * Updates the blockchain production.
     */
    public void updateBlockchainProduction() {
        blockchainProduction.updateProduction(transactionCount, blockCount);
        resetTransactionCount();
        resetBlockCount();
    }

    /**
     * Gets the production.
     * @return The production.
     */
    public SlidingWindow getProduction() {
        return blockchainProduction.getSlidingWindow();
    }
}
