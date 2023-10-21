package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.hunteryavitz.blockchainapi.utils.structures.SlidingWindow;
import lombok.Data;

/**
 * The BlockchainProduction class is a model for the production health metric.
 */
@Data
public class BlockchainProduction {

    /**
     * The sliding window for the production health metric.
     */
    private SlidingWindow slidingWindow;

    /**
     * The constructor for the BlockchainProduction class.
     */
    public BlockchainProduction() {
        slidingWindow = new SlidingWindow(12);
    }

    /**
     * Updates the production health metric.
     * @param transactionCount the number of transactions
     * @param blockCount the number of blocks
     */
    public void updateProduction(int transactionCount, int blockCount) {
        slidingWindow.enqueueAndShift(transactionCount, blockCount);
    }
}
