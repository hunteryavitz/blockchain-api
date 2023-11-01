package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
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
    @Getter
    private static int blockCount;

    /**
     * The transactionCount is the number of transactions for the purposes of measuring production.
     */
    @Getter
    private static int transactionCount;

    /**
     * The health is an array of integers that represent the number of health metrics of each type.
     */
    @Getter
    private static Integer[] health;

    /**
     * The blockchain production.
     */
    private static BlockchainProduction blockchainProduction;

    /**
     * The constructor.
     */
    public HealthMetricService() {
        blockCount = 0;
        transactionCount = 0;
        blockchainProduction = new BlockchainProduction();
        health = new Integer[5];
        for (int i = 0; i < 5; i++) {
            health[i] = 0;
        }
    }

    /**
     * Creates the health metric service.
     */
    public void createHealthMetricService() {
        blockCount = 0;
        transactionCount = 0;
        blockchainProduction = new BlockchainProduction();
        health = new Integer[5];
        for (int i = 0; i < 5; i++) {
            health[i] = 0;
        }
    }

    /**
     * Increments the block count.
     */
    public void incrementBlockCount() {
        blockCount++;
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

    /**
     * Updates the health.
     * @param contaminationLevel The contamination level.
     * @param exception The exception.
     */
    public void updateHealth(ContaminationLevel contaminationLevel, Exception exception) {
        switch (contaminationLevel.toString()) {
            case "INFO":
                health[0] = health[0] + 1;
                System.out.println(health[0]);
                break;
            case "WARNING":
                health[1] = health[1] + 1;
                System.out.println(health[1]);
                break;
            case "ERROR":
                health[2] = health[2] + 1;
                System.out.println(health[2]);
                break;
            case "CRITICAL":
                health[3] = health[3] + 1;
                System.out.println(health[3]);
                break;
            case "UNKNOWN":
                health[4] = health[4] + 1;
                System.out.println(health[4]);
                break;
            default:
                System.out.println(exception.getMessage());
                break;
        }
    }
}
