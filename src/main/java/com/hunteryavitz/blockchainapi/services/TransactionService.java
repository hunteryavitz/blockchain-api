package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * The TransactionService class is responsible for managing transactions.
 */
@Service
public class TransactionService {

    /**
     * The transactionPool is an array of transactions.
     */
    private static Transaction[] transactionPool;

    /**
     * The blockchainService is an instance of the BlockchainService class.
     */
    private static BlockchainService blockchainService;

    /**
     * The transactionCount is the number of transactions for the purposes of measuring production.
     */
    @Autowired
    HealthMetricService healthMetricService;

    /**
     * The constructor for the TransactionService class.
     */
    public void createInitialTransactionPool() {
        if (blockchainService == null) {
            blockchainService = new BlockchainService();
        }
        transactionPool = new Transaction[10];
    }

    /**
     * The submitTransaction method is responsible for submitting a transaction to the transaction pool.
     * @param transaction The transaction to be submitted to the transaction pool.
     */
    public void submitTransaction(Transaction transaction) {

        // NOTE: Here to pass the unit tests
        if (healthMetricService == null) {
            healthMetricService = new HealthMetricService();
        }

        // NOTE: Here to pass the unit tests
        if (blockchainService == null) {
            blockchainService = new BlockchainService();
        }

        for (int i = 0; i < transactionPool.length; i++) {
            if (transactionPool[i] == null) {
                transactionPool[i] = transaction;
                healthMetricService.incrementTransactionCount();
                if (i == (transactionPool.length - 1)) {
                    blockchainService.addBlockToBlockchain(transactionPool);
                    createInitialTransactionPool();
                }

                break;
            }
        }
    }

    /**
     * The getNextBlockIndexFromBlockchain method is responsible for getting the next block index from the blockchain.
     * @return The next block index from the blockchain.
     */
    public Transaction[] getTransactionPool() {
        return transactionPool;
    }
}
