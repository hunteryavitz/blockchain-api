package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.springframework.stereotype.Service;

import java.util.Arrays;

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
     * The constructor for the TransactionService class.
     */
    public void createInitialTransactionPool() {
        transactionPool = new Transaction[10];
    }

    /**
     * The submitTransaction method is responsible for submitting a transaction to the transaction pool.
     * @param transaction The transaction to be submitted to the transaction pool.
     */
    public void submitTransaction(Transaction transaction) {
        for (int i = 0; i < transactionPool.length; i++) {
            if (transactionPool[i] == null) {
                transactionPool[i] = transaction;
                if (i == transactionPool.length) {
                    blockchainService.addBlockToBlockchain(transactionPool);
                    createInitialTransactionPool();
                }
                break;
            }
        }
        System.out.println("\n" + transaction + "submitted to transaction pool.");
        System.out.println("transaction pool: " + Arrays.toString(transactionPool));
    }

    /**
     * The getNextBlockIndexFromBlockchain method is responsible for getting the next block index from the blockchain.
     * @return The next block index from the blockchain.
     */
    public Transaction[] getTransactionPool() {
        return transactionPool;
    }
}
