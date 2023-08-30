package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.springframework.stereotype.Service;

import java.util.Arrays;

@Service
public class TransactionService {

    private static Transaction[] transactionPool;

    public void createInitialTransactionPool() {
        transactionPool = new Transaction[100];
    }

    public void submitTransaction(Transaction transaction) {
        for (int i = 0; i < transactionPool.length; i++) {
            if (transactionPool[i] == null) {
                transactionPool[i] = transaction;
                break;
            }
        }
        System.out.println("\n" + transaction + "submitted to transaction pool.");
        System.out.println("transaction pool: " + Arrays.toString(transactionPool));
    }

    public Transaction[] getTransactionPool() {
        return transactionPool;
    }
}
