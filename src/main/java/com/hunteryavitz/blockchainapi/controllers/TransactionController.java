package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.services.TransactionService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * The TransactionController class is responsible for managing transactions.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/transaction")
public class TransactionController {

    /**
     * The TransactionService class is responsible for managing transactions.
     */
    private final TransactionService transactionService;

    /**
     * The constructor for the TransactionController class.
     * @param transactionService The TransactionService class is responsible for managing transactions.
     */
    public TransactionController(TransactionService transactionService) {
        this.transactionService = transactionService;
        if (transactionService.getTransactionPool() == null) {
            transactionService.createInitialTransactionPool();
        }
    }

    /**
     * The getTransactionPool method is responsible for returning the transaction pool.
     * @param transaction The transaction to be added to the transaction pool.
     * @return The transaction pool.
     */
    @PostMapping(value = "/submitTransaction", consumes = "application/json", produces = "application/json")
    public ResponseEntity<Boolean> submitTransaction(@RequestBody Transaction transaction) {
        transactionService.submitTransaction(transaction);
        return ResponseEntity.ok(true);
    }

    /**
     * The getTransactionPool method is responsible for returning the transaction pool.
     * @return The transaction pool.
     */
    @GetMapping(value = "/getTransactionPool", produces = "application/json")
    public ResponseEntity<Transaction[]> getTransactionPool() {
        return ResponseEntity.ok(transactionService.getTransactionPool());
    }
}
