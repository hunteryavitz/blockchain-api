package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
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
     * The health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * The constructor for the TransactionController class.
     * @param transactionService The TransactionService class is responsible for managing transactions.
     * @param healthMetricService The health metric service.
     */
    public TransactionController(TransactionService transactionService, HealthMetricService healthMetricService) {
        this.transactionService = transactionService;
        this.healthMetricService = healthMetricService;

        try {
            if (TransactionService.getTransactionPool() == null) {
                transactionService.createInitialTransactionPool();
            }
            if (healthMetricService.getProduction() == null) {
                healthMetricService.createHealthMetricService();
            }
        } catch (Exception exception) {
            assert healthMetricService != null;
            healthMetricService.updateHealth(ContaminationLevel.CRITICAL, exception);
        }
    }

    /**
     * The getTransactionPool method is responsible for returning the transaction pool.
     * @param transaction The transaction to be added to the transaction pool.
     * @return The transaction pool.
     */
    @PostMapping(value = "/submitTransaction", consumes = "application/json", produces = "application/json")
    public ResponseEntity<Boolean> submitTransaction(@RequestBody Transaction transaction) {
        try {
            transactionService.submitTransaction(transaction);
            return ResponseEntity.ok(true);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(false);
    }

    /**
     * The getTransactionPool method is responsible for returning the transaction pool.
     * @return The transaction pool.
     */
    @GetMapping(value = "/getTransactionPool", produces = "application/json")
    public ResponseEntity<Transaction[]> getTransactionPool() {
        try {
            return ResponseEntity.ok(TransactionService.getTransactionPool());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(new Transaction[]{});
    }
}
