package com.hunteryavitz.blockchainapi.controllers.transaction;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import com.hunteryavitz.blockchainapi.services.TransactionService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin
@RequestMapping("/api/v1/transaction")
public class TransactionController {

    private final TransactionService transactionService;

    public TransactionController(TransactionService transactionService) {
        this.transactionService = transactionService;
        if (transactionService.getTransactionPool() == null) {
            transactionService.createInitialTransactionPool();
        }
    }

    @PostMapping(value = "/submitTransaction", consumes = "application/json", produces = "application/json")
    public ResponseEntity<Boolean> submitTransaction(@RequestBody Transaction transaction) {
        transactionService.submitTransaction(transaction);
        return ResponseEntity.ok(true);
    }


}
