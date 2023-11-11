package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the Transaction service for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class TransactionServiceTests {

    /**
     * The TransactionService used to make requests to the API.
     */
    @Autowired
    private TransactionService transactionService;

    /**
     * The NodeService used to make requests to the API.
     */
    @Autowired
    private NodeService nodeService;

    /**
     * Tests the createInitialTransactionPool method.
     */
    @Test
    public void testCreateInitialTransactionPool() {
        transactionService.createInitialTransactionPool();

        assert TransactionService.getTransactionPool().length == 10;
    }

    /**
     * Tests the submitTransaction method.
     */
    @Test
    public void testSubmitTransaction() {
        transactionService.createInitialTransactionPool();
        nodeService.initializeNodeService();

        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");
        transactionService.submitTransaction(transaction);

        assert TransactionService.getTransactionPool()[0].getId() == 999;
    }
}
