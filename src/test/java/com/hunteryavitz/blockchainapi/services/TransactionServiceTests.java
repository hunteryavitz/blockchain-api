package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
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
    @Mock
    private TransactionService transactionService;

    /**
     * Tests the createInitialTransactionPool method.
     */
    @Test
    public void testCreateInitialTransactionPool() {

        transactionService = new TransactionService();
        transactionService.createInitialTransactionPool();

        assert transactionService.getTransactionPool().length == 10;
    }

//    /**
//     * Tests the submitTransaction method.
//     */
//    @Test
//    public void testSubmitTransaction() {
//
//        transactionService = new TransactionService();
//        transactionService.createInitialTransactionPool();
//
//        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");
//        transactionService.submitTransaction(transaction);
//
//        assert transactionService.getTransactionPool()[0].getId() == 999;
//    }
}
