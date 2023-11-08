package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
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

    TestRestTemplate restTemplate = new TestRestTemplate();

    /**
     * Tests the createInitialTransactionPool method.
     */
    @Test
    public void testCreateInitialTransactionPool() {

        transactionService = new TransactionService();
        transactionService.createInitialTransactionPool();

        assert transactionService.getTransactionPool().length == 10;
    }

    /**
     * Tests the submitTransaction method.
     */
    @Test
    public void testSubmitTransaction() {

        transactionService = new TransactionService();
        transactionService.createInitialTransactionPool();

        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");
        transactionService.submitTransaction(transaction);

        assert transactionService.getTransactionPool()[0].getId() == 999;
    }

//    /**
//     * Tests the Transaction Service to add Block to Blockchain when full.
//     */
//    @Test
//    void testAddsBlockOnFullTransactionPool() {
//
//        transactionService = new TransactionService();
//        transactionService.createInitialTransactionPool();
//        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");
//
//        int transactionPoolLength = TransactionService.getTransactionPool().length;
//
//        for (int i = 0; i < transactionPoolLength; i++) {
//            transactionService.submitTransaction(transaction);
//        }
//
//        ResponseEntity<Block> response = restTemplate.getForEntity(
//                API_VERSION + GET_BLOCK_BY_INDEX_ENDPOINT, Block.class);
//
//        assert response.getStatusCode().is2xxSuccessful();
//
//        Block block = response.getBody();
//        System.out.println(block);
//        assert block != null;
//    }
}
