package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import com.hunteryavitz.blockchainapi.services.TransactionService;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the Transaction controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class TransactionControllerTests {

    /**
     * The TransactionService used to make requests to the API.
     */
    @Mock
    private TransactionService transactionService;

    /**
     * The TransactionController used to make requests to the API.
     */
    @InjectMocks
    private TransactionController transactionController;

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and endpoints.
     */
    private static final String API_VERSION = "/api/v1";

    /**
     * The submit transaction endpoint.
     */
    private static final String SUBMIT_TRANSACTION_ENDPOINT = "/transaction/submitTransaction";

    /**
     * The get transaction pool endpoint.
     */
    private static final String GET_BLOCK_BY_INDEX_ENDPOINT = "/blockchain/getBlockById?id=1";

    /**
     * Tests the getBlockchain endpoint.
     */
    @Test
    void testSubmitTransaction() {
        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");

        ResponseEntity<Boolean> response = restTemplate.postForEntity(
                API_VERSION + SUBMIT_TRANSACTION_ENDPOINT, transaction, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the Transaction Service to add Block to Blockchain when full.
     */
//    @Test
//    void testAddsBlockOnFullTransactionPool() {
//
//        transactionService = new TransactionService();
//        transactionService.createInitialTransactionPool();
//        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");
//
//        int transactionPoolLength = transactionService.getTransactionPool().length;
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
