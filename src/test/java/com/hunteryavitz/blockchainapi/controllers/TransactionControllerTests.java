package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.junit.jupiter.api.Test;
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
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and controller.
     */
    private static final String API_VERSION = "/api/v1/transaction";

    /**
     * The submit transaction endpoint.
     */
    private static final String SUBMIT_TRANSACTION_ENDPOINT = "/submitTransaction";

    /**
     * The get transaction pool endpoint.
     */
    private static final String GET_TRANSACTION_POOL_ENDPOINT = "/getTransactionPool";

    /**
     * The query param for testing.
     */
    private static final String QUERY_PARAM_TEST = "?test=true";

    /**
     * Tests the getBlockchain endpoint succeeds.
     */
    @Test
    void testSubmitTransaction_onSuccess_returns200AndTrue() {

        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");

        ResponseEntity<Boolean> response = restTemplate.postForEntity(API_VERSION
                        + SUBMIT_TRANSACTION_ENDPOINT, transaction, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the getBlockchain endpoint fails.
     */
    @Test
    void testSubmitTransaction_onFail_returns200AndFalse() {

        Transaction transaction = new Transaction(
                999, "right_now", "your mom", "CREATED");

        ResponseEntity<Boolean> response = restTemplate.postForEntity(API_VERSION
                + SUBMIT_TRANSACTION_ENDPOINT
                + QUERY_PARAM_TEST, transaction, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.FALSE.equals(response.getBody()));
    }

    /**
     * Tests the get transaction pool endpoint succeeds.
     */
    @Test
    void testGetTransactionPool_onSuccess_returns200AndTransactionPool() {

        ResponseEntity<Transaction[]> response = restTemplate.getForEntity(API_VERSION
                + GET_TRANSACTION_POOL_ENDPOINT, Transaction[].class);

        assert response.getStatusCode().is2xxSuccessful();
        assert response.getBody() != null;
    }

    /**
     * Tests the get transaction pool endpoint fails.
     */
    @Test
    void testGetTransactionPool_onFail_returns200AndEmptyTransactionPool() {

        ResponseEntity<Transaction[]> response = restTemplate.getForEntity(API_VERSION
                + GET_TRANSACTION_POOL_ENDPOINT
                + QUERY_PARAM_TEST, Transaction[].class);

        assert response.getStatusCode().is2xxSuccessful();
        assert response.getBody() != null;
    }
}
