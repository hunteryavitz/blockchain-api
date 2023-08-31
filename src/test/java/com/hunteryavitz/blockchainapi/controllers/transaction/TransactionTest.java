package com.hunteryavitz.blockchainapi.controllers.transaction;

import com.hunteryavitz.blockchainapi.entities.Transaction;
import com.hunteryavitz.blockchainapi.services.TransactionService;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;

/**
 * Unit tests for the Transaction controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class TransactionTest {

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

    @Test
    void testSubmitTransaction() {
        Transaction transaction = new Transaction(999, "right_now", "your mom", "CREATED");

        ResponseEntity<Boolean> response = restTemplate.postForEntity(
                API_VERSION + SUBMIT_TRANSACTION_ENDPOINT, transaction, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }
}
