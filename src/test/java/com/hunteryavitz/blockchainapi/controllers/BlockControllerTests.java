package com.hunteryavitz.blockchainapi.controllers;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the Main controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class BlockControllerTests {

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and controller.
     */
    private static final String API_VERSION = "/api/v1/block";

    /**
     * The readiness and readiness endpoints.
     */
    private static final String ADD_BLOCK_ENDPOINT = "/addBlockToBlockchain";

    /**
     * The query parameter for testing.
     */
    private static final String QUERY_PARAM_TEST = "?test=true";

    /**
     * Tests the addBlockToBlockchain method succeeds.
     */
    @Test
    void testAddBlockToBlockchain_onSuccess_returns200AndTrue() {
        ResponseEntity<Boolean> response = restTemplate.postForEntity(
                API_VERSION
                        + ADD_BLOCK_ENDPOINT, null, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

   /**
     * Tests the addBlockToBlockchain method fails.
     */
    @Test
    void testAddBlockToBlockchain_onFail_returns200AndFalse() {
        ResponseEntity<Boolean> response = restTemplate.postForEntity(
                API_VERSION
                        + ADD_BLOCK_ENDPOINT
                        + QUERY_PARAM_TEST, null, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.FALSE.equals(response.getBody()));
    }
}
