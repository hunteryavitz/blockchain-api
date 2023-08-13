package com.hunteryavitz.blockchainapi.controllers.block;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;

/**
 * Unit tests for the Main controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class BlockTests {

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
     * The readiness and readiness endpoints.
     */
    private static final String ADD_BLOCK_ENDPOINT = "/block/addBlockToBlockchain";

    /**
     * Tests the addBlockToBlockchain method.
     */
    @Test
    void testAddBlockToBlockchain() {
        ResponseEntity<Boolean> response = restTemplate.postForEntity(
                API_VERSION + ADD_BLOCK_ENDPOINT, null, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }
}
