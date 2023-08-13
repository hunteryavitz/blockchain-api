package com.hunteryavitz.blockchainapi.controllers;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;

/**
 * Unit tests for the Main controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class MainTests {

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
    private static final String READINESS_ENDPOINT = "/readiness";

    /**
     * The readiness and liveness endpoints.
     */
    private static final String LIVENESS_ENDPOINT = "/liveness";

    /**
     * The version endpoint.
     */
    private static final String VERSION_ENDPOINT = "/version";

    /**
     * The addBlock endpoint.
     */
    private static final String VERIFY_ENDPOINT = "/verifyBlockchain";

    /**
     * Tests the isReady method.
     */
    @Test
    void testIsReady() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION + READINESS_ENDPOINT, Boolean.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the isAlive method.
     */
    @Test
    void isAlive() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION + LIVENESS_ENDPOINT, Boolean.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the getVersion method.
     */
    @Test
    void testGetVersion() {
        ResponseEntity<String> response = restTemplate.getForEntity(API_VERSION + VERSION_ENDPOINT, String.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert ("0.0.11".equals(response.getBody()));
    }

    /**
     * Tests the verifyBlockchain method.
     */
    @Test
    void testVerifyBlockchain() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION + VERIFY_ENDPOINT, Boolean.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }
}
