package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.core.env.Environment;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for the Main controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
    properties = {"spring.profiles.active=test"})
@TestPropertySource(locations = "classpath:application-test.properties")
public class MainControllerTests {

    /**
     * The blockchain service.
     */
    private static BlockchainService blockchainService;

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
     * The health endpoint.
     */
    private static final String HEALTH_ENDPOINT = "/health";

    /**
     * The production endpoint.
     */
    private static final String PRODUCTION_ENDPOINT = "/production";

    /**
     * The version endpoint.
     */
    private static final String VERSION_ENDPOINT = "/version";

    /**
     * The addBlock endpoint.
     */
    private static final String VERIFY_ENDPOINT = "/verifyBlockchain";

    @Autowired
    private Environment environment;

    @Test
    public void testActiveProfiles() {
        assertTrue(Arrays.asList(environment.getActiveProfiles()).contains("test"));
    }


    /**
     * Sets up the blockchain service.
     */
    @BeforeAll
    static void beforeAll() {
        if (blockchainService == null) {
            blockchainService = new BlockchainService();
            blockchainService.createInitialBlockchain();
        }
    }

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
        ResponseEntity<Integer> response = restTemplate.getForEntity(API_VERSION + LIVENESS_ENDPOINT, Integer.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (response.getBody() != null);
        //        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the isHealthy method.
     */
    @Test
    void isHealthy() {
        ResponseEntity<Integer[]> response =
                restTemplate.getForEntity(API_VERSION + HEALTH_ENDPOINT, Integer[].class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (response.getBody() != null);
        // TODO: add more sophisticated tests here
    }

    /**
     * Tests the isProduction method.
     */
    @Test
    void production() {

        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION + PRODUCTION_ENDPOINT, Boolean.class);
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
        assert ("0.0.14".equals(response.getBody()));
    }

    /**
     * Tests the verifyBlockchain method.
     */
    @Test
    void testVerifyBlockchain() {
        blockchainService = new BlockchainService();
        blockchainService.createInitialBlockchain();
        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION + VERIFY_ENDPOINT, Boolean.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }
}
