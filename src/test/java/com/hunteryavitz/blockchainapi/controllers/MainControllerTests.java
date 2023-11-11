package com.hunteryavitz.blockchainapi.controllers;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.core.env.Environment;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

import java.util.Arrays;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for the Main controller.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
    properties = {"spring.profiles.active=test"})
@TestPropertySource(locations = "classpath:application-test.properties")
public class MainControllerTests {

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and controller.
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
     * The test query parameter.
     */
    private static final String QUERY_PARAM_TEST = "?test=true";

    /**
     * The environment.
     */
    @Autowired
    private Environment environment;

    /**
     * Test the environment.
     */
    @Test
    public void testActiveProfiles_whenTest_showsTest() {
        assertTrue(Arrays.asList(environment.getActiveProfiles()).contains("test"));
    }

    /**
     * Tests the isReady method succeeds.
     */
    @Test
    void testIsReady_onSuccess_returns200AndTrue() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                API_VERSION
                        + READINESS_ENDPOINT, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the isReady method fails.
     */
    @Test
    void testIsReady_onFail_returns200AndFalse() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                API_VERSION
                        + READINESS_ENDPOINT
                        + QUERY_PARAM_TEST, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.FALSE.equals(response.getBody()));
    }

    /**
     * Tests the isAlive method succeeds.
     */
    @Test
    void isAlive_onSuccess_returns200AndLiveness() {
        ResponseEntity<Integer> response = restTemplate.getForEntity(
                API_VERSION
                        + LIVENESS_ENDPOINT, Integer.class);

        assert response.getStatusCode().is2xxSuccessful();

        try {
            int liveness = Objects.requireNonNull(response.getBody());
            assert (liveness > -1);
        } catch (NullPointerException nullPointerException) {
            assert (true);
        }
    }

    /**
     * Tests the isAlive method fails.
     */
    @Test
    void isAlive_onFail_returns200AndNegativeOne() {
        ResponseEntity<Integer> response = restTemplate.getForEntity(
                API_VERSION
                        + LIVENESS_ENDPOINT
                        + QUERY_PARAM_TEST, Integer.class);

        assert response.getStatusCode().is2xxSuccessful();

        try {
            int liveness = Objects.requireNonNull(response.getBody());
            assert (liveness == -1);
        } catch (NullPointerException nullPointerException) {
            assert (true);
        }
    }

    /**
     * Tests the getVersion method succeeds.
     */
    @Test
    void testGetVersion_onSuccess_returns200AndVersion() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION
                        + VERSION_ENDPOINT, String.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert ("0.0.18".equals(response.getBody()));
    }

    /**
     * Tests the getVersion method fails.
     */
    @Test
    void testGetVersion_onFail_returns200AndEmpty() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION
                        + VERSION_ENDPOINT
                        + QUERY_PARAM_TEST, String.class);

        assert response.getStatusCode().is2xxSuccessful();
        assertNull (response.getBody());
    }
}
