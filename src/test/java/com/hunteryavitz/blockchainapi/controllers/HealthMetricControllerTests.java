package com.hunteryavitz.blockchainapi.controllers;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Unit tests for the HealthMetric controller.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
    properties = {"spring.profiles.active=test"})
@TestPropertySource(locations = "classpath:application-test.properties")
public class HealthMetricControllerTests {

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and endpoints.
     */
    private static final String API_VERSION = "/api/v1/healthMetric";

    /**
     * The health metric endpoint.
     */
    private static final String UPDATE_PRODUCTION_ENDPOINT = "/updateProductionHealth";

    /**
     * The health metric endpoint.
     */
    private static final String GET_PRODUCTION_HEALTH_ENDPOINT = "/getProductionHealth";

    /**
     * The health metric endpoint.
     */
    private static final String GET_EXCEPTION_HEALTH_ENDPOINT = "/getExceptionHealth";

    /**
     * The query parameter for testing.
     */
    private static final String QUERY_PARAM_TEST = "?test=true";

    /**
     * Tests the updateProductionHealth endpoint succeeds.
     */
    @Test
    void testUpdateProductionHealth_onSuccess_returns200AndTrue() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                API_VERSION
                        + UPDATE_PRODUCTION_ENDPOINT, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the updateProductionHealth endpoint fails.
     */
    @Test
    void testUpdateProductionHealth_onFail_returns200AndFalse() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                API_VERSION
                        + UPDATE_PRODUCTION_ENDPOINT
                        + QUERY_PARAM_TEST, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.FALSE.equals(response.getBody()));
    }

    /**
     * Tests the getProductionHealth endpoint succeeds.
     */
    @Test
    void testGetProductionHealth_onSuccess_returns200AndProductionHealth() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION
                        + GET_PRODUCTION_HEALTH_ENDPOINT, String.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (response.getBody() != null);
    }

    /**
     * Tests the getProductionHealth endpoint fails.
     */
    @Test
    void testGetProductionHealth_onFail_returns200AndNull() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION
                        + GET_PRODUCTION_HEALTH_ENDPOINT
                        + QUERY_PARAM_TEST, String.class);

        assert response.getStatusCode().is2xxSuccessful();
        assertNull (response.getBody());
    }

    /**
     * Tests the getExceptionHealth endpoint succeeds.
     */
    @Test
    void testGetExceptionHealth_onSuccess_returns200AndExceptionHealth() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION
                        + GET_EXCEPTION_HEALTH_ENDPOINT, String.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (response.getBody() != null);
    }

    /**
     * Tests the getExceptionHealth endpoint fails.
     */
    @Test
    void testGetExceptionHealth_onFail_returns200AndNull() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION
                        + GET_EXCEPTION_HEALTH_ENDPOINT
                        + QUERY_PARAM_TEST, String.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Objects.requireNonNull(response.getBody()).equals("[]"));
    }
}
