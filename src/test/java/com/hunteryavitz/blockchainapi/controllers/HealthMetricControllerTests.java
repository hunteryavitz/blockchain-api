package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeNetworkStatus;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the HealthMetric controller for the API.
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
    private static final String API_VERSION = "/api/v1";

    /**
     * The health metric endpoint.
     */
    private static final String UPDATE_PRODUCTION_ENDPOINT = "/healthMetric/updateProduction";

    /**
     * The health metric endpoint.
     */
    private static final String GET_PRODUCTION_HEALTH_ENDPOINT = "/healthMetric/getProductionHealth";

    /**
     * The health metric endpoint.
     */
    private static final String GET_NODE_NETWORK_STATUS_ENDPOINT = "/healthMetric/getNodeNetworkStatus";

    /**
     * Tests the updateProduction endpoint.
     */
    @Test
    void testUpdateProduction() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                API_VERSION + UPDATE_PRODUCTION_ENDPOINT, Boolean.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the getProductionHealth endpoint.
     */
    @Test
    void testGetProductionHealth() {
        ResponseEntity<String> response = restTemplate.getForEntity(
                API_VERSION + GET_PRODUCTION_HEALTH_ENDPOINT, String.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (response.getBody() != null);
    }

    /**
     * Tests the getNodeNetworkStatus endpoint.
     */
    @Test
    void testGetNodeNetworkStatus() {
        ResponseEntity<NodeNetworkStatus> response = restTemplate.getForEntity(
                API_VERSION + GET_NODE_NETWORK_STATUS_ENDPOINT, NodeNetworkStatus.class);
        assert response.getStatusCode().is2xxSuccessful();
        assert (response.getBody() != null);

        NodeNetworkStatus nodeNetworkStatus = response.getBody();
        assert (nodeNetworkStatus.getNodeNetwork() != null);
    }
}
