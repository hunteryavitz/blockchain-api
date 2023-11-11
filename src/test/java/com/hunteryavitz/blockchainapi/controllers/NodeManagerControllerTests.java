package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeHealthResponse;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeRegistryRequest;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeStatusResponse;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the NodeManager controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {"spring.profiles.active=test"})
@TestPropertySource(locations = "classpath:application-test.properties")
public class NodeManagerControllerTests {

    /**
     * The rest template.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and controller.
     */
    private static final String API_VERSION = "/api/v1/nodeManager";

    /**
     * The register node endpoint.
     */
    private static final String REGISTER_NODE_ENDPOINT = "/registerNode";

    /**
     * The node network roll call endpoint.
     */
    private static final String NODE_NETWORK_ROLL_CALL_ENDPOINT = "/nodeNetworkRollCall";

    /**
     * The get node network status endpoint.
     */
    private static final String GET_NODE_NETWORK_STATUS_ENDPOINT = "/getNodeNetworkStatus";

    /**
     * The get node network health endpoint.
     */
    private static final String GET_NODE_NETWORK_HEALTH_ENDPOINT = "/getNodeNetworkHealth";

    /**
     * The query param test.
     */
    private static final String QUERY_PARAM_TEST = "?test=true";

    /**
     * Test register node succeeds.
     */
    @Test
    void testRegisterNode_onSuccess_returns200AndNodeStatusActive() {
        NodeRegistryRequest nodeRegistryRequest = new NodeRegistryRequest();
        nodeRegistryRequest.setCertificate("secret-sauce");
        nodeRegistryRequest.setPort(9999);
        ResponseEntity<NodeStatus> response = restTemplate.postForEntity(API_VERSION
                + REGISTER_NODE_ENDPOINT, nodeRegistryRequest, NodeStatus.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert NodeStatus.ACTIVE.equals(response.getBody());
    }

    /**
     * Test register node fails.
     */
    @Test
    void testRegisterNode_onFail_returns200AndNodeStatusFailedRegistration() {
        NodeRegistryRequest nodeRegistryRequest = new NodeRegistryRequest();
        nodeRegistryRequest.setCertificate("secret-sauce");
        nodeRegistryRequest.setPort(9999);
        ResponseEntity<NodeStatus> response = restTemplate.postForEntity(API_VERSION
                + REGISTER_NODE_ENDPOINT
                + QUERY_PARAM_TEST, nodeRegistryRequest, NodeStatus.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert NodeStatus.FAILED_REGISTRATION.equals(response.getBody());
    }

    /**
     * Test get node network status succeeds.
     */
    @Test
    void testGetNodeNetworkStatus_onSuccess_returns200AndNodeNetworkStatus() {

        ResponseEntity<NodeStatusResponse[]> response = restTemplate.getForEntity(API_VERSION
                + GET_NODE_NETWORK_STATUS_ENDPOINT, NodeStatusResponse[].class);

        assert response.getStatusCode().is2xxSuccessful();
        assert response.getBody() != null;
    }

    /**
     * Test get node network status fails.
     */
    @Test
    void testGetNodeNetworkStatus_onFail_returns200AndEmptyArray() {
        ResponseEntity<NodeStatusResponse[]> response = restTemplate.getForEntity(API_VERSION
                + GET_NODE_NETWORK_STATUS_ENDPOINT
                + QUERY_PARAM_TEST, NodeStatusResponse[].class);

        assert response.getStatusCode().is2xxSuccessful();
        assert response.getBody() != null;
    }

    /**
     * Test node network roll call succeeds.
     */
    @Test
    void testNodeNetworkRollCall_onSuccess_returns200AndTrue() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION
                + NODE_NETWORK_ROLL_CALL_ENDPOINT, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert Boolean.TRUE.equals(response.getBody());
    }

    /**
     * Test node network roll call fails.
     */
    @Test
    void testNodeNetworkRollCall_onFail_returns200AndFalse() {
        ResponseEntity<Boolean> response = restTemplate.getForEntity(API_VERSION
                + NODE_NETWORK_ROLL_CALL_ENDPOINT
                + QUERY_PARAM_TEST, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert Boolean.FALSE.equals(response.getBody());
    }

    /**
     * Test get node network health succeeds.
     */
    @Test
    void testGetNodeNetworkHealth_onSuccess_returns200AndNodeNetworkHealth() {
        ResponseEntity<NodeHealthResponse> response = restTemplate.getForEntity(API_VERSION
                + GET_NODE_NETWORK_HEALTH_ENDPOINT
                + QUERY_PARAM_TEST, NodeHealthResponse.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert response.getBody() != null;
    }

    /**
     * Test get node network health fails.
     */
    @Test
    void testGetNodeNetworkHealth_onFail_returns200AndEmptyArray() {
        ResponseEntity<NodeHealthResponse> response = restTemplate.getForEntity(API_VERSION
                + GET_NODE_NETWORK_HEALTH_ENDPOINT
                + QUERY_PARAM_TEST, NodeHealthResponse.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert response.getBody() != null;
    }
}
