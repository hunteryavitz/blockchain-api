package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.services.NodeService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the Node controller.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {"spring.profiles.active=test"})
@TestPropertySource(locations = "classpath:application-test.properties")
public class NodeControllerTests {

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and controller.
     */
    private static final String API_VERSION = "/api/v1/node";

    /**
     * The getNodeStatus endpoint.
     */
    private static final String GET_NODE_STATUS_ENDPOINT = "/getNodeStatus";

    /**
     * The test query parameter.
     */
    private static final String QUERY_PARAM_TEST = "?test=true";

    /**
     * The test node status for success.
     */
    @Test
    public void testGetNodeStatus_onSuccess_returns200AndActive() {
        NodeService.self = NodeStatus.ACTIVE;
        ResponseEntity<String> response = restTemplate.getForEntity(API_VERSION
                + GET_NODE_STATUS_ENDPOINT, String.class);
        assert NodeStatus.ACTIVE.toString().equals(response.getBody());
    }

    /**
     * The test node status for failure.
     */
    @Test
    public void testGetNodeStatus_onFail_returns200AndUnresponsive() {
        NodeService.self = NodeStatus.UNRESPONSIVE;
        ResponseEntity<String> response = restTemplate.getForEntity(API_VERSION
                + GET_NODE_STATUS_ENDPOINT + QUERY_PARAM_TEST, String.class);
        assert NodeStatus.UNRESPONSIVE.toString().equals(response.getBody());
    }
}
