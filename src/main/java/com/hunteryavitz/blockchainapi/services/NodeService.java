package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.constants.AppConstants;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeRegistryRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

/**
 * Service class for node operations.
 */
@Service
public class NodeService {

    /**
     * The status of the current node.
     */
    public static NodeStatus self = NodeStatus.INACTIVE;

    /**
     * Initializes the node service.
     */
    public static void initializeNodeService() {
        registerSelf();
    }

    /**
     * Registers the current node with the registry.
     */
    private static void registerSelf() {
        NodeRegistryRequest nodeRegistryRequest = new NodeRegistryRequest();
        nodeRegistryRequest.setPort(AppConstants.getPort());
        nodeRegistryRequest.setCertificate(AppConstants.getCertificate());

        RestTemplate restTemplate = new RestTemplate();

        try {
            self = restTemplate.postForObject(AppConstants.getRegistryAddress(), nodeRegistryRequest, NodeStatus.class);
        } catch (Exception exception) {
            self = NodeStatus.INACTIVE;
        }
    }
}
