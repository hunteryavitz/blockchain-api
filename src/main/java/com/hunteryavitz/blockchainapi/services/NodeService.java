package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.constants.AppConstants;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeRegistryRequest;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
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
    public NodeStatus self = NodeStatus.INACTIVE;

    /**
     * The number of transactions received by the current node.
     */
    @Getter
    public int traffic = 0;

    /**
     * The app constants.
     */
    private final AppConstants appConstants;


    /**
     * Constructor for the node service.
     * @param appConstants The app constants.
     */
    @Autowired
    public NodeService(AppConstants appConstants) {
        this.appConstants = appConstants;
    }

    /**
     * Initializes the node service.
     */
    public void initializeNodeService() {
        registerSelf();
    }

    /**
     * Registers the current node with the registry.
     */
    private void registerSelf() {
        NodeRegistryRequest nodeRegistryRequest = new NodeRegistryRequest();
        nodeRegistryRequest.setPort(appConstants.getPort());
        nodeRegistryRequest.setCertificate(appConstants.getCertificate());

        RestTemplate restTemplate = new RestTemplate();

        try {
            ResponseEntity<NodeStatus> response = restTemplate.postForEntity(appConstants.getRegistryAddress(), nodeRegistryRequest, NodeStatus.class);
            this.self = response.getBody();
        } catch (Exception exception) {
            this.self = NodeStatus.INACTIVE;
        }
    }

    /**
     * Increments the traffic count.
     */
    public void incrementTraffic() {
        this.traffic++;
    }
}
