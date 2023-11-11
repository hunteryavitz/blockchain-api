package com.hunteryavitz.blockchainapi.entities.healthmetric;

import lombok.Data;

import java.util.List;

/**
 * NodeHealthResponse is the response for the node network health.
 */
@Data
public class NodeHealthResponse {

    /**
     * The node health response.
     */
    private List<NodeHealth> nodeHealthResponse;

    /**
     * Add node health to node health response.
     * @param nodeHealth the node health
     */
    public void addNodeHealth(NodeHealth nodeHealth) {
        nodeHealthResponse.add(nodeHealth);
    }
}
