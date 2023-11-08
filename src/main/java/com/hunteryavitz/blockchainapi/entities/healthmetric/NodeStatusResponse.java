package com.hunteryavitz.blockchainapi.entities.healthmetric;

import lombok.Data;

/**
 * NodeRegistryRequest is a request object for registering a node.
 */
@Data
public class NodeStatusResponse {

    /**
     * The id of the node.
     */
    private long x;

    /**
     * The port of the node.
     */
    private int y;
}
