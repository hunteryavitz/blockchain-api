package com.hunteryavitz.blockchainapi.entities.healthmetric;

import lombok.Data;

/**
 * NodeRegistryRequest is a request object for registering a node.
 */
@Data
public class NodeRegistryRequest {

    /**
     * The port of the node.
     */
    private int port;

    /**
     * The certificate of the node.
     */
    private String certificate;
}
