package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import lombok.Data;

/**
 * NodeHealth is the health of a node.
 */
@Data
public class NodeHealth {

    /**
     * The node id.
     */
    private long nodeId;

    /**
     * The node status.
     */
    private NodeStatus nodeStatus;

    /**
     * The node traffic.
     */
    private int nodeTraffic;
}
