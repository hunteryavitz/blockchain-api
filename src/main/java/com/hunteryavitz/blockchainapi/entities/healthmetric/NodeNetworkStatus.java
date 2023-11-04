package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import lombok.Data;

import java.util.HashSet;
import java.util.Set;

/**
 * The NodeNetworkStatus class represents the status of the node network.
 */
@Data
public class NodeNetworkStatus {

    /**
     * The node network.
     */
    private Set<Node> nodeNetwork;

    /**
     * The constructor.
     */
    public NodeNetworkStatus() {
        nodeNetwork = new HashSet<>();
        nodeNetwork.add(new Node(1, NodeStatus.ACTIVE, 10));
        nodeNetwork.add(new Node(2, NodeStatus.ACTIVE, 40));
        nodeNetwork.add(new Node(3, NodeStatus.INACTIVE, 0));
    }
}
