package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import lombok.Data;

/**
 * The NodeNetworkStatus class represents the status of the node network.
 */
@Data
public class NodeNetworkStatus {

    /**
     * The node network.
     */
    private Node[] nodeNetwork;

    /**
     * The constructor.
     */
    public NodeNetworkStatus() {
        nodeNetwork = new Node[5];

        for (int i = 0; i < nodeNetwork.length; i++) {
            nodeNetwork[i] = new Node();
            nodeNetwork[i].setId(i);
            nodeNetwork[i].setNodeStatus(Math.random() * 100 > 50 ? NodeStatus.ACTIVE : NodeStatus.INACTIVE);
            nodeNetwork[i].setTraffic((int) (Math.floor(Math.random() * 100)));
        }
    }

    /**
     * The node of the network.
     */
    @Data
    private static class Node {

        /**
         * The id of the node.
         */
        private long id;

        /**
         * The status of the node.
         */
        private NodeStatus nodeStatus;

        /**
         * The traffic of the node.
         */
        private int traffic;
    }
}
