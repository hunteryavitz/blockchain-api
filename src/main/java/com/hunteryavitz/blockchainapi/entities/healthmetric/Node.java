package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import lombok.Data;

/**
 * The node of the network.
 */
@Data
public class Node {

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

        /**
         * The constructor.
         */
        public Node(int id, NodeStatus nodeStatus, int traffic) {
                this.id = id;
                this.nodeStatus = nodeStatus;
                this.traffic = traffic;
        }
}
