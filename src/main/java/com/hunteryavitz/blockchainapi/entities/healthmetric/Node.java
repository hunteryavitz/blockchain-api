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
         * The address of the node.
         */
        private String address;

        /**
         * The status of the node.
         */
        private NodeStatus nodeStatus;

        /**
         * The traffic of the node.
         */
        private int traffic;

        /**
         * The default constructor.
         */
        public Node() {
                id = -1;
                address = "-1";
                nodeStatus = NodeStatus.INACTIVE;
                traffic = -1;
        }

        /**
         * The overloaded constructor.
         * @param id the id of the node
         * @param address the address of the node
         * @param nodeStatus the status of the node
         * @param traffic the traffic of the node
         */
        public Node(int id, String address, NodeStatus nodeStatus, int traffic) {
                this.id = id;
                this.address = address;
                this.nodeStatus = nodeStatus;
                this.traffic = traffic;
        }
}
