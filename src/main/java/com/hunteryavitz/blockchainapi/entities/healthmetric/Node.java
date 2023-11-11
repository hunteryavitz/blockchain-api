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
        private String addressGetStatus;

        /**
         * The address of the node.
         */
        private String addressGetTraffic;

        /**
         * The status of the node.
         */
        private NodeStatus nodeStatus;

        /**
         * The traffic of the node.
         */
        private int traffic;

        /**
         * The overloaded constructor.
         * @param id the id of the node
         * @param addressGetStatus the address of the node
         * @param addressGetTraffic the address of the node
         * @param nodeStatus the status of the node
         * @param traffic the traffic of the node
         */
        public Node(int id, String addressGetStatus, String addressGetTraffic, NodeStatus nodeStatus, int traffic) {
                this.id = id;
                this.addressGetStatus = addressGetStatus;
                this.addressGetTraffic = addressGetTraffic;
                this.nodeStatus = nodeStatus;
                this.traffic = traffic;
        }
}
