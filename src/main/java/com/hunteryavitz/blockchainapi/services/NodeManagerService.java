package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.Node;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeRegistryRequest;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeStatusResponse;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.HashSet;
import java.util.Set;

/**
 * NodeManagerService is responsible for managing the network of nodes.
 */
@Service
public class NodeManagerService {

    /**
     * nodeNetwork is a set of nodes that are currently registered with the network.
     */
    private static Set<Node> nodeNetwork = new HashSet<>();

    /**
     * healthMetricService is a service that is responsible for managing the health of the network.
     */
    private static HealthMetricService healthMetricService;

    /**
     * initializeNodeManagerService is responsible for initializing the NodeManagerService.
     */
    public static void initializeNodeManagerService() {
        if (healthMetricService == null) {
            healthMetricService = new HealthMetricService();
            healthMetricService.createHealthMetricService();
        }
        System.out.println("Initializing Node Manager Service");
    }

    /**
     * registerNode is responsible for registering a node with the network.
     * @param nodeRegistryRequest is the request that contains the node's certificate and port.
     * @return the status of the node's registration.
     */
    public static NodeStatus registerNode(NodeRegistryRequest nodeRegistryRequest) {
        if (authenticateNodeRegistryCertificate(nodeRegistryRequest.getCertificate())) {
            String nodeAddress = assembleNodeAddress(nodeRegistryRequest.getPort());
            Node node = new Node(nodeNetwork.size() + 1, nodeAddress, NodeStatus.ACTIVE, 0);
            nodeNetwork.add(node);
            return NodeStatus.ACTIVE;
        }

        return NodeStatus.FAILED_REGISTRATION;
    }

    /**
     * authenticateNodeRegistryCertificate is responsible for authenticating a node's certificate.
     * @param certificate is the certificate that is being authenticated.
     * @return the status of the certificate's authentication.
     */
    private static boolean authenticateNodeRegistryCertificate(String certificate) {
        if (certificate.equals("secret-sauce")) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * assembleNodeAddress is responsible for assembling a node's address.
     * @param port is the port that the node is running on.
     * @return the node's address.
     */
    private static String assembleNodeAddress(int port) {
        return "http://localhost:" + port + "/api/v1/node/getNodeStatus";
    }

    /**
     * getRollCall is responsible for getting the roll call of the network.
     * @return the status of the roll call.
     */
    public static Boolean getRollCall() {
        try {
            System.out.println("getRollCall");
            Set<Node> nodeNetworkProxy = new HashSet<>();
            for (Node node : nodeNetwork) {
                RestTemplate restTemplate = new RestTemplate();
                NodeStatus nodeStatus = restTemplate.getForEntity("http://localhost:8080/api/v1/node/getNodeStatus", NodeStatus.class).getBody();
                assert nodeStatus != null;
                node.setNodeStatus(nodeStatus);
                nodeNetworkProxy.add(node);
            }
            nodeNetwork = nodeNetworkProxy;
            return Boolean.TRUE;
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return Boolean.FALSE;
    }

    /**
     * getNodeNetworkStatus is responsible for getting the status of the node network.
     * @return the status of the node network.
     */
    public static NodeStatusResponse[] getNodeNetworkStatus() {
        NodeStatusResponse[] nodeStatusResponses = new NodeStatusResponse[nodeNetwork.size()];
        int i = 0;
        for (Node node : nodeNetwork) {
            NodeStatusResponse nodeStatusResponse = new NodeStatusResponse();
            nodeStatusResponse.setX(node.getId());
            nodeStatusResponse.setY(node.getTraffic());
            nodeStatusResponses[i] = nodeStatusResponse;
            i++;
        }
        return nodeStatusResponses;
    }
}
