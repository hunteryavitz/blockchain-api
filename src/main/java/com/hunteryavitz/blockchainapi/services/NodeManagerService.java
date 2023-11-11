package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
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
    }

    /**
     * registerNode is responsible for registering a node with the network.
     * @param nodeRegistryRequest is the request that contains the node's certificate and port.
     * @return the status of the node's registration.
     */
    public static NodeStatus registerNode(NodeRegistryRequest nodeRegistryRequest) {
        if (authenticateNodeRegistryCertificate(nodeRegistryRequest.getCertificate())) {
            String nodeAddressStatus = assembleNodeAddressStatus(nodeRegistryRequest.getPort());
            String nodeAddressTraffic = assembleNodeAddressTraffic(nodeRegistryRequest.getPort());
            Node node = new Node(nodeNetwork.size() + 1, nodeAddressStatus, nodeAddressTraffic, NodeStatus.ACTIVE, 0);
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
    private static String assembleNodeAddressStatus(int port) {
        return "http://localhost:" + port + "/api/v1/node/getNodeStatus";
    }

    /**
     * assembleNodeAddress is responsible for assembling a node's address.
     * @param port is the port that the node is running on.
     * @return the node's address.
     */
    private static String assembleNodeAddressTraffic(int port) {
        return "http://localhost:" + port + "/api/v1/node/getNodeTraffic";
    }

    /**
     * getRollCall is responsible for getting the roll call of the network.
     * @return the status of the roll call.
     */
    public static Boolean getRollCall() {
        try {
            Set<Node> nodeNetworkProxy = new HashSet<>();

            for (Node node : nodeNetwork) {
                RestTemplate restTemplate = new RestTemplate();
                NodeStatus nodeStatus = restTemplate.getForEntity(node.getAddressGetStatus(), NodeStatus.class).getBody();
                Integer traffic = restTemplate.getForEntity(node.getAddressGetTraffic(), Integer.class).getBody();
                assert nodeStatus != null;
                assert traffic != null;
                node.setNodeStatus(nodeStatus);
                node.setTraffic(traffic);
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

    /**
     * getNodeNetworkHealth is responsible for getting the health of the node network.
     * @return the health of the node network.
     */
    public static NodeHealthResponse getNodeNetworkHealth() {
        NodeHealthResponse nodeHealthResponse = new NodeHealthResponse();
        nodeHealthResponse.setNodeHealthResponse(new ArrayList<>());

        for (Node node : nodeNetwork) {
            NodeHealth nodeHealth = new NodeHealth();
            nodeHealth.setNodeId(node.getId());
            nodeHealth.setNodeStatus(node.getNodeStatus());
            nodeHealth.setNodeTraffic(node.getTraffic());
            nodeHealthResponse.addNodeHealth(nodeHealth);
        }

        return nodeHealthResponse;
    }
}
