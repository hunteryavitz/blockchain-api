package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.services.NodeService;
import jakarta.websocket.server.PathParam;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The node for the node network.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/node")
public class NodeController {

    /**
     * The health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * The node service.
     */
    private final NodeService nodeService;

    /**
     * The node controller constructor.
     * @param healthMetricService the health metric service
     * @param nodeService the node service
     */
    public NodeController(HealthMetricService healthMetricService, NodeService nodeService) {
        this.healthMetricService = healthMetricService;
        this.nodeService = nodeService;
        nodeService.initializeNodeService();
    }

    /**
     * Gets the node status.
     * @param test the test query parameter
     * @return the node status
     */
    @GetMapping("/getNodeStatus")
    public ResponseEntity<NodeStatus> getNodeStatus(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(nodeService.self);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(NodeStatus.UNRESPONSIVE);
    }

    /**
     * Gets the node traffic.
     * @param test the test query parameter
     * @return the node traffic
     */
    @GetMapping("/getNodeTraffic")
    public ResponseEntity<Integer> getNodeTraffic(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(nodeService.traffic);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(-1);
    }
}
