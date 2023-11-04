package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.services.NodeService;
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
     * The node controller constructor.
     * @param healthMetricService the health metric service
     */
    public NodeController(HealthMetricService healthMetricService) {
        this.healthMetricService = healthMetricService;
        NodeService.initializeNodeService();
    }

    /**
     * Gets the node status.
     * @return the node status
     */
    @GetMapping("/getNodeStatus")
    public ResponseEntity<NodeStatus> getNodeStatus() {
        try {
            return ResponseEntity.ok(NodeService.self);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.badRequest().build();
    }
}
