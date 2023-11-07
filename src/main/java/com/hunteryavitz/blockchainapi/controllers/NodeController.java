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
    public ResponseEntity<String> getNodeStatus(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(NodeService.self.toString());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(NodeStatus.UNRESPONSIVE.toString());
    }
}
