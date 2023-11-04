package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.Node;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeRegistryRequest;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.services.NodeManagerService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Set;

/**
 * NodeManagerController is the controller for the NodeManagerService.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/nodeManager")
public class NodeManagerController {

    /**
     * The Health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * Instantiates a new Node manager controller.
     * @param healthMetricService the health metric service
     */
    public NodeManagerController( HealthMetricService healthMetricService) {
        this.healthMetricService = healthMetricService;
        NodeManagerService.initializeNodeManagerService();
    }

    /**
     * Register node response entity.
     * @param nodeRegistryRequest the node registry request
     * @return the node status
     */
    @PostMapping("/registerNode")
    public ResponseEntity<NodeStatus> registerNode(@RequestBody NodeRegistryRequest nodeRegistryRequest) {
        try {
            return ResponseEntity.ok(NodeManagerService.registerNode(nodeRegistryRequest));
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.badRequest().build();
    }

    /**
     * Gets node network status.
     * @return the node network status
     */
    @GetMapping("/getNodeNetworkStatus")
    public ResponseEntity<Set<Node>> getNodeNetworkStatus() {
        try {
            return ResponseEntity.ok(NodeManagerService.nodeNetwork);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.badRequest().build();
    }

    /**
     * Gets the node network status.
     * @return Boolean indicating whether the node network status was updated.
     * Call this from a cronjob every x/time period.
     */
    @GetMapping("/nodeNetworkRollCall")
    public ResponseEntity<Boolean> nodeNetworkRollCall() {
        try {
            return ResponseEntity.ok(NodeManagerService.getRollCall());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(Boolean.FALSE);
    }
}
