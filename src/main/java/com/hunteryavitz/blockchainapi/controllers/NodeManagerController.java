package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.constants.NodeStatus;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeRegistryRequest;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeStatusResponse;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.services.NodeManagerService;
import jakarta.websocket.server.PathParam;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
     * @param test the test query parameter
     * @return the node status
     */
    @PostMapping("/registerNode")
    public ResponseEntity<NodeStatus> registerNode(@RequestBody NodeRegistryRequest nodeRegistryRequest,
                                                   @PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("Test exception");
            }
            return ResponseEntity.ok(NodeManagerService.registerNode(nodeRegistryRequest));
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }
        return ResponseEntity.ok(NodeStatus.FAILED_REGISTRATION);
    }

    /**
     * Gets node network status.
     * @param test test query param
     * @return the node network status
     */
    @GetMapping("/getNodeNetworkStatus")
    public ResponseEntity<NodeStatusResponse[]> getNodeNetworkStatus(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("test exception");
            }
            return ResponseEntity.ok(NodeManagerService.getNodeNetworkStatus());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(new NodeStatusResponse[]{});
    }

    /**
     * Gets the node network status.
     * @param test test query param
     * @return Boolean indicating whether the node network status was updated.
     * Call this from a cronjob every x/time period.
     */
    @GetMapping("/nodeNetworkRollCall")
    public ResponseEntity<Boolean> nodeNetworkRollCall(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("test exception");
            }
            return ResponseEntity.ok(NodeManagerService.getRollCall());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(Boolean.FALSE);
    }
}
