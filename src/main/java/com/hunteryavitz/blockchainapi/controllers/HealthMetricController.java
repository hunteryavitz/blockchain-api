package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.entities.healthmetric.NodeNetworkStatus;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.utils.structures.SlidingWindow;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The health metric controller.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/healthMetric")
public class HealthMetricController {

    /**
     * The health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * The blockchain service.
     * @param healthMetricService The health metric service.
     */
    public HealthMetricController(HealthMetricService healthMetricService) {
        this.healthMetricService = healthMetricService;

        try {
            if (healthMetricService.getProduction() == null) {
                healthMetricService.createHealthMetricService();
            }
        } catch (Exception exception) {
            assert healthMetricService != null;
            healthMetricService.updateHealth(ContaminationLevel.CRITICAL, exception);
        }
    }

    /**
     * Updates the health metric.
     * @return A ResponseEntity containing a boolean indicating whether the health metric was updated.
     */
    @GetMapping("/updateProduction")
    public ResponseEntity<Boolean> updateProduction() {
        try {
            healthMetricService.updateBlockchainProduction();
            return ResponseEntity.ok(true);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(false);
    }

    /**
     * Gets the production health.
     * @return The production health.
     */
    @GetMapping("/getProductionHealth")
    public ResponseEntity<String> getProductionHealth() {
        try {
            SlidingWindow slidingWindow = healthMetricService.getProduction();
            return ResponseEntity.ok(slidingWindow.asJson());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok("");
    }

    /**
     * Gets the production health.
     * @return The production health.
     */
    @GetMapping("/health")
    public ResponseEntity<Integer[]> getHealth() {
        try {
            return ResponseEntity.ok(HealthMetricService.getHealth());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(new Integer[]{});
    }

    /**
     * Gets the node network status.
     * @return The node network status.
     */
    @GetMapping("/getNodeNetworkStatus")
    public ResponseEntity<NodeNetworkStatus> getNodeNetworkStatus() {
        try {
            return ResponseEntity.ok(new NodeNetworkStatus());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(new NodeNetworkStatus());
    }
}
