package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.utils.structures.SlidingWindow;
import jakarta.websocket.server.PathParam;
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
    }

    /**
     * Updates the health metric.
     * @param test The test query parameter.
     * @return A ResponseEntity containing a boolean indicating whether the health metric was updated.
     */
    @GetMapping("/updateProductionHealth")
    public ResponseEntity<Boolean> updateProductionHealth(@PathParam("test") boolean test) {
        try {
            healthMetricService.updateBlockchainProduction();
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(true);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(false);
    }

    /**
     * Gets the production health.
     * @param test The test query parameter.
     * @return The production health.
     */
    @GetMapping("/getProductionHealth")
    public ResponseEntity<String> getProductionHealth(@PathParam("test") boolean test) {
        try {
            SlidingWindow slidingWindow = healthMetricService.getProduction();
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(slidingWindow.asJson());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok("");
    }

    /**
     * Gets the production health.
     * @param test The test query parameter.
     * @return The production health.
     */
    @GetMapping("/getExceptionHealth")
    public ResponseEntity<Integer[]> getExceptionHealth(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(HealthMetricService.getHealth());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.INFO, exception);
        }

        return ResponseEntity.ok(new Integer[]{});
    }
}
