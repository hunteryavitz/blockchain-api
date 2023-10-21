package com.hunteryavitz.blockchainapi.controllers;

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
//    @Autowired
    HealthMetricService healthMetricService;

    /**
     * The constructor for the health metric controller.
     * @param healthMetricService The health metric service.
     */
    public HealthMetricController(HealthMetricService healthMetricService) {
        this.healthMetricService = healthMetricService;
    }

    /**
     * Updates the health metric.
     * @return A ResponseEntity containing a boolean indicating whether the health metric was updated.
     */
    @GetMapping("/updateProduction")
    public ResponseEntity<Boolean> updateProduction() {
        healthMetricService.updateBlockchainProduction();
        return ResponseEntity.ok(true);
    }

    /**
     * Gets the production health.
     * @return The production health.
     */
    @GetMapping("/getProductionHealth")
    public ResponseEntity<String> addBlockToBlockchain() {
        SlidingWindow slidingWindow = healthMetricService.getProduction();
        return ResponseEntity.ok(slidingWindow.asJson());
    }
}
