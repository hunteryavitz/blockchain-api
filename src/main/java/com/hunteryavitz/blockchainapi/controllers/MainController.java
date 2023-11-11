package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.utils.Utils;
import jakarta.websocket.server.PathParam;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Main controller for the API.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1")
public class MainController {

    /**
     * The blockchain service.
     */
    private final BlockchainService blockchainService;

    /**
     * The health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * Constructor for the Main controller.
     * @param blockchainService The blockchain service.
     * @param healthMetricService The health metric service.
     */
    public MainController(BlockchainService blockchainService, HealthMetricService healthMetricService) {
        this.blockchainService = blockchainService;
        this.healthMetricService = healthMetricService;

        try {
            if (blockchainService.getBlockchain() == null) {
                blockchainService.createInitialBlockchain();
            }
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.CRITICAL, exception);
        }
    }

    /**
     * Returns a 200 response and true to indicate that the API is ready to accept requests.
     * @param test The test query parameter.
     * @return 200 response and true
     */
    @GetMapping("/readiness")
    public ResponseEntity<Boolean> isReady(@PathParam("test") boolean test) {
        try {
            blockchainService.checkReadiness();
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(true);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(false);
    }

    /**
     * Returns a 200 response to indicate that the API is alive.
     * @param test The test query parameter.
     * @return 200 response
     */
    @GetMapping("/liveness")
    public ResponseEntity<Integer> isAlive(@PathParam("test") boolean test) {
        try {
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(blockchainService.isAlive());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.ERROR, exception);
        }

        return ResponseEntity.ok(-1);
    }

    /**
     * Returns the version of the API.
     * @param test The test query parameter.
     * @return 200 response with the version of the API
     */
    @GetMapping("/version")
    public ResponseEntity<String> getVersion(@PathParam("test") boolean test) {
        try {
            String filePath = "VERSION.txt";
            if (test) {
                throw new Exception("Test exception");
            }
            return ResponseEntity.ok(Utils.readFileToString(filePath).trim());
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok("");
    }
}
