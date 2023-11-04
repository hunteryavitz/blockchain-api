package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import com.hunteryavitz.blockchainapi.utils.Utils;
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
            if (healthMetricService.getProduction() == null) {
                healthMetricService.createHealthMetricService();
            }
        } catch (Exception exception) {
            assert healthMetricService != null;
            healthMetricService.updateHealth(ContaminationLevel.CRITICAL, exception);
        }
    }

    /**
     * Returns a 200 response to indicate that the API is ready to accept requests.
     * @return 200 response
     */
    @GetMapping("/readiness")
    public ResponseEntity<Boolean> isReady() {

        // application is running
        // service passes checks
        // blockchain is valid
        // files
        // database
        // web services

        try {
            blockchainService.checkReadiness();
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
            return ResponseEntity.ok(false);
        }

        return ResponseEntity.ok(true);
    }

    /**
     * Returns a 200 response to indicate that the API is alive.
     * @return 200 response
     */
    @GetMapping("/liveness")
    public ResponseEntity<Integer> isAlive() {
        int isAlive;

        try {
            isAlive = blockchainService.isAlive();
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.ERROR, exception);
            return ResponseEntity.ok(-1);
        }

        return ResponseEntity.ok(isAlive);
    }

    /**
     * Returns the version of the API.
     * @return 200 response with the version of the API
     */
    @GetMapping("/version")
    public ResponseEntity<String> getVersion() {
        String version;

        try {
            String filePath = "VERSION.txt";
            version = Utils.readFileToString(filePath).trim();
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
            return ResponseEntity.ok(null);
        }

        return ResponseEntity.ok(version);
    }

    /**
     * Returns the blockchain.
     * @return 200 response with the blockchain
     */
    @GetMapping("/verifyBlockchain")
    public ResponseEntity<Boolean> verifyBlockchain() {
        try {
            if (blockchainService.verifyBlockchain()) {
                return ResponseEntity.ok(true);
            }
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.CRITICAL, exception);
        }

        return ResponseEntity.ok(false);
    }
}
