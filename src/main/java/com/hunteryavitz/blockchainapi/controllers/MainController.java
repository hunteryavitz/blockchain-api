package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.services.BlockchainService;
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
     * Constructor for the Main controller.
     * @param blockchainService The blockchain service.
     */
    public MainController(BlockchainService blockchainService) {
        this.blockchainService = blockchainService;
        if (blockchainService.getBlockchain() == null) {
            blockchainService.createInitialBlockchain();
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

        blockchainService.checkReadiness();
        return ResponseEntity.ok(true);
    }

    /**
     * Returns a 200 response to indicate that the API is alive.
     * @return 200 response
     */
    @GetMapping("/liveness")
    public ResponseEntity<Integer> isAlive() {
        return ResponseEntity.ok(blockchainService.isAlive());
    }

    /**
     * Returns the health metrics of the blockchain.
     * @return 200 response with the health metrics of the blockchain
     */
    @GetMapping("/health")
    public ResponseEntity<Integer[]> isHealthy() {
        Integer[] healthMetrics = blockchainService.getHealthMetrics();
        return ResponseEntity.ok(healthMetrics);
    }

    /**
     * Returns the version of the API.
     * @return 200 response with the version of the API
     */
    @GetMapping("/version")
    public ResponseEntity<String> getVersion() {
        return ResponseEntity.ok("0.0.15");
    }

    /**
     * Returns the blockchain.
     * @return 200 response with the blockchain
     */
    @GetMapping("/verifyBlockchain")
    public ResponseEntity<Boolean> verifyBlockchain() {
        if (blockchainService.verifyBlockchain()) {
            return ResponseEntity.ok(true);
        }
        return ResponseEntity.ok(false);
    }
}
