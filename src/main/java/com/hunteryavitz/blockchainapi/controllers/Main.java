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
public class Main {

    /**
     * The blockchain service.
     */
    private final BlockchainService blockchainService;

    /**
     * Constructor for the Main controller.
     * @param blockchainService The blockchain service.
     */
    public Main(BlockchainService blockchainService) {
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
        return ResponseEntity.ok(true);
    }

    /**
     * Returns a 200 response to indicate that the API is alive.
     * @return 200 response
     */
    @GetMapping("/liveness")
    public ResponseEntity<Boolean> isAlive() {
        return ResponseEntity.ok(true);
    }

    /**
     * Returns the version of the API.
     * @return 200 response with the version of the API
     */
    @GetMapping("/version")
    public ResponseEntity<String> getVersion() {
        return ResponseEntity.ok("0.0.12");
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
