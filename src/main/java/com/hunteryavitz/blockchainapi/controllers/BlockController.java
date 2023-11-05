package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import jakarta.websocket.server.PathParam;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The BlockController class is the controller for the block endpoints.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/block")
public class BlockController {

    /**
     * The blockchainService field is the service for the blockchain.
     */
    private final BlockchainService blockchainService;

    /**
     * The health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * The BlockController constructor is responsible for initializing the controller.
     * @param blockchainService The blockchain service.
     * @param healthMetricService The health metric service.
     */
    public BlockController(BlockchainService blockchainService, HealthMetricService healthMetricService) {
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
     * The addBlockToBlockchain method is responsible for adding a block to the blockchain.
     * @return A ResponseEntity containing a boolean indicating whether the block was added to the blockchain.
     */
    @PostMapping("/addBlockToBlockchain")
    public ResponseEntity<Boolean> addBlockToBlockchain(@PathParam("test") boolean test) {
        try {
            blockchainService.addBlockToBlockchain();
            if (test) {
                throw new Exception("Test exception");
            }

            return ResponseEntity.ok(true);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(false);
    }
}
