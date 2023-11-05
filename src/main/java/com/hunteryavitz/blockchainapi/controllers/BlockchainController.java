package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.constants.ContaminationLevel;
import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import com.hunteryavitz.blockchainapi.services.HealthMetricService;
import jakarta.websocket.server.PathParam;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * The BlockchainController class is the controller for the blockchain endpoints.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/blockchain")
public class BlockchainController {

    /**
     * The blockchainService is the service for the blockchain endpoints.
     */
    private final BlockchainService blockchainService;

    /**
     * The health metric service.
     */
    private final HealthMetricService healthMetricService;

    /**
     * The constructor for the BlockchainController class.
     * @param blockchainService The service for the blockchain endpoints.
     * @param healthMetricService The health metric service.
     */
    public BlockchainController(BlockchainService blockchainService, HealthMetricService healthMetricService) {
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
     * The addBlockToBlockchain method is the endpoint for adding a block to the blockchain.
     * @return A ResponseEntity containing a boolean indicating whether the block was added to the blockchain.
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

    /**
     * The getBlockchain method is the endpoint for getting the blockchain.
     * @return A ResponseEntity containing the blockchain.
     */
    @GetMapping("/getBlockchain")
    public ResponseEntity<Block[]> getBlockchain(@PathParam("test") boolean test) {
        try {
            Block[] blockchain = blockchainService.getBlockchain();
            if (test) {
                throw new Exception("Empty blockchain");
            }

            return ResponseEntity.ok(blockchain);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(new Block[0]);
    }

    /**
     * The getBlockById method is the endpoint for getting a block by its id.
     * @param id The id of the block.
     * @return A ResponseEntity containing the block.
     */
    @GetMapping("/getBlockById")
    public ResponseEntity<Block> getBlockById(@RequestParam int id, @PathParam("test") boolean test) {
        try {
            Block block = blockchainService.getBlockById(id);
            if (test) {
                throw new Exception("Empty block");
            }

            return ResponseEntity.ok(block);
        } catch (Exception exception) {
            healthMetricService.updateHealth(ContaminationLevel.WARNING, exception);
        }

        return ResponseEntity.ok(new Block());
    }
}
